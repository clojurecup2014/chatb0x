(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string parse-string]]
            [cemerick.friend :as friend]
            [chatb0x.data :as data]
            [digest :refer [md5]]
            [clojure.string :as str]))

(def my-req (atom nil))

(defn agent-is-authorized [req] (friend/authorized? #{:chatb0x.user/agent} (friend/identity req)))
;; BRADS FUNCTIONS FOR DATA
;; get-assigned-agents, get-unassigned-agents, get-free-agents 
;; Get brad to make get-free-agent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This gets the visitor from the agent message header
(defn get-visitor-from-msg [data]     (:visitor (read-string data)))
(defn message-is-chat [data]          (:message (read-string data)))
(defn message-is-agent-connect [data] (data/ip-to-chnl (:agent-join (read-string data))))
(defn get-text [data]                 (:message (read-string data)))
(defn format-vis-join [visitor]       (generate-string {:visitor-join (chnl-to-ip visitor)}))
(defn from-msg-is-client-an-agent [req] (get-in req [:session :cemerick.friend/identity :authentications nil :username]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

;; Vistior either comes from msg, is the sender, or there is no visitor
(defn get-visitor-from-msg-or-if-client-is-visitor [ch-sender data]
  (let [vis-from-msg  (get-visitor-from-msg data)
        sender-to-vis (if (data/visitor-is-in-map ch-sender) ch-sender false)]
    (if vis-from-msg
      vis-from-msg
      sender-to-vis)))

(defn connect-visitor-to-agent [visitor]
  (let [agent (data/get-free-agent)]
    (when agent
      (let [msg (format-vis-join visitor)]
        (println "sending serv->agent" msg "to" agent)  
        (send! agent msg false)))))

(defn connect-unconnected-visitors-to-agents []
  (doseq [visitor @data/visitors]
    (let [visitor-channel     (first visitor)
          visitor-unconnected (not (second visitor))]
      (when visitor-unconnected (connect-visitor-to-agent visitor-channel)))))

(defn connect-agent-to-visitor [agent data]
  (let [visitor (message-is-agent-connect data)]
    (data/agents-add-visitor agent visitor)
    (data/visitors-add       visitor agent)))

(defn get-agent-from-client [client]
  (let [agent1 (@data/visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (data/is-agent client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling

(defn msg-text [sender data]
  (let [agent   (data/get-agent   sender)
        visitor (get-visitor-from-msg-or-if-client-is-visitor sender data)
        text    (get-text data)
        gravatar-url (:gravatar-url (get-in @data/clients [sender]))
        msg     (generate-string {:ch-visitor (:ch-visitor data) :message text :gravatar-url gravatar-url})]
    (println "msg-text: \n\tsender: " sender "\n\tdata: " data "\n\tgrav: " gravatar-url)
    (when text
      (when agent
        (println "sending serv->agent:" msg "to" agent)
        (send! agent msg false)
        (when visitor
          (println "sending serv->visitor" msg "to" visitor)
          (send! visitor msg false))))))

(defn msg-close [client]
  (let [agent   (data/get-agent   client)
        text    "chat was terminated!"]
    (if (= client agent)
      (do (println "websockets: agent closed, send closed message to all visitors" (data/get-agent-visitors agent))
          (doseq [visitor (data/get-agent-visitors agent)]
            (println "Sending msg to visitor " visitor)
            (send! visitor (generate-string {:agent agent :visitor visitor :message text}) false)))
      (do (println "websockets: visitor closed, send closed message to the agent" client agent)
          (if agent (send! agent (generate-string {:agent agent :visitor client :message text}) false))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in data/clients.
(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (if (from-msg-is-client-an-agent req)
      (when (agent-is-authorized req)
        (do (println "ws-connected:" "\n\t agent-channel" (:async-channel req)) ;; TODO: Connect all unconnected visitors to agent(s)
            (data/add-new-agent req channel)
            (connect-unconnected-visitors-to-agents)))
      (do (println "ws-connected:" "\n\t visitor-channel" (:async-channel req))
          (data/add-new-visitor req channel)
          (connect-visitor-to-agent channel)))
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (do (println "ws-receive:" "\n\t channel"  channel "\n\t data"  data)
                              (when (message-is-agent-connect data) (connect-agent-to-visitor channel data))
                              (when (message-is-chat data)          (msg-text channel data)))))
    ;; CLOSE
    (on-close channel   (fn [status]
                          (do (println channel "ws-close:" "\n\t status"  status)
                              (msg-close channel)
                              (data/remove-client channel))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commented code

;;;;;;;; BUGS
;;;; 1) If visitor drops before agent trys to join, then there will be an error.
;;;; 2) Null type channel gets put into agent. Possibly happens when removing all visitors. See debug output below.
;;       * See in below: Agents DS:    {#<AsyncChannel 0.0.0.0/0.0.0.0:8080<->null> {}}
;; ws-connected: 
;; 	 agent-channel #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942>
;; ***calc-gravatar: 
;; 	req:  {:cookies {ring-session {:value d7be478e-bbfa-4136-9bcd-9f7a54a9829d}}, :remote-addr 127.0.0.1, :params {}, :route-params {}, :headers {origin http://localhost:8080, host localhost:8080, upgrade websocket, user-agent Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:32.0) Gecko/20100101 Firefox/32.0, cookie ring-session=d7be478e-bbfa-4136-9bcd-9f7a54a9829d, connection keep-alive, Upgrade, pragma no-cache, sec-websocket-key d/cEIk+RUp2yYhGOp9pmVg==, accept text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8, accept-language en-US,en;q=0.5, sec-websocket-version 13, accept-encoding gzip, deflate, cache-control no-cache}, :async-channel #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942>, :server-port 8080, :content-length 0, :form-params {}, :websocket? true, :session/key d7be478e-bbfa-4136-9bcd-9f7a54a9829d, :query-params {}, :content-type nil, :character-encoding utf8, :uri /chatb0x/ws, :server-name localhost, :query-string nil, :body nil, :scheme :http, :cemerick.friend/auth-config {:allow-anon? true, :default-landing-uri /agent-chat, :login-uri /login, :credential-fn #<core$fn__11518 chatb0x.core$fn__11518@4c21986a>, :workflows [#<workflows$interactive_form$fn__5173 cemerick.friend.workflows$interactive_form$fn__5173@5ac81e10>]}, :request-method :get, :session {:cemerick.friend/identity {:current nil, :authentications {nil {:in-chat false, :sites #{}, :roles #{:chatb0x.user/agent}, :password $2a$10$e1goq5kHiR9xauiifbFP1ehNKXZsaJ4M2oNloWjfMrZ0qYtfLF6GO, :username nickgeoca@gmail.com}}}}} 
;; 	email:  nickgeoca@gmail.com
;; Clients DS:   {#<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942> {:name nil, :gravatar-url http://www.gravatar.com/avatar/c1af707c54ce0e6f740ded0f7d906dc9, :room nil}}
;; Agents DS:    {#<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942> {}}
;; Visitors DS:  {}
;; ws-receive: 
;; 	 channel #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942> 
;; 	 data {:initial-path "/welcome"}
;; #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55942> ws-close: 
;; 	 status :going-away
;; websockets: agent closed, send closed message to all visitors {}
;; Clients DS:   {}
;; Agents DS:    {#<AsyncChannel 0.0.0.0/0.0.0.0:8080<->null> {}}
;; Visitors DS:  {}
;; ws-connected: 
;; 	 agent-channel #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55947>
;; ***calc-gravatar: 
;; 	req:  {:cookies {ring-session {:value d7be478e-bbfa-4136-9bcd-9f7a54a9829d}}, :remote-addr 127.0.0.1, :params {}, :route-params {}, :headers {origin http://localhost:8080, host localhost:8080, upgrade websocket, user-agent Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:32.0) Gecko/20100101 Firefox/32.0, cookie ring-session=d7be478e-bbfa-4136-9bcd-9f7a54a9829d, connection keep-alive, Upgrade, pragma no-cache, sec-websocket-key MAH61jW77989s5+bt7omuw==, accept text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8, accept-language en-US,en;q=0.5, sec-websocket-version 13, accept-encoding gzip, deflate, cache-control no-cache}, :async-channel #<AsyncChannel /127.0.0.1:8080<->/127.0.0.1:55947>, :server-port 8080, :content-length 0, :form-params {}, :websocket? true, :session/key d7be478e-bbfa-4136-9bcd-9f7a54a9829d, :query-params {}, :content-type nil, :character-encoding utf8, :uri /chatb0x/ws, :server-name localhost, :query-string nil, :body nil, :scheme :http, :cemerick.friend/auth-config {:allow-anon? true, :default-landing-uri /agent-chat, :login-uri /login, :credential-fn #<core$fn__11518 chatb0x.core$fn__11518@4c21986a>, :workflows [#<workflows$interactive_form$fn__5173 cemerick.friend.workflows$interactive_form$fn__5173@5ac81e10>]}, :request-method :get, :session {:cemerick.friend/identity {:current nil, :authentications {nil {:in-chat false, :sites #{}, :roles #{:chatb0x.user/agent}, :password $2a$10$e1goq5kHiR9xauiifbFP1ehNKXZsaJ4M2oNloWjfMrZ0qYtfLF6GO, :username nickgeoca@gmail.com}}}}} 
;; 	email:  nickgeoca@gmail.com
