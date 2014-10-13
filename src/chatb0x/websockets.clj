(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string parse-string]]
            [cemerick.friend :as friend]
            [digest :refer [md5]]
            [clojure.string :as str]))

(def my-req (atom nil))

(defn calc-gravatar [req]
  (let [email (get-in req [:session :cemerick.friend/identity :authentications nil :username])]
    (println "***calc-gravatar: \n\treq: " req "\n\temail: " email)
    (reset! my-req req)
    (if email
      (str "http://www.gravatar.com/avatar/"
           (-> email
               (str/trim)
               (str/lower-case)
               (md5)))
      (str "http://www.gravatar.com/avatar/"))))

(defn agent-is-authorized [req] (friend/authorized? #{:chatb0x.user/agent} (friend/identity req)))
;; BRADS FUNCTIONS FOR DATA
;; get-assigned-agents, get-unassigned-agents, get-free-agents 
;; Get brad to make get-free-agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IP/CHNL Conversion
(def channel-ip-map (atom {}))

(defn ip-to-chnl [ip] (get @channel-ip-map ip))

(defn chnl-to-ip [chnl]
  (let [str (re-find (re-matcher #"\>\/\d+\.\d+\.\d+\.\d+\:\d+" (generate-string (pr-str chnl))))]
    (if str
      (subs str 2)
      nil)))

(defn add-chnl [chnl]
  (let [ip (chnl-to-ip chnl)]
    (swap! channel-ip-map assoc ip chnl)))

(defn remove-chnl [chnl]
  (let [ip (chnl-to-ip chnl)]
    (swap! channel-ip-map dissoc ip)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This gets the visitor from the agent message header
(defn get-visitor-from-msg [data]     (:visitor (read-string data)))
(defn message-is-chat [data]          (:message (read-string data)))
(defn message-is-agent-connect [data] (ip-to-chnl (:agent-join (read-string data))))
(defn get-text [data]                 (:message (read-string data)))
(defn format-vis-join [visitor]       (generate-string {:visitor-join (chnl-to-ip visitor)}))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
(def ds-clients (atom {}))  ;; Key: channel;       data: email, name, etc
(def ds-agents (atom {}))   ;; Key: agent-channel; data: {vis1-chnl, vis2-chnl, ...}
(def ds-visitors (atom {})) ;; Key: visitor-chnl;  data: agent-chnl
(defn print-ds [] (println "\nagents" @ds-agents "\nvisitors" @ds-visitors "\nclients" @ds-clients "\nchannel-ip-map" @channel-ip-map))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn get-free-agent [] (first (keys @ds-agents))) ;; TOOD: make rand-nth?

(defn is-agent [client] (@ds-agents client))

;; Return nil if unknown
(defn get-agent [client]
  (let [agent1 (@ds-visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (is-agent client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))

;; Complicated, but vistior either comes from msg, is the sender, or there is no visitor
(defn get-visitor [sender data]
  (let [vis-from-msg  (get-visitor-from-msg data)
        sender-to-vis (if  (get @ds-visitors sender false) sender false)]
    (if vis-from-msg
      vis-from-msg
      sender-to-vis)))
(defn get-agent-visitors [client]
  (get @ds-agents client nil))

(defn ds-agents-add-visitor [agent visitor]
  (if (= visitor {})
    (swap! ds-agents assoc agent {})
    (swap! ds-agents assoc-in [agent visitor] visitor)))

(defn ds-visitors-add [visitor agent] (swap! ds-visitors assoc visitor agent))

(defn remove-agent [client]
  (let [visitors (get @ds-agents client)]
    (swap! ds-clients dissoc client)       ;; Cleanup: remove agent from ds-clients
    (doseq [visitor visitors]  
      (swap! ds-clients dissoc visitor)    ;; Cleanup: remove visitors from ds-clients
      (swap! ds-visitors dissoc visitor)) ;; Cleanup: ds-visitors ;; MAY NEED CHANNEL OUT OF THE MAP THAT WE GET
    (swap! ds-agents dissoc agent)))

(defn remove-visitor [client]
  (remove-chnl client)
  (swap! ds-clients dissoc client)       ;; Cleanup: remove agent from ds-clients
  (if (get ds-visitors client)
    (swap! ds-agents update-in [(get-agent client)] dissoc client))
  (swap! ds-visitors dissoc client))    ;; Cleanup: remove agent from ds-visitors

(defn close-cleanup-ds [client] ;; TODO: simplify by making functions innocuous. (do (remove-agent) (remove-visitor))
  (if (is-agent client)
    (remove-agent   client)
    (remove-visitor client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; New Code

;;(defn is-client-a-visitor)
(defn is-client-an-agent [req]
  (let [value (get-in req [:session :cemerick.friend/identity :authentications nil :username])]
    value))

(defn add-new-client [req channel]
                   (swap! ds-clients assoc channel
                          {:name nil
                           :gravatar-url (calc-gravatar req)
                           :room nil}))

(def my-visitor (atom nil)) ;;DEBUG
(defn connect-visitor-to-agent [visitor]
  (let [agent (get-free-agent)]
    (when agent
      (let [msg (format-vis-join visitor)]
        (println "sending serv->agent" msg "to" agent)  
        (send! agent msg false)))))

(defn connect-unconnected-visitors-to-agents []
  (doseq [visitor @ds-visitors]
    (let [visitor-channel     (first visitor)
          visitor-unconnected (not (second visitor))]
      (when visitor-unconnected (connect-visitor-to-agent visitor-channel)))))

(defn connect-agent-to-visitor [agent data]
  (let [visitor (message-is-agent-connect data)]
    (ds-agents-add-visitor agent visitor)
    (ds-visitors-add       visitor agent)))

(defn get-agent-from-client [client]
  (let [agent1 (@ds-visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (is-agent client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling
(defn msg-text [sender data]
  (let [agent   (get-agent   sender)
        visitor (get-visitor sender data)
        text    (get-text data)
        gravatar-url (:gravatar-url (get-in @ds-clients [sender]))
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
  (let [agent   (get-agent   client)
        text    "chat was terminated!"]
    (if (= client agent)
      (do (println "websockets: agent closed, send closed message to all visitors" (get-agent-visitors agent))
          (doseq [visitor (get-agent-visitors agent)]
            (println "Sending msg to visitor " visitor)
            (send! visitor (generate-string {:agent agent :visitor visitor :message text}) false)))
      (do (println "websockets: visitor closed, send closed message to the agent" client agent)
          (if agent (send! agent (generate-string {:agent agent :visitor client :message text}) false))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in ds-clients.
(defn add-new-agent [req channel]
  (do (add-new-client req channel)
      (ds-agents-add-visitor channel {})))
(defn add-new-visitor [req channel]
  (do (add-new-client req channel)
      (ds-visitors-add channel nil)
      (add-chnl channel)))
(defn debug-print-data-structures []
  (comment (do (println "Clients DS:  " @ds-clients)
               (println "Agents DS:   " @ds-agents)
               (println "Visitors DS: " @ds-visitors))))
;;(def list-unconnected-clients (atom (list)))

(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (debug-print-data-structures)
    (if (is-client-an-agent req)
      (when (agent-is-authorized req)
        (do (println "ws-connected:" "\n\t agent-channel" (:async-channel req)) ;; TODO: Connect all unconnected visitors to agent(s)
            (add-new-agent req channel)
            (connect-unconnected-visitors-to-agents)))
      (do (println "ws-connected:" "\n\t visitor-channel" (:async-channel req))
          (reset! my-visitor channel) ;;DEBUG
          (add-new-visitor req channel)
          (connect-visitor-to-agent channel)))
    (debug-print-data-structures)
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (do (println "ws-receive:" "\n\t channel"  channel "\n\t data"  data)
                              (when (message-is-agent-connect data) (connect-agent-to-visitor channel data))
                              (when (message-is-chat data)          (msg-text channel data)))))
    ;; CLOSE
    (on-close channel   (fn [status]
                          (do (println channel "ws-close:" "\n\t status"  status)
                              (msg-close channel)
                              (close-cleanup-ds channel))))))


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
