(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string parse-string]]
            [cemerick.friend :as friend]
            [chatb0x.data :as data]
            [digest :refer [md5]]
            [clojure.string :as str]))

;; REMOVE
;; (def my-req (atom nil))
(defn calc-gravatar [req]
  (let [email (get-in req [:session :cemerick.friend/identity :authentications nil :username])]
    (println "***calc-gravatar: \n\treq: " req "\n\temail: " email)
;;    (reset! my-req req)
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
        sender-to-vis (if (data/visitor-in-db ch-sender) ch-sender false)]
    (if vis-from-msg
      vis-from-msg
      sender-to-vis)))

(defn connect-visitor-to-agent [visitor]
  (let [agent (data/db-get-free-agent)]
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
    (data/db-add-agent   agent visitor)
    (data/db-add-visitor visitor agent)))

(defn get-agent-from-client [client]
  (let [agent1 (@data/visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (data/agent-in-db client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling

(defn msg-text [sender data]
  (let [agent   (data/db-get-agent-with-any-chnl   sender)
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
  (let [agent   (data/db-get-agent-with-any-chnl  client)
        text    "chat was terminated!"]
    (if (= client agent)
      (do (println "websockets: agent closed, send closed message to all visitors" (data/db-get-agent-visitors agent))
          (doseq [visitor (data/db-get-agent-visitors agent)]
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
            (data/add-agent req channel)
            (connect-unconnected-visitors-to-agents)))
      (do (println "ws-connected:" "\n\t visitor-channel" (:async-channel req))
          (data/add-visitor req channel)
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
                              (cond
                               (data/agent-in-db client)   (data/delete-agent  client)
                               (data/visitor-in-db client) (data/delete-visitor client)))))))
