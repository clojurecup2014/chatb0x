(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string]]))

;; BRADS FUNCTIONS FOR DATA
;; get-assigned-agents, get-unassigned-agents, get-free-agents 
;; Get brad to make get-free-agent
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
(def ds-clients (atom {}))  ;; Key: channel;       data: email, name, etc
(def ds-agents (atom {}))   ;; Key: agent-channel; data: {vis1-chnl, vis2-chnl, ...}
(def ds-visitors (atom {})) ;; Key: visitor-chnl;  data: agent-chnl

(defn get-free-agent [] (first (keys @ds-agents)))
(defn is-agent [client] (@ds-agents client))

;; Return nil if unknown
(defn get-agent [client]
  (let [agent1 (@ds-visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (is-agent client) client nil)] ;; Client is agent or not
    (println "websockets: get-agent")
    (or agent1 agent2)))
(defn get-agent-visitors [client]
  (get @ds-agents client nil))

;; This gets the visitor from the agent message header
;; Return nil if unknown.
(defn get-visitor [data]
  (do (println "websockets: fn(get-visitor)->" (:visitor data))
      (:visitor data)))               

(defn ds-agents-add-visitor [agent visitor]
  (update-in @ds-agents [agent] #(assoc % visitor visitor)))
(defn ds-visitors-add [visitor agent]
  (swap! ds-visitors assoc visitor agent))

(defn get-text [data] (:message data))

(defn remove-agent [client]
  (let [visitors (get @ds-agents client)]      ;; COULD BE BAD!!!! is it dead when removed from ds in next lines of code?
    (swap! ds-clients dissoc client)       ;; Cleanup: remove agent from ds-clients
    (doseq [visitor visitors]  
      (swap! ds-clients dissoc visitor)    ;; Cleanup: remove visitors from ds-clients
      (swap! ds-visitors dissoc visitor)) ;; Cleanup: ds-visitors ;; MAY NEED CHANNEL OUT OF THE MAP THAT WE GET
    (swap! ds-agents dissoc agent)))

(defn remove-visitor [client]
  (swap! ds-clients dissoc client)       ;; Cleanup: remove agent from ds-clients
  (update-in @ds-agents [(get-agent client)] #(dissoc % client))
  (swap! ds-visitors dissoc client))    ;; Cleanup: remove agent from ds-visitors

(defn close-cleanup-ds [client]
  (if (is-agent client)
    (do (println "bazolla!")
        (remove-agent client)
        (println "bazolla2!"))
    (remove-visitor client)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling
(defn send-msg2 [client1 client2 msg]
  (do (if client1
        (send! client1 msg false))
      (if client2
        (send! client2 msg false))))

(defn msg-init [client1 client2]
  "Send address of opposite end to both clients"
  (send! client1 (generate-string {:channel client2}) false)
  (send! client2 (generate-string {:channel client1}) false))

(defn msg-text [sender data]
  (let [agent   (get-agent   sender)
        visitor (get-visitor data)
        text    (get-text    data)]
    (send-msg2 agent visitor (generate-string ;; TODO FIX :agent :visitor !!!
                              {:agent "agent-baz" :visitor "visitor-bar" :message text}))))

(defn msg-close [client]
  (let [agent   (get-agent   client)
        text    "chat was terminated!"]
    (if (= client agent)
      (do (println "websockets: agent closed, send closed message to all visitors" (get-agent-visitors agent))
          (doseq [visitor (get-agent-visitors agent)]
            (println "Sending msg to visitor " visitor)
            (send! visitor (generate-string {:agent agent :visitor visitor :message text}) false)))
      (do (println "websockets: visitor closed, send closed message to the agent" client agent)
          (if (= agent nil) (send! agent (generate-string {:agent agent :visitor client :message text}) false))
          (println "barbazbop")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in ds-clients.
;; TODO- LOG MESSAGES, 
(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (println req)
    (if (contains? (get-in req [:session :cemerick.friend/identity :authentications nil :roles]) :chatb0x.user/agent)
      (do (println "Agent connected: " channel) ;; Agent
          (swap! ds-clients assoc channel {:name nil :email nil :room nil}) ;; Add to ds-clients
          (swap! ds-agents assoc channel {})) 
      (let [ch-agent   (get-free-agent)            ;; Visitor
            ch-visitor channel
            vis-email   (get-in req [:session :cemerick.friend/identity :authentications nil :username])
            agent-email "garbage-for-now@hotmail.com"]
        (if (= ch-agent nil)
          (println "ERROR EVENTUALLY!!! no agents on, ignoring visitor!")
          (do (println "Visitor connected: " channel)
              (swap! ds-clients assoc ch-visitor {:name nil :email vis-email :room nil}) ;; Add to ds-clients
              (ds-agents-add-visitor ch-agent   ch-visitor)
              (ds-visitors-add       ch-visitor ch-agent)
              (send! ch-agent (generate-string {:visitor (str ch-visitor) :vis-email vis-email}) false)
              (send! ch-visitor (generate-string {:agent-email agent-email}) false)))))
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (println "on-receive channel:" channel " data:" data)
                          (msg-text channel data)))
    ;; CLOSE
    (on-close channel   (fn [status]
                          (println channel "disconnected. status: " status)
                          (msg-close channel)
                          (println "baz")
                          (close-cleanup-ds channel)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commented code
(comment  (defn send-msg [message-map room]
            (let [client-filter-fn (fn [room] (fn [client] (if (= room (:room (val client))) true false)))
                  clients-in-room (fn [room clients] (filter (client-filter-fn room) clients))
                  channels-to-room (keys (clients-in-room room @comment-clients))
                  message-string (generate-string message-map)]
              (when (seq channels-to-room)
                (println "sending message: " message-map "to" (count channels-to-room) "channels")
                (doseq [channel channels-to-room]
                  (send! channel message-string false))))))

(comment  )

(comment  (defn send-message-to-clients [msg]
            (let [clients (keys @clients)]
              (when (seq clients)
                (doseq [client clients]
                  (send! client msg false))))))
