(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string]]))

;; BRADS FUNCTIONS FOR DATA
;; get-assigned-agents, get-unassigned-agents, get-free-agents 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
(def ds-clients (atom {}))  ;; Key-channel; data-email,name,etc
(def ds-agents (atom {}))   ;; Key-agent-channel; data-{vis1, vis2, etc}

;; Functions
;;(defn clients-get)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(comment  (defn send-message-to-clients [msg]
            (let [clients (keys @clients)]
              (when (seq clients)
                (doseq [client clients]
                  (send! client msg false))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling
(defn msg-init [client1 client2]
  "Types: channel, channel"
  (send! client1 client2)
  (send! client2 client1))

(defn msg-text [client-send data]
  (let [client-receive (:receive-addr data)
        text           (:text         data)])
  (send! client-receive {:client-send client-send :text text}))

(defn msg-close [client]
  (send! client {:text "chat was terminated"}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in ds-clients.
;; TODO- connect visitor w/ agent; reconsider the problem of multi-user to single agent in datastructure
(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (println channel "connected")
    (swap! ds-clients assoc channel {:name nil :email nil :room nil}) ;; Add to ds-clients
    (let [agents (user/get_agents)]
      (true? (channel agents)                                         ;; Is this user an agent?
             (swap! ds-agents assoc channel {})))                     ;; Add to ds-agents. key-channel; value-{<visitors>}
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (println "on-receive channel:" channel " data:" data)
                          (println "comment-ws comment-clients" @comment-clients)
                          ;; Make a history later.. (swap! comment-clients assoc-in [channel] (read-string data))
                          (send-message-to-clients data)))))
    ;; CLOSE
    (on-close channel   (fn [status]
                          (println channel "disconnected. status: " status)
                          (swap! comment-clients dissoc channel)
                          (msg-close )))

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
