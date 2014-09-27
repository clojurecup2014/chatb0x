(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [cheshire.core :refer [generate-string]]))

(def clients (atom {}))
(def comment-clients (atom {}))

(defn chat-ws [req]
  (with-channel req channel
    (swap! comment-clients assoc channel {:name nil :email nil :room nil})
    (println channel "connected")
    (on-close channel
              (fn [status]
                (swap! comment-clients dissoc channel)
                (println channel "disconnected. status: " status)))
    (on-receive channel (fn [data]
                         (println "on-receive channel:" channel " data:" data)
                         (swap! comment-clients assoc-in [channel] (read-string data))
                         (println "comment-ws comment-clients" @comment-clients)))))

(defn send-msg [message-map room]
  (let [client-filter-fn (fn [room] (fn [client] (if (= room (:room (val client))) true false)))
        clients-in-room (fn [room clients] (filter (client-filter-fn room) clients))
        channels-to-room (keys (clients-in-room room @comment-clients))
        message-string (generate-string message-map)]
    (when (seq channels-to-room)
      (println "sending message: " message-map "to" (count channels-to-room) "channels")
      (doseq [channel channels-to-room]
        (send! channel message-string false)))))
