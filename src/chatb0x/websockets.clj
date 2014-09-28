(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [clojure.edn :as edn]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string parse-string]]))

;; BRADS FUNCTIONS FOR DATA
;; get-assigned-agents, get-unassigned-agents, get-free-agents 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Data Structures
(def agent-addr (atom nil))
(def vis-addr (atom nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling
(defn msg-close [client]
  (send! client {:text "chat was terminated"} false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in ds-clients.
;; TODO- connect visitor w/ agent; reconsider the problem of multi-user to single agent in datastructure
(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (println channel "connected")
    (if (nil? @agent-addr)
      (do (println "agent added")
          (reset! agent-addr channel))
      (do (println "vis added")
          (reset! vis-addr channel)))
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (prn "on-receive channel:" channel " data:" data)
                          (let [data (edn/read-string data)]
                            (if (= @agent-addr channel)
                              (do
                                (prn "Sending to channel:" channel " data:" data)
                                (send! @vis-addr (pr-str data) false)) ;; Send agent msg to visitor
                              (do
                                (prn "Sending to channel::" channel " data:" data)
                                (send! @agent-addr (pr-str data) false)))))) ;; Send visitor msg to agent
    ;; CLOSE
    (on-close channel (fn [status]
                        (prn channel "disconnected. status: " status)
                        (if (= @vis-addr channel)
                          (do (prn "visitor disconnecting")
                              (reset! vis-addr nil) ;; Visitor disconnected
                              (if @agent-addr (msg-close @agent-addr)))
                          (do (prn "agent disconnecting")
                              (reset! agent-addr nil) ;; Agent disconnected
                              (if @vis-addr (msg-close @vis-addr))))))))
