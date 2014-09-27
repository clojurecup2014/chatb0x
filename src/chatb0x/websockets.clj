(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string]]))

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
                          (println "on-receive channel:" channel " data:" data)
                          (if (= @agent-addr channel)
                            (do (println "Sending to vis") (send! @vis-addr (generate-string data) false)) ;; Send agent msg to visitor
                            (do (println "Sending to agent") (send! @agent-addr (generate-string data) false))))) ;; Send visitor msg to agent
    ;; CLOSE
    (on-close channel (fn [status]
                        (println channel "disconnected. status: " status)
                        (if (= @vis-addr channel)
                          (do (println "visitor disconnecting")
                              (reset! vis-addr nil) ;; Visitor disconnected
                              (if @agent-addr (msg-close @agent-addr)))
                          (do (println "agent disconnecting")
                              (reset! agent-addr nil) ;; Agent disconnected
                              (if @vis-addr (msg-close @vis-addr))))))))
