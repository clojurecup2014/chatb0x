(ns chatb0x.data
  (:require [clojure.string :as str]
            [cheshire.core :refer [generate-string parse-string]]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert channel type to string, and vice versa
(def channel-ip-map (atom {}))

(defn ip-to-chnl [ip] (get @channel-ip-map ip))

(defn chnl-to-ip [chnl]
  (let [str (re-find (re-matcher #"\>\/\d+\.\d+\.\d+\.\d+\:\d+" (generate-string (pr-str chnl))))]
    (if str
      (subs str 2)
      nil)))

(defn add-chnl-to-map [chnl]
  (let [ip (chnl-to-ip chnl)]
    (swap! channel-ip-map assoc ip chnl)))

(defn del-chnl-from-map [chnl]
  (let [ip (chnl-to-ip chnl)]
    (swap! channel-ip-map dissoc ip)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Database
(def map-clients (atom {}))  ;; Key: channel;       data: email, name, etc
(def map-agents (atom {}))   ;; Key: agent-channel; data: {vis1-chnl, vis2-chnl, ...}
(def map-visitors (atom {})) ;; Key: visitor-chnl;  data: agent-chnl

(defn db-add-client [channel user-data]
  (swap! map-clients assoc channel user-data))

(defn db-add-visitor [ch-visitor ch-agent] (swap! map-visitors assoc ch-visitor ch-agent))

(defn db-add-agent [ch-agent ch-visitor]
  (if (= ch-visitor {})
    (swap! map-agents assoc ch-agent {})
    (swap! map-agents assoc-in [ch-agent ch-visitor] ch-visitor)))

(defn db-get-free-agent [] (first (keys @map-agents)))

(defn db-get-agent-visitors [ch-client]
  (get @map-agents ch-client nil))

(defn db-get-visitor-data [ch-visitor] (get @map-visitors ch-visitor false))
(defn db-get-client-data [ch-client] (get-in @map-clients [ch-client]))
(defn agent-in-db [ch-client] (db-get-agent-visitors ch-client))

(defn visitor-in-db [ch-visitor] (db-get-visitor-data ch-visitor))

(defn db-delete-agent [ch-agent] (swap! map-agents dissoc ch-agent))
(defn db-delete-visitor [ch-visitor] (swap! map-visitors dissoc ch-visitor))
(defn db-delete-client [ch-client] (swap! map-clients dissoc ch-client))
(defn db-delete-agents-visitor [ch-agent ch-visitor]
  (if ch-agent (swap! map-agents update-in [ch-agent] dissoc ch-visitor)))

(defn db-get-agent-with-any-chnl [ch-client]
  (let [ch-agent1 (@map-visitors ch-client)              ;; Client is visitor, lookup agent
        ch-agent2 (if (agent-in-db ch-client) ch-client nil)] ;; Client is agent or not
    (or ch-agent1 ch-agent2)))

;; Sits on functions from above
(defn add-agent [ch-agent user-data]
  (do (db-add-client ch-agent user-data)
      (db-add-agent ch-agent {})))

(defn add-visitor [ch-visitor user-data]
  (do (add-chnl-to-map ch-visitor)
      (db-add-client ch-visitor user-data)
      (db-add-visitor ch-visitor nil)))

(defn delete-agent [ch-agent]
  (let [ch-visitors (db-get-agent-visitors ch-agent)]
    (doseq [ch-visitor ch-visitors]   ;; Remove visitors
      (db-delete-client ch-visitor)
      (db-delete-visitor ch-visitor)) 
    (db-delete-client ch-agent)
    (db-delete-agent ch-agent)))      ;; Remove agent

(defn delete-visitor [ch-visitor]
  (do (del-chnl-from-map ch-visitor)
      (db-delete-agents-visitor (db-get-agent-with-any-chnl ch-visitor) ch-visitor)
      (db-delete-client ch-visitor)       ;; Cleanup: remove agent from map-clients
      (db-delete-visitor ch-visitor)))

(def db-all-visitors @map-visitors)
(defn db-set-agents-chat-visitor [ch-agent ch-visitor]
  (swap! map-agents assoc-in [ch-agent "sel-vis"] ch-visitor))

(defn db-get-agents-chat-visitor [ch-agent]
  (get-in @map-agents [ch-agent "sel-vis"]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug lines
(defn print-datastructs [] (doseq [[k v] {"agents" @map-agents
                                          "visitors" @map-visitors
                                          "clients" @map-clients
                                          "channel-ip-map" @channel-ip-map}]
                                  (println k)
                                  (doseq [[k2 v2] v]
                                    (println k2 v2))))
