(ns chatb0x.data
  (:require [clojure.string :as str]))

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

(def map-clients (atom {}))  ;; Key: channel;       data: email, name, etc
(def map-agents (atom {}))   ;; Key: agent-channel; data: {vis1-chnl, vis2-chnl, ...}
(def map-visitors (atom {})) ;; Key: visitor-chnl;  data: agent-chnl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Debug lines
(defn print-datastructs [] (println "\nagents" @map-agents
                                    "\nvisitors" @map-visitors
                                    "\nclients" @map-clients
                                    "\nchannel-ip-map" @channel-ip-map))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions
(defn add-new-visitor [req channel]
  (do (add-new-client req channel)
      (data/visitors-add channel nil)
      (data/add-chnl channel)))

(defn add-new-agent [req channel]
  (do (add-new-client req channel)
      (data/agents-add-visitor channel {})))

(defn add-new-client [req channel]
                   (swap! data/clients assoc channel
                          {:name nil
                           :gravatar-url (calc-gravatar req)
                           :room nil}))

(defn get-visitor-data [ch-visitor] (get @map-visitors ch-sender false))

(defn visitor-is-in-map [ch-visitor] (get-visitor-data ch-visitor))

(defn is-agent [ch-client] (@map-agents ch-client))

(defn get-free-agent [] (first (keys @map-agents))) ;; TOOD: make rand-nth?

(defn get-agent [ch-client]
  (let [ch-agent1 (@map-visitors ch-client)              ;; Client is visitor, lookup agent
        ch-agent2 (if (is-agent ch-client) ch-client nil)] ;; Client is agent or not
    (or ch-agent1 ch-agent2)))

(defn get-agent-visitors [ch-client]
  (get @map-agents ch-client nil))

(defn agents-add-visitor [agent visitor]
  (if (= visitor {})
    (swap! map-agents assoc agent {})
    (swap! map-agents assoc-in [agent visitor] visitor)))

(defn visitors-add [visitor agent] (swap! map-visitors assoc visitor agent))

(defn remove-agent [client]
  (let [visitors (get @map-agents client)]
    (swap! map-clients dissoc client)       ;; Cleanup: remove agent from map-clients
    (doseq [visitor visitors]  
      (swap! map-clients dissoc visitor)    ;; Cleanup: remove visitors from map-clients
      (swap! map-visitors dissoc visitor)) ;; Cleanup: map-visitors ;; MAY NEED CHANNEL OUT OF THE MAP THAT WE GET
    (swap! map-agents dissoc agent)))

(defn remove-visitor [client]
  (remove-chnl client)
  (swap! map-clients dissoc client)       ;; Cleanup: remove agent from map-clients
  (if (get map-visitors client)
    (swap! map-agents update-in [(get-agent client)] dissoc client))
  (swap! map-visitors dissoc client))

(defn remove-client [client] ;; TODO: simplify by making functions innocuous. (do (remove-agent) (remove-visitor))
  (if (is-agent client)
    (remove-agent   client)
    (remove-visitor client)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Convert channel type to string, and vice versa
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
