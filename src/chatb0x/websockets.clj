(ns chatb0x.websockets
  (:require [org.httpkit.server :refer [with-channel on-close on-receive send!]]
            [chatb0x.user :refer :all]
            [cheshire.core :refer [generate-string]]
            [cemerick.friend :as friend]
            [digest :refer [md5]]
            [clojure.string :as str]))

(defn calc-gravatar [req]
  (let [email (get-in req [:session :cemerick.friend/identity :authentications nil :username])]
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
;; Data Structures
(def ds-clients (atom {}))  ;; Key: channel;       data: email, name, etc
(def ds-agents (atom {}))   ;; Key: agent-channel; data: {vis1-chnl, vis2-chnl, ...}
(def ds-visitors (atom {})) ;; Key: visitor-chnl;  data: agent-chnl

(defn get-free-agent [] (first (keys @ds-agents))) ;; TOOD: make rand-nth?
(defn is-agent [client] (@ds-agents client))

;; Return nil if unknown
(defn get-agent [client]
  (let [agent1 (@ds-visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (is-agent client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))
(defn get-agent-visitors [client]
  (get @ds-agents client nil))

;; This gets the visitor from the agent message header
;; Return nil if unknown.
(defn get-visitor [data]
  (:visitor data))               

(defn ds-agents-add-visitor [agent visitor]
  (swap! ds-agents assoc-in [agent visitor] visitor)
  ;;(update-in @ds-agents [agent] #(assoc % visitor visitor))
  )

(defn ds-visitors-add [visitor agent]
  (swap! ds-visitors assoc visitor agent))

(defn get-text [data]
  (:message (read-string data)))

(defn remove-agent [client]
  (let [visitors (get @ds-agents client)]
    (swap! ds-clients dissoc client)       ;; Cleanup: remove agent from ds-clients
    (doseq [visitor visitors]  
      (swap! ds-clients dissoc visitor)    ;; Cleanup: remove visitors from ds-clients
      (swap! ds-visitors dissoc visitor)) ;; Cleanup: ds-visitors ;; MAY NEED CHANNEL OUT OF THE MAP THAT WE GET
    (swap! ds-agents dissoc agent)))

(defn remove-visitor [client]
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
  (let [value (get-in req
                      [:session :cemerick.friend/identity :authentications nil :username])]
    value))
(defn add-new-ds-clients [req channel]
                   (swap! ds-clients assoc channel
                          {:name nil
                           :gravatar-url (calc-gravatar req)
                           :room nil}))
(defn add-new-ds-agents [channel] (swap! ds-agents assoc channel {}))
(defn add-new-ds-visitors [visitor]
  (let [agent (get-free-agent)]
    (swap! ds-visitors assoc visitor agent)
    (if agent (swap! ds-agents assoc-in [agent visitor] visitor))))
(defn visitor-is-connected [channel] (get @ds-visitors channel))

(defn connect-unconnected-visitors-to-agents []
  (doseq [visitor @ds-visitors]
    (println "visitor:: " visitor)))

(defn get-agent-from-client [client]
  (let [agent1 (@ds-visitors client)              ;; Client is visitor, lookup agent
        agent2 (if (is-agent client) client nil)] ;; Client is agent or not
    (or agent1 agent2)))
(def client-has-associated-agent get-agent-from-client)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Agent visitor handling
(defn msg-init [client1 client2]
  "Send address of opposite end to both clients"
  (send! client1 (pr-str {:channel client2}) false)
  (send! client2 (pr-str {:channel client1}) false))

(defn msg-text [sender data]
  (let [agent   (get-agent   sender)
        visitor (get-visitor data)
        text    (get-text data)
        msg     (pr-str {:ch-visitor (:ch-visitor data) :message text})]
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
            (send! visitor (pr-str {:agent agent :visitor visitor :message text}) false)))
      (do (println "websockets: visitor closed, send closed message to the agent" client agent)
          (if agent (send! agent (pr-str {:agent agent :visitor client :message text}) false))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; If user, then connect up agent. Both user and agent get init message.
;; If agent, then no need to connect up. Just store agent in ds-clients.

(defn debug-print-data-structures []
  (do (println "Clients DS:  " @ds-clients)
      (println "Agents DS:   " @ds-agents)
      (println "Visitors DS: " @ds-visitors)))
;;(def list-unconnected-clients (atom (list)))

(defn chat-ws [req]
  (with-channel req channel
    ;; CONNECT
    (debug-print-data-structures)
    (if (and (is-client-an-agent req) (agent-is-authorized req))
      (do (println "ws-connected:" "\n\t agent-channel" (:async-channel req)) ;; TODO: Connect all unconnected visitors to agent(s)
          (add-new-ds-clients req channel)
          (add-new-ds-agents      channel)
          (connect-unconnected-visitors-to-agents))
      (do (println "ws-connected:" "\n\t visitor-channel" (:async-channel req))
          (add-new-ds-clients req channel)
          (add-new-ds-visitors    channel)))
    (debug-print-data-structures)
    ;; RECEIVE
    (on-receive channel (fn [data]
                          (do (println "ws-receive:" "\n\t channel"  channel "\n\t data"  data)
                              (msg-text channel data))))
    ;; CLOSE
    (on-close channel   (fn [status]
                          (do (println channel "ws-close:" "\n\t status"  status)
                              (msg-close channel)
                              (close-cleanup-ds channel))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commented code
(comment (do (if (is-visitor-unconnected? channel)
               (let [ch-agent   (get-free-agent)
                     ch-visitor channel
                     gravatar-url   (calc-gravatar req)] ;; Visitor
                 (println "Giving visitor an agent...")
                 (println ch-visitor)
                 (ds-agents-add-visitor ch-agent   ch-visitor)
                 (ds-visitors-add   ch-visitor ch-agent)
                 (send! ch-agent (pr-str {:ch-visitor (str ch-visitor) :gravatar-url gravatar-url}) false)))))
(comment (contains? 
          (get-in req
                  [:session :cemerick.friend/identity :authentications nil :roles]
                  :chatb0x.user/agent)))
