(ns chatb0x.user
   (:require [cemerick.friend :as friend]
             (cemerick.friend [workflows :as workflows]
                              [credentials :as creds])
             [clojure.string :as str]))

(def users (atom {"friend@gmail.com" {:username "friend@gmail.com"
                                      :password (creds/hash-bcrypt "clojure")
                                      :roles #{::admin}
                                      :sites #{"clojurecup.com"}
                                      :in-chat false}
                  "agent1@example.com" {:username "agent1@example.com"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :roles #{::agent}
                                                  :sites #{"example.com"}
                                                  :in-chat false}
                  "agent2@example.com" {:username "agent1@example.com"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :roles #{::agent}
                                                  :sites #{"example.com"}
                                                  :in-chat true}
                  "agent@chatb0x.clojurecup.com" {:username "agent@chatb0x.clojurecupcom"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :roles #{::agent}
                                                  :sites #{"clojurecup.com"}
                                                  :in-chat false}
                  "agent1@chatb0x.clojurecup.com" {:username "agent1@chatb0x.clojurecupcom"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :roles #{::agent}
                                                  :sites #{"clojurecup.com"}
                                                  :in-chat true}
                  "agent2@chatb0x.clojurecup.com" {:username "agent2@chatb0x.clojurecupcom"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :roles #{::agent}
                                                  :sites #{"clojurecup.com"}
                                                  :in-chat false}}))

(defn check-registration
  "Validates the username and password"
  [username password]
  (and (not (nil? (re-matches #"^(?=.*\d)(?=.*[a-zA-Z]).{7,50}$" password)))
       (not (str/blank? username))
       (not (contains? @users username))))

(defn create-user
  [{:keys [username password] :as user-data}]
  (let [lower-case-username (str/lower-case username)]
    (->  user-data (assoc :username lower-case-username
                          :password (creds/hash-bcrypt password)
                          :roles #{::agent}
                          :sites #{}
                          :in-chat false))))

(defn modify-role
  "Move the user to a different role, e.g. to promote to admin"
  [user role]
  (swap! users #(assoc-in % [user :role] role)))

(defn add-site
  "Authorize the agent to receive calls for the site"
  [user site]
  (swap! users #(update-in % [user :sites] conj site)))

(defn remove-site
  "Remove the agent's authorization to receive calls for the site"
  [user site]
  (swap! users #(update-in % [user :sites] disj site)))

(defn enter-chat
  "Marks the agent as being in chat"
  [user]
  (swap! users #(assoc-in % [user :in-chat] true)))

(defn exit-chat
  "Marks the agent as being out of chat"
  [user]
  (swap! users #(assoc-in % [user :in-chat] false)))

(defn get-sites
  "Returns the set of sites a user is assigned to"
  [user]
  (get-in user [1 :sites]))

(defn get-roles
  "Returns the set of roles a user has"
  [user]
  (get-in user [1 :roles]))

(defn get-agents
  "Returns all agents"
  []
  (into {} (filter #((get-roles %) ::agent) @users)))

(defn get-assigned-agents
  "Returns map of agents assigned to site"
  [site]
  (into {} (filter #((get-sites %) site) (get-agents))))

(defn get-unassigned-agents
  "Returns map of agents not assigned to site"
  [site]
  (into {} (remove #((get-sites %) site) (get-agents))))

(defn get-free-agents
  "Returns map of agents assigned to site and not in chat"
  [site]
  (into {} (remove #(get-in % [1 :in-chat]) (get-agents site))))

(defn get-friend-username [req]
  (:username (friend/current-authentication req)))

(defn trim-email-address [email] (first (re-find #"(\S)+(?=@)" email)))

