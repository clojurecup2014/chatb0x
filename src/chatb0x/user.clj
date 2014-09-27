(ns chatb0x.user
   (:require [cemerick.friend :as friend]
             (cemerick.friend [workflows :as workflows]
                              [credentials :as creds])
             [clojure.string :as str]))

;;; Friend atom and accessor functions

(def users (atom {"friend@gmail.com" {:username "friend@gmail.com"
                                      :password (creds/hash-bcrypt "clojure")
                                      :role :admin}
                  "agent@chatb0x.clojurecup.com" {:username "agent@chatb0x.clojurecupcom"
                                                  :password (creds/hash-bcrypt "clojure")
                                                  :role :agent
                                                  :sites #{"clojurecup.com"}}}))

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
                          :password (creds/hash-bcrypt password)))))

(defn modify-role
  "Move the user to a different role, e.g. to promote to moderator"
  [user role]
  (assoc-in user [:role] role))

(defn add-site
  "Authorize the agent to receive calls for the site"
  [user site]
  (update-in user [:sites] conj site))

(defn remove-site
  "Remove the agent's authorization to receive calls for the site"
  [user site]
  (update-in user [:sites] disj site))

(defn get-friend-username [req] ; This doesn't smell right...
  (:username (friend/current-authentication req)))

(defn trim-email-address [email] (first (re-find #"(\S)+(?=@)" email)))

