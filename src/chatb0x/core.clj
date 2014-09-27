(ns chatb0x.core
  (:require [chatb0x.redirect :refer (wrap-drop-www)]
            [chatb0x.websockets :as ws]
            [chatb0x.brepl :refer (brepl brepl-injection)]
            [net.cgrand.enlive-html :as html]
            [net.cgrand.reload :as reload]
            [compojure.handler :as handler]
            [compojure.route :refer (resources not-found)]
            [compojure.core :refer (GET POST defroutes)]
            [ring.util.response :as resp]
            [ring.middleware.params :refer (wrap-params)]
            [ring.middleware.nested-params :refer (wrap-nested-params)]
            [ring.middleware.keyword-params :refer (wrap-keyword-params)]
            [ring.middleware.session :refer (wrap-session)]
            [ring.middleware.session.store :refer (read-session)]
            [ring.middleware.reload :refer (wrap-reload)]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :as set]
            [cemerick.friend :as friend]
            (cemerick.friend [workflows :as workflows]
                             [credentials :as creds])
            [org.httpkit.server :refer [run-server]]
            [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :as cider])
  (:import java.net.URI)
  (:gen-class))

(reload/auto-reload *ns*) ; To automatically reload Enlive templates -
                                        ; wrap-reload used below in handler

;;; Friend atom and accessor functions

(def users (atom {"friend@gmail.com" {:username "friend@gmail.com" :password (creds/hash-bcrypt "clojure")}}))

(defn check-registration [username password] ; strong password, non-blank username, doesn't already exist
  (and (not (nil? (re-matches #"^(?=.*\d)(?=.*[a-zA-Z]).{7,50}$" password)))
       (not (str/blank? username))
       (not (contains? @users username))))

(defn- create-user
  [{:keys [username password] :as user-data}]
  (let [lower-case-username (str/lower-case username)]
    (->  user-data (assoc :username lower-case-username
                          :password (creds/hash-bcrypt password)))))

(defn get-friend-username [req] ; This doesn't smell right...
  (:username (current-authentication req)))

(defn trim-email-address [email] (first (re-find #"(\S)+(?=@)" email)))

;;; Navigation, Templating, and Snippets

(def navigation-items  (array-map  "Home" "/" "About" "/about" "Contact" "/contact"))

(def navigation-items-user (array-map "Home" "/" "About" "/about" "Contact" "/contact" "App" "/welcome"))

(def navigation-items-invert (set/map-invert navigation-items))

(defn get-navigation-caption [req] (navigation-items-invert (req :uri)))

(html/defsnippet comment-description "public/chatb0x-box.html"
  [:div.detailBox]
  [desc]
  [:p.taskDescription] (html/content desc))

(html/defsnippet auth-profile (io/resource "public/welcome.html")
  [:body :div.user]
  [req]
  [#{:span.user}] (html/content (trim-email-address (get-friend-username req))))

(html/defsnippet navbar (io/resource "public/landing.html")
  [:body :div.navbar]
  [req]  
  [:ul [:li html/first-of-type]] (if (friend/identity req)
                                        ;users see the app item in their menu
                                   (html/clone-for [[caption uri] navigation-items-user]
                                                   [:li] (if (= (req :uri) uri)
                                                           (html/set-attr :class "active")
                                                           identity)
                                                   [:li :a] (html/content caption)
                                                   [:li :a] (html/set-attr :href uri))
                                        ;anonymous users do not see the app
                                   (html/clone-for [[caption uri] navigation-items]
                                                   [:li] (if (= (req :uri) uri)
                                                           (html/set-attr :class "active")
                                                           identity)
                                                   [:li :a] (html/content caption)
                                                   [:li :a] (html/set-attr :href uri)))
  [:div.sign-in-form] (if (friend/identity req) (html/substitute (auth-profile req)) identity))

(html/defsnippet non-app-content (io/resource "public/landing.html")
  [:#content]
  [req]
  [:#content] (case (get-navigation-caption req) 
                "Home" (html/set-attr :id "content") 
                "About" (html/do-> (html/content "See what we're all about")
                                   (html/wrap :h2))
                "Contact" (html/do-> (html/content "Learn how to make contact")
                                     (html/wrap :h2))
                (html/do-> (html/content  "Best check yo self, page not found!")
                           (html/wrap :h2 {:class "alert alert-warning" :style "text-align: center;"}))))

(html/deftemplate landing (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:body :#content] (html/substitute (non-app-content req))
  [:body] (brepl-injection))

;;; Default page for erroneous logins 
(html/deftemplate login (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
                                        ;[:body :#content] (html/substitute (non-app-content req))
  [:body :div.navbar :input] (html/set-attr :style "color: red")
  [:body :div.navbar :input.username] (html/set-attr :placeholder "Re-enter Email Address")
  [:body :div.navbar :input.password] (html/set-attr :placeholder "Re-enter Password")
  [:body] (brepl-injection))

;;; Page for erroneous registrations
(html/deftemplate reregister (io/resource "public/landing.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:body :#content :form :input] (html/set-attr :class "input-block-level btn-lg register alert-danger")
  [:body :div.navbar :ul [:li html/first-of-type]] (html/set-attr :class "active")
  [:body] (brepl-injection))

;;; App page
(html/deftemplate welcome (io/resource "public/welcome.html")
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [#{:span.user}] (html/content (trim-email-address (get-friend-username req) ))
  [:body] (brepl-injection))

;;; Chat box template
(html/deftemplate chatb0x "public/welcome.html"
  [req]
  [:body :div.navbar] (html/substitute (navbar req))
  [:div.container :h1] (html/substitute nil)
  [:div.navbar] (html/after
            (html/html (comment-description "Thing we are commenting on...")))
  [:body] (brepl-injection))


;;; Logging/Debugging
(defn log-request [req]
  (println ">>>>" req)) 

(defn wrap-verbose [h]
  (fn [req]
    (log-request req)
    (h req)))

;;; Compjure routes, site handler, ring server
(defroutes unsecured-site
  (resources "/")
  (GET "/" req (landing req))
  (GET "/about" req (landing req))
  (GET "/contact" req (landing req))
  (GET "/chatb0x" req (chatb0x req))
  (GET "/chatb0x/ws" [] ws/chat-ws)
  (GET "/welcome" req
                                        ;(println "welcome req:" req)
                                        ;(println "(:user (req :params))" (:username (req :params)))
                                        ;(println "user name extraction: " (:authentications (:cemerick.friend/identity (:session req))))
       (friend/authenticated  (welcome req)))
  (GET "/login" req (login req))
  (GET "/logout" req (friend/logout* (resp/redirect "/")))
  (GET "/reregister" req (reregister req))
  (POST "/register" {{:keys [username password] :as params} :params :as req}
        (if  (check-registration username password)
          (let [user (create-user (select-keys params [:username :password]))]        
            (swap! users #(-> % (assoc (str/lower-case username) user))) ; (println "user is " user)        
            (friend/merge-authentication (resp/redirect "/welcome") user)) ; (println "register redirect req: " req)
          (resp/redirect "/reregister") ))  
  (not-found (landing {:uri  "PageNotFound"}))) 

(def secured-site
  (-> unsecured-site
      (friend/authenticate {:allow-anon? true
                            :default-landing-uri "/welcome"
                            :credential-fn #(creds/bcrypt-credential-fn @users %)
                            :workflows [(workflows/interactive-form)]})
                                        ; required Ring middlewares
      ;;(wrap-verbose) ; log the request map
      (wrap-drop-www)
      (wrap-keyword-params)
      (wrap-nested-params)
      (wrap-params)
      (wrap-session)
      (wrap-reload)))
