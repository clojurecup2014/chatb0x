(ns chatb0x.core
  (:require [clojure.browser.repl]
            [figwheel.client :as fw :include-macros true]
            [kioo.om :refer [html 
                             content 
                             set-style 
                             set-attr 
                             do-> 
                             substitute 
                             listen 
                             add-class 
                             remove-class]]
            [kioo.core :refer [handle-wrapper]]
            [om.core :as om :include-macros true]
            [om.dom :as dom :include-macros true]
            [cljs-hash.md5 :refer [md5]]
            [clojure.string :refer [trim lower-case replace replace-first]]
            [cljs.reader :refer [read-string]])
  (:require-macros [kioo.om :refer [defsnippet deftemplate]]))

(enable-console-print!)

(fw/watch-and-reload
 :websocket-url   "ws://localhost:3449/figwheel-ws"
 :jsload-callback (fn [] (print "reloaded")))

(defonce app-state (atom {:msg-vect [{:gravatar-url "http://www.gravatar.com/avatar/94736001f6c023d37cd5d132f092bf3b" :author "Chatb\u2205x Agent" :message "Welcome to chatb\u2205x. Please let me know if you have any questions."}
                                     ]}))

;; Mockup stuff ========================================

(println "chatb0x.core is connected")

(defn default-action [action]
  (fn [e]
    (.preventDefault e)
    (action)))

(defn by-id [id]
  (.getElementById js/document id))

(def host (.-host js/location))
(def path (.-pathname js/location))

(def ws-url (str "ws://" host "/chatb0x/ws"))

(def socket (js/WebSocket. ws-url))

(defn send-message [id] 
  (let [message (.-value (by-id id))]
    (.send socket {:message message :host host :path path})
    (set! (.-value (by-id id)) nil)
    (println "chatb0x sent message:" message)))

(defn set-send-field [id]
  (set! (.-onkeydown (by-id id)) 
        #(when (or (= (.-keyCode %) 13)
                   (= (.-which %) 13)) 
           (send-message id))))
(comment
  (set-send-field "message-big")
  (set-send-field "message-small"))

;; define your app data so that it doesn't get over-written on reload
(defonce app-data (atom {}))

(set! (.-onopen socket)
      (fn [event]
        (.send socket {:initial-path path})
        (println "WebSocket connected. Destination: " ws-url)))

;; The keys are all ints, so sort them such that :10 > :2
(defn msg-comparator [key1 key2] (compare (read-string (name key1))
                                          (read-string (name key2))))

;; FIXME
(set! (.-onmessage socket)
      (fn [event]
        (let [data (cljs.reader/read-string (.-data event))]
          (println "socket.onmessage data:" data)
          
          (swap! app-state #(update-in % [:msg-vect] conj data))
          (println "app-state:" @app-state)
          )))

(defn gravatar [email]
  (if email
    (do
      (swap! app-state assoc :gravatar-url (str "http://www.gravatar.com/avatar/"
                                                (-> email
                                                    (trim)
                                                    (lower-case)
                                                    (md5))))
      (str "http://www.gravatar.com/avatar/"
           (-> email
               (trim)
               (lower-case)
               (md5))))
    (do
      (swap! app-state assoc :gravatar-url
             ("http://www.gravatar.com/avatar"))
      ("http://www.gravatar.com/avatar"))))

;; ============================================================

(defn get-chat-session []
  )

;; ============================================================

(defn init [snippet]
  (fn [data]
    (om/component (snippet data))))

;; ============================================================

(defsnippet chat-message-snippet "public/chatb0x-box.html" 
  [:div.first-conversation]
  [msg-vect]
  {[:img] (set-attr :src (:gravatar-url msg-vect))
   [:h5.media-heading] (content (:author msg-vect))
   [:small] (content (:message msg-vect))})

(defsnippet agent-message-snippet "public/chatb0x-box.html"
  [:div.first-agent-conversation]
  [msg-vect]
  {[:img] (set-attr :src (:gravatar-url (second msg-vect)))
   [:h5.media-heading] (content (:author (second msg-vect)))
   [:small] (content (:message (second msg-vect)))})

(defsnippet chatb0x-snippet "public/chatb0x-box.html"
  [:div.conversation-wrap]
  [data]
  {[:div.conversation-wrap] (add-class "pull-right")
   [:div.first-conversation] (substitute (map chat-message-snippet (:msg-vect data)))
   [:div.extra-chat] (substitute nil)
   [:input#message-small] (do->
                           (set-attr :id "chatb0x-message")
                           (listen :onKeyDown #(when (= (.-key %) "Enter")
                                                 (send-message "chatb0x-message"))))})

(defsnippet agent-chat-snippet "public/chatb0x-box.html"
  [:div.agent-conversation-wrap]
  [data]
  {[:div.agent-conversation-wrap] (do->
                                   (add-class "pull-left")
                                   (add-class "col-lg-12")
                                   (remove-class "col-lg-8"))
   [:div.first-agent-conversation] (substitute (map chat-message-snippet 
                                                    (filter #(:message %)
                                                            (:msg-vect data))))
   [:div.extra-chat] (substitute nil)
   [:input#message-big] (do->
                         (set-attr :id "agent-message")
                         (listen :onKeyDown #(when (= (.-key %) "Enter")
                                               (send-message "agent-message"))))})

(defsnippet visitor-list-snippet "public/agent-chatb0x.html"
  [:a.first]
  [channel]
  {[:a.first] (content "Hello!")
   })

(defsnippet list-snippet "public/agent-chatb0x.html"
  [:div#visitor-list]
  [data]
  {[:a.first] (substitute (map visitor-list-snippet 
                                      (filter #(:ch-visitor %)
                                              (:msg-vect data))))})



;; ============================================================

(defn chatb0x-view [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (get-chat-session))
    om/IRender
    (render [_]
      (om/build (init chatb0x-snippet) data))))

(defn agent-view [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (get-chat-session))
    om/IRender
    (render [_]
      (do
       (om/build (init agent-chat-snippet) data)))))

(defn list-view [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (get-chat-session))
    om/IRender
    (render [_]
      (do
       (om/build (init list-snippet) data)))))

(defn page-view [data owner]
  (reify
    om/IRender
    (render [_]
      ;;FIXME
      (if (not= path "/agent-chat") ;;(:available-for-chat data)
        (om/build chatb0x-view data)
        (om/build agent-view data)))))

(defn page-view2 [data owner]
  (reify
    om/IRender
    (render [_]
      ;;FIXME
      (om/build list-view data))))

(if (not= path "/agent-chat")
  (om/root page-view app-state 
           {:target (.appendChild 
                     (.-body js/document) 
                     (.createElement js/document "div"))})
  (do
    (om/root page-view app-state
             {:target (.getElementById js/document "agent-message-box")})
    (om/root page-view2 app-state
             {:target (.getElementById js/document "visitor-list")}))) 
