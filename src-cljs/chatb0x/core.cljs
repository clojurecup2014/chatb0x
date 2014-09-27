(ns chatb0x.core
  (:require [clojure.browser.repl]
            [figwheel.client :as fw :include-macros true]
            [kioo.om :refer [html content set-style set-attr do-> substitute listen add-class]]
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

(defonce app-state (atom {}))

;; Mockup stuff ========================================

(println "chatb0x.core is connected")

(defn default-action [action]
  (fn [e]
    (.preventDefault e)
    (action)))

(defn by-id [id]
  (.getElementById js/document id))

(def ws-url (str "ws://" (.-host js/location) "/chatb0x/ws"))
(def socket (js/WebSocket. ws-url))

(defn send-message [id] 
  (let [message (.-value (by-id id))]
    (.send socket {:msg message})
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
        (println "WebSocket connected. Destination: " ws-url)))

;; The keys are all ints, so sort them such that :10 > :2
(defn msg-comparator [key1 key2] (compare (read-string (name key1))
                                          (read-string (name key2))))

;; FIXME
(set! (.-onmessage socket)
      (fn [event]
        (let [json-data (.parse js/JSON (.-data event))
              data (js->clj json-data :keywordize-keys true)
              sorted-message-map (into (sorted-map-by msg-comparator)
                                       (conj (:msg-vect @app-state) data))]
          ;;(println "socket.onmessage data:" data)
          (swap! app-state assoc :msg-vect sorted-message-map))))

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
  ())

;; ============================================================

(defn init [snippet]
  (fn [data]
    (om/component (snippet data))))

;; ============================================================

(def my-message-vect [1 {:gravatar-url "http://www.gravatar.com/avatar/94736001f6c023d37cd5d132f092bf3b" :author "John McCarthy" :message "Welcome to chatb0x. Please let me know if you have any questions."}])

(defsnippet chat-message-snippet "public/chatb0x-box.html" 
  [:div.first-conversation]
  [msg-vect]
  {[:img] (set-attr :src (:gravatar-url (second msg-vect)))
   [:h5.media-heading] (content (:author (second msg-vect)))
   [:small] (content (:message (second msg-vect)))})

(defsnippet chatb0x-snippet "public/chatb0x-box.html"
  [:div.conversation-wrap]
  [data]
  {[:div.conversation-wrap] (add-class "pull-right")
   [:div.first-conversation] (substitute (chat-message-snippet my-message-vect))
   [:div.extra-chat] (substitute nil)
   [:input#message-small] (listen :onKeyDown #(when (= (.-key %) "Enter")
                                                (send-message "message-small")))})

;; ============================================================

(defn chatb0x-view [data owner]
  (reify
    om/IWillMount
    (will-mount [_]
      (get-chat-session))
    om/IRender
    (render [_]
      (om/build (init chatb0x-snippet) data))))

(defn page-view [data owner]
  (reify
    om/IRender
    (render [_]
      ;;FIXME
      (if true ;;(:available-for-chat data)
        (om/build chatb0x-view data)))))

(om/root page-view app-state 
         {:target (.appendChild 
                   (.-body js/document) 
                   (.createElement js/document "div"))})
