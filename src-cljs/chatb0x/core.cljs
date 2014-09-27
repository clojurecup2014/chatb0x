(ns chatb0x.core
  (:require [clojure.browser.repl]
            [figwheel.client :as fw :include-macros true]))

(enable-console-print!)

;; define your app data so that it doesn't get over-written on reload
;; (defonce app-data (atom {}))

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

(set-send-field "message-big")
(set-send-field "message-small")


(fw/watch-and-reload
 :websocket-url   "ws://localhost:3449/figwheel-ws"
 :jsload-callback (fn [] (print "reloaded")))

