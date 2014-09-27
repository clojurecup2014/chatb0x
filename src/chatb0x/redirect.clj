(ns chatb0x.redirect)

(defn perm-redirect
  "Returns a Ring response for an HTTP 301 redirect"
  [url]
  {:status 301
   :headers {"Location" url}
   :body ""})

(defn wrap-drop-www [handler]
  "www is redundant, hard to say, old and busted"
  (fn [req]
    (if (= "www.c0mment.com" ((req :headers) "host"))
      (perm-redirect "http://c0mment.com")
      (handler req))))
