(ns chatb0x.server
  (:require [chatb0x.core :refer [secured-site]]
            [org.httpkit.server :refer [run-server]]
            [clojure.tools.nrepl.server :as nrepl]
            [cider.nrepl :as cider]))

(def nrepl-port 8030)
(def http-port  8080)


(defn start-nrepl []
  (nrepl/start-server :port nrepl-port
                      :bind "127.0.0.1"
                      :handler cider/cider-nrepl-handler))

(defn start-webserver []
  (run-server secured-site {:port http-port
                            :ip "127.0.0.1"}))

(defn -main [& args]
  (try
    (println "Starting nrepl on port" nrepl-port)
    (start-nrepl)

    (println "Starting webserver on port" http-port)
    (start-webserver)

    (catch Throwable t
      (.printStackTrace t)
      (System/exit 1))))
