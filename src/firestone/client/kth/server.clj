(ns firestone.client.kth.server
  (:require [org.httpkit.server :as httpkit]
            [firestone.client.kth.endpoints :refer [handle-request!]]))

(defonce server-atom (atom nil))

(defn start-server!
  []
  (reset! server-atom (httpkit/run-server
                        (fn [request]
                          (handle-request! request))
                        {:port 8001})))

(defn stop-server!
  []
  (let [server (deref server-atom)]
    (when server
      (server :timeout 100))))

(comment
  (start-server!)
  (stop-server!)
  )