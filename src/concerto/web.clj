(ns concerto.web
  (:require [clojure.tools.nrepl :as nrepl]
            [org.httpkit.server :as server]
            [cheshire.core :as cheshire]))

(def out *out*)

(defn eval-handler [eval-fn request]
  ;; for some reason the http-kit server doesn't support {:as :text}
  (binding [*out* out] (prn request))
  (if-let [body-obj (:body request)]
    (let [bytes (.bytes body-obj)
          charset (java.nio.charset.Charset/forName "utf8")
          body (String. bytes 0 (alength bytes) charset)]
      {:status 200 :body (cheshire/generate-string (eval-fn body))})
    {:status 200 :body (cheshire/generate-string (eval-fn))}))

(defn server []
  (let [conn (nrepl/connect :port 8003)
        client (nrepl/client conn 1000)
        session (nrepl/new-session client)
        eval-fn (fn
                  ([] (client))
                  ([code] (nrepl/message client {:op :eval :code code :session session})))
        server (server/run-server (partial eval-handler eval-fn) {:ip "localhost" :port 8004 :threads 1})]
    {:conn conn :client client :session session :server server}))

(comment
  (def s (server))
  ((:server s))
  (nrepl/message client {:op :join :username "B" :session session})
  (nrepl/message client {:op :eval :code "1" :session session}))
