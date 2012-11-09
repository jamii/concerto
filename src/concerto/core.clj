(ns concerto.core
  (:require [clojure.tools.nrepl :as c]
            [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.middleware :as m]
            [clojure.tools.nrepl.middleware.session :as m.session]
            [clojure.tools.nrepl.misc :as misc]))

; speak with a thousand voices

; :username -> {:username :session :transport}
(def users (atom {}))

(defn get-user [username]
  (get @users username))

(defn lookup-user [key value]
  (some #(= value (key %)) (vals @users)))

(defn add-user! [user]
  (swap! users assoc (:username user) user))

(defn broadcast [from-username text]
  (prn 'broadcasting from-username text)
  (doseq [[username {:keys [transport session]}] @users]
    (when (not= from-username username)
      (t/send transport {:session session
                         :broadcast {:from from-username
                                     :text text}}))))

(defn join-middleware [handler]
  (fn [{:keys [op transport session username] :as msg}]
    (prn 'msg msg) ;; TODO remove
    (if (not= "join" op)
      (handler msg)
      (do ;; TODO set ns to user.[username]
        (add-user! {:username username :session session :transport transport})
        (broadcast username (str username " joined!"))
        (t/send transport (misc/response-for msg :status :done))))))

(m/set-descriptor! #'join-middleware
                   {:requires #{#'m.session/session}
                    :expects #{}
                    :handles {"join"
                              {:doc "Join a concerto repl."
                               :requires {"username" "The name of the user joining"
                                          "session" "The session to which we should attach this username"}
                               :optional {}
                               :returns {"status" "Done"}}}})

(defrecord Eval [code username transport]
  clojure.tools.nrepl.transport.Transport
  (recv [this timeout]
    (t/recv transport timeout))
  (send [this msg]
    (t/send transport msg)
    (when (contains? msg :value) ; is this the result of the eval?
      (broadcast username
                 (str username " " (:ns msg) "> "
                      code "\n"
                      (:out msg)
                      (:value msg))))))

(defn broadcast-middleware [handler]
  (fn [{:keys [op transport session code] :as msg}]
    (if (= "eval" op)
      (let [username (if-let [user (lookup-user :session session)]
                       (:username user)
                       "unknown user")]
        (handler (assoc msg :transport (->Eval code (str username " <" session ">") transport))))
      (handler msg))))

(m/set-descriptor! #'broadcast-middleware
                   {:requires #{#'m.session/session}
                    :expects #{"eval"}
                    :handles {}})

(def default-handler (s/default-handler broadcast-middleware join-middleware))

(defonce server
  (s/start-server :port 8003 :handler default-handler))

(comment
  ;; in server repl
  (use 'concerto.core)

  ;; in client repl A
  (require '[clojure.tools.nrepl :as nrepl])
  (def conn (nrepl/connect :port 8003))
  (def client (nrepl/client conn 1000))
  (def session (nrepl/new-session client))
  (nrepl/message client {:op :join :username "A" :session session})

  ;; in client repl B
  (require '[clojure.tools.nrepl :as nrepl])
  (def conn (nrepl/connect :port 8003))
  (def client (nrepl/client conn 1000))
  (def session (nrepl/new-session client))
  (nrepl/message client {:op :join :username "B" :session session})
  (nrepl/message client {:op :eval :code "1" :session session})
  )
