(ns concerto.core
  (:require [clojure.tools.nrepl :as c]
            [clojure.tools.nrepl.server :as s]
            [clojure.tools.nrepl.transport :as t]
            [clojure.tools.nrepl.middleware :as m]
            [clojure.tools.nrepl.middleware.session :as m.session]
            [clojure.tools.nrepl.misc :as misc]))

;; speak with a thousand voices

;; TODO
;; namespace the keys we use?
;; figure out whats going on with output printing
;; highlight user name, print join message

;; :username -> {:username :session :transport}
(def users (atom {}))

(defn get-user [username]
  (get @users username))

(defn lookup-user [key value]
  (some (fn [user]
          (when (= value (key user))
            user))
        (vals @users)))

(defn add-user! [user]
  (swap! users assoc (:username user) user))

(defn broadcast [username msg]
  (doseq [{:keys [transport session]} (vals @users)]
    ;; TODO seems to be a bug in nrepl with decoding :session nil
    (t/send transport {:username username
                       :broadcast msg})))

(defn join-middleware [handler]
  (fn [{:keys [op transport session username] :as msg}]
    (prn 'msg msg) ;; TODO log instead
    (if (not= "join" op)
      (handler msg)
      (do ;; TODO set ns to user.[username]
        (add-user! {:username username :session session :transport transport})
        (broadcast username {:joined session})
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
      (broadcast username (assoc msg :code code)))))

(defn broadcast-middleware [handler]
  (fn [{:keys [op transport session code] :as msg}]
    (if (= op "eval")
      (if-let [user (lookup-user :session session)]
        (handler (assoc msg :transport (->Eval code (:username user) transport)))
        (handler msg))
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
