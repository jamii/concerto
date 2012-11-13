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
;;   fix seems to re-introduced tooling output on same session :(
;;   at some point we broke nrepl-error too :(
;; highlight user name, print join message

;; :session -> {:username :session :transport}
(def users (atom {}))

(defn session->user [session]
  (get @users session))

(defn add-user! [user]
  (swap! users assoc (:session user) user))

(defn broadcast [session msg]
  (when-let [user (session->user session)]
    (doseq [{:keys [transport session]} (vals @users)]
      (t/send transport {:session session
                         :username (:username user)
                         :broadcast msg}))))

(defn join-middleware [handler]
  (fn [{:keys [op transport session username] :as msg}]
    (prn 'msg msg) ;; TODO log instead
    (cond
     (not= "join" op) (handler msg)
     (nil? session) (t/send transport (misc/response-for msg :status [:err :no-session]))
     :else (do (add-user! {:username username :session session :transport transport})
               (broadcast session {:joined session})
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

(defn eval-middleware [handler]
  (fn [{:keys [op session code ns] :as msg}]
    (when (= "eval" op)
      (broadcast session {:code code :ns ns}))
    (handler msg)))

(m/set-descriptor! #'eval-middleware
                   {:requires #{}
                    :expects #{"eval"}
                    :handles {}})

(defrecord Broadcast [transport]
  clojure.tools.nrepl.transport.Transport
  (recv [this timeout]
    (t/recv transport timeout))
  (send [this msg]
    (t/send transport msg)
    (when-let [session (:session msg)]
      (broadcast session msg))))

(defn broadcast-middleware [handler]
  (fn [msg] (handler (update-in msg [:transport] ->Broadcast))))

(m/set-descriptor! #'broadcast-middleware
                   {:requires #{}
                    :expects #{}
                    :handles {}})

(def default-handler [& other-handlers]
  (-> (s/default-handler other-handlers)
      broadcast-middleware
      eval-middleware
      join-middleware))

(defn server [& other-handlers]
  (s/start-server :port 8003 :handler (apply default-handler other-handlers)))

(comment
  ;; for debugging :)

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
