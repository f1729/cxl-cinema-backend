(ns cxl-cinema-backend.core
  (:gen-class)
  (:require [org.httpkit.server :refer :all]
            [compojure.core :refer :all]
            [compojure.route :refer :all]
            [ring.middleware.params :refer [wrap-params]]
            [clojure.tools.logging :refer [info]]
            [clojure.data.json :refer [write-str read-str]]
            [clojure.string :as str]
            [environ.core :refer [env]]))

;; Utils

(defn- now [] (quot (System/currentTimeMillis) 1000))

(defn get-command
  [msg]
  (re-find (re-pattern "^\\/(\\w*)\\s(.*)") msg))


;; ATOMS 

(def clients (atom {}))
(def rooms (atom {}))


(let [max-id (atom 0)]
  (defn next-id []
    (swap! max-id inc)))

(defonce all-msgs (ref [{:id     (next-id), ; all message, in a list
                         :time   (now)
                         :msg    "this is a live chatroom, have fun",
                         :author "system"}]))

;; MESSAGING HANDLER

(defn prepare-msg
  ([data] (merge data {:time (now) :id (next-id)}))
  ([data extras] (merge (prepare-msg data) extras)))

(defn get-client-by-room [room]
  (let [clients (get @rooms room)] clients))


(defn extra-for-command
  [command-argument]
  {:msg nil :command (nth command-argument 1) :argument (nth command-argument 2)})

(defn send-message-to-client
  [client data]
  (println client data)
  (if-let [command-argument (get-command (:msg data))]
    (send! client (write-str (prepare-msg data (extra-for-command command-argument))))
    (send! client (write-str (prepare-msg data)))))

(defn mesg-received [msg]
  (let [data (read-str msg :key-fn keyword)]
    (info "mesg received" data)
    (when-let [room (:room data)]
      (doseq [client (keys (get-client-by-room room))] 
        (send-message-to-client client data)))))


;; CLIENT LOGIN HANDLER

(defn get-room-header
  [req]
  (get (:params req) "room"))

(defn get-room
  [room]
  (get @rooms room))

(defn add-client-room
  [client room]
  (swap! rooms assoc room (merge (get-room room) {client true})))

(defn delete-client-room
  [client room]
  (swap! rooms assoc room (dissoc (get-room room) client)))

(defn chat-handler [req]
  (let [room (get-room-header req)]
    (with-channel req channel
      (info "::: CLIENT IN ROOM :::" channel)
      (add-client-room channel room)
      (on-receive channel #'mesg-received)
      (on-close channel (fn [status]
                          (delete-client-room channel room)
                          (info channel "closed, status" status))))))


;; ROUTES

(defroutes chartrootm
  (GET "/ws" [] chat-handler)
  (files "" {:root "static"})
  (not-found "<p>Page not found.</p>"))


(defn -main [& [port]]
  (run-server (-> #'chartrootm wrap-params) {:port (Integer. (or (env :port) 9899))})
  (info "server started. http://127.0.0.1:9899"))
