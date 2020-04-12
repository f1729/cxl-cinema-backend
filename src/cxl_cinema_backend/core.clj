(ns cxl-cinema-backend.core
  (:gen-class)
  (:require [org.httpkit.server :refer :all]
            [compojure.core :refer :all]
            [compojure.route :refer :all]
            [clojure.tools.logging :refer [info]]
            [clojure.data.json :refer [write-str read-str]]
            [clojure.string :as str]))


;; Clients

(def clients (atom {}))


(defn- now [] (quot (System/currentTimeMillis) 1000))

(let [max-id (atom 0)]
  (defn next-id []
    (swap! max-id inc)))

(defonce all-msgs (ref [{:id     (next-id), ; all message, in a list
                         :time   (now)
                         :msg    "this is a live chatroom, have fun",
                         :author "system"}]))

(defn add-message
  [data extra]
  (let [data (merge data {:time (now) :id (next-id) } extra)]
    (dosync
      (let [all-msgs* (conj @all-msgs data)
            total     (count all-msgs*)]
        (if (> total 100)
          (ref-set all-msgs (vec (drop (- total 100) all-msgs*)))
          (ref-set all-msgs all-msgs*))))))


(defn prepare-msg
  ([data] (merge data {:time (now) :id (next-id)}))
  ([data extras] (merge (prepare-msg data) extras)))

(defn mesg-received [msg]
  (let [data (read-str msg :key-fn keyword)]
    (info "mesg received" data)
    (if (:msg data)
      (doseq [client (keys @clients)]
        ;; send all, client will filter them
        (if-let [command-argument (re-find (re-pattern "^\\/(\\w*)\\s(.*)") (:msg data))]
          (send! client (write-str (prepare-msg data {:msg nil :command (nth command-argument 1) :argument (nth command-argument 2)})))
          (send! client (write-str (prepare-msg data))))))))



;;
;; (send! client (write-str @all-msgs))

;;(when (:msg data)
;;  (if-let [command-argument (re-find (re-pattern "^\\/(\\w*)\\s(.*)") (:msg data))]
;;    (add-message data {:msg nil :command (nth command-argument 1) :argument (nth command-argument 2)})
;;    (add-message data {})))

(defn chat-handler [req]
  (with-channel req channel
    (info channel "connected")
    (swap! clients assoc channel true)
    (on-receive channel #'mesg-received)
    (on-close channel (fn [status]
                        (swap! clients dissoc channel)
                        (info channel "closed, status" status)))))



(defroutes chartrootm
(GET "/ws" []  chat-handler)
(files "" {:root "static"})
(not-found "<p>Page not found.</p>" ))



(defn -main [& args]
  (run-server (-> #'chartrootm) {:port 80})
  (info "server started. http://127.0.0.1:9899"))
