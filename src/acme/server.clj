;;    (ns server
;;      (:require [org.httpkit.server :as http])
;;      (:require [logic :as logic])
;;      (:require [clojure.string :as str]))
;;    
;;    
;;    
;;    (defn parse-query [query-string]
;;      (if (str/blank? query-string)
;;        {}
;;        (->> (str/split query-string #"&")
;;             (map #(str/split % #"="))
;;             (into {} (map (fn [[k v]] [(keyword k) v]))))))
;;    
;;    ;; Це твій головний обробник (Handler).
;;    ;; Він приймає запит (req) і повертає мапу з відповіддю.
;;    (defn app [req]
;;      (let [method (:request-method req)
;;            path (:uri req)
;;            params (parse-query (:query-string req))]
;;        ;; Твій сервер надрукує в термінал всю мапу запиту
;;        (println "--- НОВИЙ ЗАПИТ ---")
;;        (println "Метод:" method)
;;        (println "Шлях:" path)
;;        (cond (and (= path "/state") (= method :get))
;;              (do (println "state 1")
;;                  ;; А це те, що піде назад клієнту
;;                  {:status 200
;;                   :headers {"Content-Type" "text/plain; charset=utf-8"}
;;                   :body (pr-str logic/state)})
;;              (and (= path "/moves") (= method :get))
;;              (do (println "moves 2")
;;                  (let [card (keyword (get params :card))
;;                        x (Integer/parseInt (get params :x))
;;                        y (Integer/parseInt (get params :y))]
;;                    (println card [x y])
;;                    {:status 200
;;                     :body (pr-str (logic/possible-moves card [x y] (:field logic/state)))}))
;;              :else {:status 200
;;                     :headers {"Content-Type" "text/plain; charset=utf-8"}
;;                     :body "нічого не просять - нічого не невиконано"})))
;;    
;;    ;; Функція запуску сервера
;;    (defonce server (atom nil))
;;    
;;    (defn stop-server []
;;      (when-not (nil? @server)
;;        (@server :timeout 100)
;;        (reset! server nil)))
;;    
;;    (defn start-server []
;;      (stop-server) ;; Зупиняємо старий, якщо він був
;;      (reset! server (http/run-server app {:port 8080})))
;;    
;;    ;; Запускаємо, тільки якщо ми в REPL
;;    (start-server)










;; 67 67 67 67 67 67 67 67 67 67 67 67 67 67 
 (ns server
   (:require [org.httpkit.server :as http])
   (:require [logic :as logic])
   (:require [clojure.edn :refer [read-string]])
   (:require [clojure.string :as str]))

;; (defonce state (atom nil))
;; 
;; (def mock-req-moves
;;   {:uri "/moves"
;;    :request-method :get
;;    :query-string "card=tiger&x=2&y=2"})
;; 
;; (def mock-req-action-move
;;   (-> mock-req-moves
;;       (assoc :query-string "card=tiger&x-from=2&y-from=2&x-to=2&y-to=0")
;;       (assoc :request-method :post)
;;       (assoc :uri "/action-move")))
;; 
;; (reset! state logic/state)
;; @state

(defonce rooms (atom {})) ;; rooms list - {123 {:state {...} :missing-player nil :turn :player1}}

(defonce channel-hub (atom #{})) ;; channel list - players' connection to the server for long-polling (processing opponent actions)

(defn create-room ([room-id]
                   (swap! rooms assoc room-id
                          (if (@rooms room-id)
                            (@rooms room-id)
                            {:state logic/state
                             :missing-player :player2})))
  ([] (let [random-rooms (iterate (partial (fn [f _] (f))) (+ (rand-int 900) 100)) ;; do not remove, iterate + partial combo is cool
            fitting-random-rooms (filter #(not (contains? @rooms %)) random-rooms)
            first-fitting-room (first fitting-random-rooms)]
        (create-room first-fitting-room))))

(defn join-room [room-id]
  (let [room (@rooms room-id)
        missing-player (if room (:missing-player room) nil)
        joined-room (if missing-player (assoc (@rooms room-id) :missing-player nil) nil)]
    (if missing-player (do
                         (swap! rooms assoc room-id joined-room)
                         [(@rooms room-id) missing-player])
        nil)))

;; tests
;; @rooms
;; (reset! rooms {})
;; (create-room 124)
;; (@rooms 123)
;; (create-room)
;; (join-room 124)


(defn parse-query [query-string]
  (if (str/blank? query-string)
    {}
    (->> (str/split query-string #"&")
         (map #(str/split % #"="))
         (into {} (map (fn [[k v]] [(keyword k) v]))))))

(defn coordinates [vec x y]
  [(Integer/parseInt (x vec))
   (Integer/parseInt (y vec))])

;; (comment (parse-query "x=2&y=2&card=tiger"))
;; (comment (coordinates (parse-query "x=2&y=2&card=") :x :y))
;; (comment (keyword "tiger"))

(defn look-moves [rooms room-id params]
  (let [params-set (parse-query params)
        [x y] (coordinates params-set :x :y)
        card (keyword (:card params-set))
        player (keyword (:player params-set))
        moves (logic/possible-moves ((get rooms room-id) :state) card [x y] player)]
    (println moves)
    {:status  200
     :headers {"Content-Type" "text/html"}
     :body    (pr-str moves)}))


(defn make-move [rooms room-id params]
  (let [params-set (parse-query params)
        [x-from y-from] (coordinates params-set :x-from :y-from)
        [x-to y-to] (coordinates params-set :x-to :y-to)
        card (keyword (:card params-set))
        player (keyword (:player params-set))
        state ((rooms room-id) :state)
        new-state (logic/action-move state player card [x-from y-from] [x-to y-to])]
    (println new-state)
    [new-state player]))

(defn app [req]
  (let [method (:request-method req)
        params (:query-string req)
        path (:uri req)
        room-id (:room-id (parse-query params))]
    (println req)
    ;; (reset! state logic/state)
    (cond
      (and (= path "/subscribe") (= method :get))
      (http/with-channel req channel
        (swap! channel-hub conj channel)
        (http/on-close channel (fn [_] (swap! channel-hub disj channel)))
        (println "channel:" channel))
      (and (= path "/moves") (= method :get))
      (look-moves @rooms room-id params)
      (and (= path "/create-room") (= method :post))
      (let [created-room (if room-id
                           (create-room room-id)
                           (create-room))]
        (println created-room)
        {:status  200
         :headers {"Content-Type" "application/edn"}
         :body    (pr-str {:player (if created-room :player1 nil) :room created-room})})
      (and (= path "/join-room") (= method :post))
      (let [set-params (parse-query params)
            room-id? (:room-id set-params)
            [joined-room player-joined] (join-room room-id?)]
        (println joined-room)
        (doseq [channel @channel-hub]
          (http/send! channel {:status 200 :headers {"Content-Type" "application/edn"} :body (pr-str [(:state joined-room) :player2])}))
        {:status  200
         :headers {"Content-Type" "application/edn"}
         :body    (pr-str {:player (if joined-room player-joined nil) :room joined-room})})
      (and (= path "/action-move") (= method :post))
      (let [[new-state player] (make-move @rooms room-id params)
            winner (logic/game-over? new-state)]
        (swap! rooms assoc-in [room-id :state] new-state)
        (doseq [channel @channel-hub]
          (http/send! channel {:status 200 :headers {"Content-Type" "application/edn"} :body (pr-str [new-state player])}))
        {:status  200
         :headers {"Content-Type" "application/edn"}
         :body    (pr-str [new-state player])})
      :else
      {:status  404
       :headers {"Content-Type" "text/html"}
       :body    "ойойой!"})))



;; (comment (app mock-req-moves))
;; (comment (app mock-req-action-move))

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))



(defn -main [& args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and https://http-kit.github.io/migration.html#reload
  (reset! server (http/run-server #'app {:port 8080})))

(-main :kys)