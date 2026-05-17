(ns logic)

(def card-moves (read-string (slurp "cards.edn")))
(def battlefield
  [[:player2 :player2 :player2 :opportunity :player2]
   [:opportunity :player2 :opportunity :opportunity :opportunity]
   [:opportunity :opportunity :player1 :opportunity :opportunity]
   [:opportunity :opportunity :opportunity :player1 :opportunity]
   [:player1 :player1 :player1 :opportunity :opportunity]])

(def win-pos {:player1 [2 0] :player2 [2 4]})

(def state {:field battlefield
            :kings {:player1 [2 4] :player2 [2 0]}
            :cards {:player1 #{:tiger :otter}
                    :player2 #{:bear :tanuki}
                    :neutral :dragon}})
(def selected-tile [2 2])

(defn game-over? [state] ;; returns the winner if gamestate tells the game is over, nil otherwise
  (let [king1 ((state :kings) :player1)
        king2 ((state :kings) :player2)
        king-alive? (fn [king player] (= (-> state
                                             :field
                                             (get-in [(second king) (first king)])) player))
        king1-alive? (king-alive? king1 :player1)
        king2-alive? (king-alive? king2 :player2)
        king1-won? (= (win-pos :player1) king1)
        king2-won? (= (win-pos :player2) king2)]
    (cond (or (not king2-alive?) king1-won?) :player1
          (or (not king1-alive?) king2-won?) :player2
          :else nil)))

(defn reverse-battlefield [battlefield]
  (vector (map
           (partial replace {:player1 :player2 :player2 :player1})
           (map rseq (rseq battlefield)))))

(defn possible-moves ([state card position player]
                      (let [[x y] position
                            battlefield (state :field)
                            moves (card card-moves)
                            selected-piece (get-in battlefield [y x])
                            operator (if (= selected-piece player) + -)
                            result (map (fn [[dx dy]] [(operator x dx) (operator y dy)]) moves)]
                        (when operator
                          (filter #(and (<= 0 (first %) 4)
                                        (<= 0 (second %) 4)
                                        (not= selected-piece (get-in battlefield [(second %) (first %)])))
                                  result))))
  ([state card [x y]]
   (possible-moves state card [x y] (get-in (state :field) [y x]))))

(comment (possible-moves {:field [[:player2 :player2 :opportunity :opportunity :player2]
                                  [:opportunity :player2 :player1 :opportunity :opportunity]
                                  [:opportunity :opportunity :opportunity :opportunity :opportunity]
                                  [:opportunity :opportunity :opportunity :player1 :opportunity]
                                  [:player1 :player1 :player1 :opportunity :opportunity]],
                          :player :player1,
                          :kings {:player1 [2 4], :player2 [2 0]},
                          :cards {:player1 #{:dragon :tiger},
                                  :player2 #{:bear :tanuki},
                                  :neutral :otter}} :tiger [1 1]))

(comment (let [[x y] [1 1]
               battlefield [[:player2 :player2 :opportunity :opportunity :player2]
                            [:opportunity :player2 :player1 :opportunity :opportunity]
                            [:opportunity :opportunity :opportunity :opportunity :opportunity]
                            [:opportunity :opportunity :opportunity :player1 :opportunity]
                            [:player1 :player1 :player1 :opportunity :opportunity]]
               moves (:tiger card-moves)
               selected-piece (get-in battlefield [y x])
               operator (if (= selected-piece :player2) + -)
               result (map (fn [[dx dy]] [(operator x dx) (operator y dy)]) moves)]
           (when operator
             (filter #(and (<= 0 (first %) 4)
                           (<= 0 (second %) 4)
                           (not= selected-piece (get-in battlefield [(second %) (first %)])))
                     result))))
(comment (get-in [[:player2 :player2 :opportunity :opportunity :player2]
                  [:opportunity :player2 :opportunity :player1 :opportunity]
                  [:opportunity :opportunity :opportunity :opportunity :opportunity]
                  [:opportunity :opportunity :opportunity :player1 :opportunity]
                  [:player1 :player1 :player1 :opportunity :opportunity]]
                 [1 3]))

(comment (pr-str :player1))


(comment battlefield)
(comment (possible-moves state (second (:player2 (:cards state))) [2 0] :player1))

(defn battlefield-change [battlefield [x-from y-from] [x-to y-to] player]
  (assoc-in (assoc-in battlefield [y-from x-from] :opportunity) [y-to x-to] player))

(defn other-player [player] (if (= :player1 player)  :player2 :player1))

(defn valid-move? [state player card piece move-to]
  (let [battlefield (:field state)
        [x y] piece]
    ;;(println "player = piece? " (= player (get-in battlefield [y x])))
    ;;(println "card = yes? " (= player (get-in battlefield [y x])))
    ;;(println "move = possible? " (= player (get-in battlefield [y x])))
    (and (= player (get-in battlefield [y x]))                     ;; Це фігура гравця?
         (contains? (get-in state [:cards player]) card)           ;; Карта є в руці?
         (contains? (set (possible-moves state card piece)) move-to))))

(defn apply-move [state player card piece move-to]
  (let [card-neutral (:neutral (:cards state))
        king ((state :kings) player)
        king? (= king piece)
        new-king (if king? move-to king)]
    (-> state
        (update-in [:field] #(battlefield-change % piece move-to player))
        (update-in [:cards player] #(-> %
                                        (disj card)
                                        (conj card-neutral)))
        (assoc-in [:kings player] new-king)
        (assoc-in [:cards :neutral] card))))

(defn action-move [state player card-played piece move-to]
  (if (valid-move? state player card-played piece move-to)
    (apply-move state player card-played piece move-to)
    state))

(comment (action-move state :player1 :tiger [2 2] [2 0]))

(comment (-> state
             :cards
             :player1
             (disj :tiger)
             (conj :dragon)))
(comment ((set (:player1 (:cards state))) :tiger))

(map (partial replace {0 1 1 0}) [[1 0] [1 0]])

(comment battlefield)
(comment (action-move (:neutral (:cards state)) (first (:player1 (:cards state))) [2 0] [2 2] battlefield))


(map + [1 2 3] (iterate (partial + 0) 5))

;; [[1 1]  [2 0]  [-1 -1]]
;; [[1 2 -1]  [1 0 -1]]
;; map (+ 5) [1 2 3]

(iterate inc 1)
(iterate (partial + 0) 5)

(defn f [x] (* x x))
(comment (apply < (take 1000 (map f (iterate (partial + 1/100) -1)))))
(f -1/2)