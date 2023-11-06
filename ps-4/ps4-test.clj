(comment "Make sure that you follow the instructions carefully
          and use the right procedure names, inputs, and outputs")

;;Preliminaries
(def vocabulary '(call me ishmael))
(def theta1 (list (/ 1 2 ) (/ 1 4 ) (/ 1 4 )))
(def theta2 (list (/ 1 4 ) (/ 1 2 ) (/ 1 4 )))
(def thetas (list theta1 theta2))
(def theta-prior (list (/ 1 2) (/ 1 2)))

(defn score-categorical [outcome outcomes params]
  (if (empty? params)
    (print "no matching outcome")
    (if (= outcome (first outcomes))
      (first params)
      (score-categorical outcome (rest outcomes) (rest params)))))

(defn list-foldr [f base lst]
  (if (empty? lst)
    base
    (f (first lst)
      (list-foldr f base (rest lst)))))

(defn log2 [n]
  (/ (Math/log n) (Math/log 2)))

(defn score-BOW-sentence [sen probabilities]
  (list-foldr
    (fn [word rest-score]
      (+ (log2 (score-categorical word vocabulary probabilities))
        rest-score))
    0
    sen))

(defn score-corpus [corpus probabilities]
  (list-foldr
    (fn [sen rst]
      (+ (score-BOW-sentence sen probabilities) rst))
    0
    corpus))

(defn logsumexp [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
      (log2
        (apply +
          (map (fn [z] (Math/pow 2 z))
            (map (fn [x] (- x mx)) log-vals)))))))

(def my-corpus '((call me)
        (call ishmael)))

;;Problem 1: define `theta-corpus-joint`
(defn theta-corpus-joint [theta corpus theta-probs]
  (let [theta-log-prob (log2 (score-categorical theta thetas theta-probs))
        corpus-log-prob (score-corpus corpus theta)]
    (+ theta-log-prob corpus-log-prob)))
(theta-corpus-joint theta1 my-corpus theta-prior)


;;Problem 2: define `compute-marginal`
(defn compute-marginal [corpus theta-probs]
  (let [theta-corpus-joints (map (fn [theta] (theta-corpus-joint theta corpus theta-probs)) thetas)]
    (logsumexp theta-corpus-joints))
  )
(compute-marginal my-corpus theta-prior)


;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
  (let [log-numerator (theta-corpus-joint theta corpus theta-probs)
        log-denominator (compute-marginal corpus theta-probs)]
    (- log-numerator log-denominator)))
(compute-conditional-prob theta1 my-corpus theta-prior)


;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
  (map (fn [theta] (compute-conditional-prob theta corpus theta-probs)) thetas))
(compute-conditional-dist my-corpus theta-prior)
;; compute values bag to normal probabilities
(map (fn [x] (Math/pow 2 x)) (compute-conditional-dist my-corpus theta-prior))


;;Problem 6: define `compute-posterior-predictive`

; (defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
;   (let [conditional-dist ...
;     (compute-marginal ...


;Problem 7: define `sample-BOW-corpus`
(defn normalize [params]
  (let [sum (apply + params)]
    (map (fn [x] (/ x sum)) params)))

(defn flip [weight]
  (if (< (rand 1) weight)
      true
      false))

(defn sample-categorical [outcomes params]
  (if (flip (first params))
      (first outcomes)
      (sample-categorical (rest outcomes)
                          (normalize (rest params)))))

(defn sample-BOW-sentence [len probabilities]
    (if (= len 0)
      '()
      (cons (sample-categorical vocabulary probabilities)
        (sample-BOW-sentence (- len 1) probabilities))))


;;Problem 8
; (defn sample-theta-corpus [sent-len corpus-len theta-probs]
;   (let [theta ...
;     (list theta ...


;;Problem 9: define `estimate-corpus-marginal`
(defn get-theta [theta-corpus]
  (first theta-corpus))

(defn get-corpus [theta-corpus]
  (first (rest theta-corpus)))

(defn get-count [outcome lst]
  (let [filtered-lst 
        (filter (fn [x] (= outcome x)) lst)]
  (count filtered-lst)))

; ;uncomment the following after you have defined `sample-theta-corpus` above
; (defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
;   (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))


;;Problem 11: define `rejection-sampler`

; (defn rejection-sampler
;   [theta observed-corpus sample-size sent-len corpus-len theta-probs]
;   ...)
