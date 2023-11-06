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
;;(theta-corpus-joint theta1 my-corpus theta-prior)


;;Problem 2: define `compute-marginal`
(defn compute-marginal [corpus theta-probs]
  (let [theta-corpus-joints (map (fn [theta] (theta-corpus-joint theta corpus theta-probs)) thetas)]
    (logsumexp theta-corpus-joints))
  )
(compute-marginal my-corpus theta-prior)
;;(Math/pow 2 (compute-marginal my-corpus theta-prior))


;;Problem 3: define `compute-conditional-prob`
(defn compute-conditional-prob [theta corpus theta-probs]
  (let [log-numerator (theta-corpus-joint theta corpus theta-probs)
        log-denominator (compute-marginal corpus theta-probs)]
    (- log-numerator log-denominator)))
;;(compute-conditional-prob theta1 my-corpus theta-prior)


;;Problem 4: define `compute-conditional-dist`
(defn compute-conditional-dist [corpus theta-probs]
  (map (fn [theta] (compute-conditional-prob theta corpus theta-probs)) thetas))

;;Problem 5:
;;(compute-conditional-dist my-corpus theta-prior)
(map (fn [x] (Math/pow 2 x)) (compute-conditional-dist my-corpus theta-prior))


;;Problem 6: define `compute-posterior-predictive`
(defn compute-posterior-predictive [observed-corpus new-corpus theta-probs]
   (let [log-conditional-dist (compute-conditional-dist observed-corpus theta-probs)
         conditional-dist (map (fn [x] (Math/pow 2 x)) log-conditional-dist)]
     (compute-marginal new-corpus conditional-dist)))
(compute-posterior-predictive my-corpus my-corpus theta-prior)


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

(defn sample-BOW-corpus [theta, sent-len, corpus-len]
  (if (= corpus-len 0)
    '()
    (cons (sample-BOW-sentence sent-len theta)
      (sample-BOW-corpus theta sent-len (- corpus-len 1)))))
;;(sample-BOW-corpus theta1 3 2)

;;Problem 8
(defn sample-theta-corpus [sent-len corpus-len theta-probs]
   (let [theta (sample-categorical thetas theta-probs)
         corpus (sample-BOW-corpus theta sent-len corpus-len)]
     (list theta corpus)))
;;(sample-theta-corpus 3 2 theta-prior)

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
(defn sample-thetas-corpora [sample-size sent-len corpus-len theta-probs]
  (repeatedly sample-size (fn [] (sample-theta-corpus sent-len corpus-len theta-probs))))

(defn estimate-corpus-marginal [corpus sample-size sent-len corpus-len theta-probs]
  (let [thetas-corpora (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)
        corpora (map get-corpus thetas-corpora)]
    (/ (get-count corpus corpora) sample-size)))
;;(estimate-corpus-marginal my-corpus 100 2 2 theta-prior)

;;Problem 10:
(defn mean [lst]
  (/ (apply + lst) (count lst)))
(defn variance [lst]
  (let [mean (mean lst)]
    (/ (apply + (map (fn [x] (Math/pow (- x mean) 2)) lst)) (count lst))))
(defn estimate-corpus-marginal-mean-var [corpus sample-size sent-len corpus-len theta-probs]
  (let [estimates (repeatedly 100 (fn [] (estimate-corpus-marginal corpus sample-size sent-len corpus-len theta-probs)))]
    (list (mean estimates) (variance estimates))))
(estimate-corpus-marginal-mean-var my-corpus 50 2 2 theta-prior)
(estimate-corpus-marginal-mean-var my-corpus 10000 2 2 theta-prior)

;;Problem 11: define `rejection-sampler`
(defn rejection-sampler
  [theta observed-corpus sample-size sent-len corpus-len theta-probs]
  (let [thetas-corpora (sample-thetas-corpora sample-size sent-len corpus-len theta-probs)
        matching-corpora (filter (fn [tc] (= (get-corpus tc) observed-corpus)) thetas-corpora)
        count-theta (get-count theta (map get-theta matching-corpora))
        total-matching (count matching-corpora)]
    (if (zero? total-matching)
      nil
      (/ count-theta total-matching))))
;;(rejection-sampler theta1 my-corpus 100 2 2 theta-prior)

;;Problem 12: define `rejection-sampler-mean-var`
(defn rejection-sampler-mean-var
  [theta observed-corpus sample-size sent-len corpus-len theta-probs]
  (let [estimates (repeatedly 100 (fn [] (rejection-sampler theta observed-corpus sample-size sent-len corpus-len theta-probs)))
        estimates (filter (fn [x] (not (nil? x))) estimates)] 
    (list (mean estimates) (variance estimates))))
(rejection-sampler-mean-var theta1 my-corpus 100 2 2 theta-prior)
(rejection-sampler-mean-var theta1 my-corpus 200 2 2 theta-prior)
(rejection-sampler-mean-var theta1 my-corpus 500 2 2 theta-prior)
(rejection-sampler-mean-var theta1 my-corpus 1000 2 2 theta-prior)