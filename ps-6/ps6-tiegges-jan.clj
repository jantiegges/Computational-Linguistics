;; variables and functions defined in the background section:

(def hidden-states '(Start N V))

(def vocabulary '(Call me Ishmael))

(def theta-transition-Start '(0 0.9 0.1))
(def theta-transition-N '(0 0.3 0.7))
(def theta-transition-V '(0 0.8 0.2))

(def theta-transition-dists-1
    (list theta-transition-Start 
          theta-transition-N 
          theta-transition-V))

(def theta-observation-Start '(0.0 0.0 0.0))
(def theta-observation-N '(0.1 0.5 0.4))
(def theta-observation-V '(0.8 0.1 0.1))

(def theta-observation-dists-1
    (list theta-observation-Start
          theta-observation-N
          theta-observation-V))

;; helper functions

(defn dist-lookup
    "gets the distribution over states for a given state 
     from the list of dists"
    [state states dists]
    (if (= state (first states))
        (first dists)
        (dist-lookup state (rest states) (rest dists))))

(defn log2 [n] (/ (Math/log n) (Math/log 2)))

(defn logsumexp 
  "given a list of log-vals: exponentiates, sums, and re-logs them"
  [log-vals]
  (let [mx (apply max log-vals)]
    (+ mx
       (log2
        (apply +
               (map (fn [z] (Math/pow 2 z))
                    (map (fn [x] (- x mx))
                         log-vals)))))))

(defn logscore-categorical
    "log probability of word 'outcome' given the vocabulary 'outcomes'
    and the list of categorical outcome probabilities 'params'"
    [outcome outcomes params]
    (if (= outcome (first outcomes))
        (log2 (first params))
        (logscore-categorical outcome (rest outcomes) (rest params))))

;; Problem 1  - score-next-state-word

(defn score-next-state-word
  "compute log probability of going from 'curr-state' to
   'next-state' and emitting 'next-word'"
  [curr-state next-state next-word t-dists o-dists]
  (let [trans-dist (dist-lookup curr-state hidden-states t-dists)
        obs-dist (dist-lookup next-state hidden-states o-dists)
        log-trans-prob (logscore-categorical next-state hidden-states trans-dist)
        log-obs-prob (logscore-categorical next-word vocabulary obs-dist)] 
    (+ log-trans-prob log-obs-prob)))
;; (score-next-state-word 'Start 'N 'Call theta-transition-dists-1 theta-observation-dists-1)

;; Problem 2  - compute-next-observation-marginal

(defn compute-next-observation-marginal
  "get log marginal probability of next word being 
   'next-observation' given 'curr-state'"
  [curr-state next-observation t-dists o-dists]
  (logsumexp (map (fn [next-state]
                    (score-next-state-word curr-state next-state next-observation t-dists o-dists))
                  hidden-states)))
;; (compute-next-observation-marginal 'Start 'Call theta-transition-dists-1 theta-observation-dists-1)

;; Problem 3  - score-next-states-words

(defn score-next-states-words
  "compute log prob of going from 'curr-state'
     to sequence of states 'next-states'
     and emitting sequence of words 'next-words'"
  [curr-state next-states next-words t-dists o-dists]
  (if (empty? next-states)
    0
    (let [next-state (first next-states)
          next-word (first next-words)
          log-prob (score-next-state-word curr-state next-state next-word t-dists o-dists)]
      (+ log-prob
         (score-next-states-words next-state (rest next-states) (rest next-words) t-dists o-dists)))))
;; (score-next-states-words 'Start '(N V) '(Call me) theta-transition-dists-1 theta-observation-dists-1)

;; Problem 4  - compute-next-words-marginal

(defn compute-next-words-marginal
  "get the log marginal prob of sequence 'next-words' given 'curr-state'"
  [curr-state next-words t-dists o-dists]
  (if (empty? next-words)
    0
    (logsumexp (map (fn [next-state]
                      (+ (score-next-state-word curr-state next-state (first next-words) t-dists o-dists)
                         (compute-next-words-marginal next-state (rest next-words) t-dists o-dists)))
                    hidden-states))))
;; (compute-next-words-marginal 'Start '(Call me) theta-transition-dists-1 theta-observation-dists-1)

;; Problem 5
;; (compute-next-words-marginal 'Start '(Call me) theta-transition-dists-1 theta-observation-dists-1)
;; (compute-next-words-marginal 'Start '(me Call) theta-transition-dists-1 theta-observation-dists-1)

;; Problem 6  - compute-hidden-prior

(defn compute-hidden-prior
  "get log prior probability 'list-of-states' given transition 
   distributions 't-dists'"
  [list-of-states t-dists]
  (if (or (empty? list-of-states) (empty? (rest list-of-states)))
    0
    (let [curr-state (first list-of-states)
          next-state (second list-of-states)
          trans-dist (dist-lookup curr-state hidden-states t-dists)
          log-trans-prob (logscore-categorical next-state hidden-states trans-dist)]
      (+ log-trans-prob
         (compute-hidden-prior (rest list-of-states) t-dists)))))
;; (compute-hidden-prior '(Start N V) theta-transition-dists-1)

;; Problem 7  - compute-likelihood-of-words

(defn compute-likelihood-of-words
  "Likelihood (of hidden states) = conditional probability
   of the words in the list-of-words given the list-of-states"
  [list-of-states list-of-words o-dists]
  (if (or (empty? list-of-states) (empty? list-of-words))
    0
    (if (= (first list-of-states) 'Start)
      (compute-likelihood-of-words (rest list-of-states) list-of-words o-dists)
      (let [curr-state (first list-of-states)
            curr-word (first list-of-words)
            obs-dist (dist-lookup curr-state hidden-states o-dists)
            log-obs-prob (logscore-categorical curr-word vocabulary obs-dist)]
        (+ log-obs-prob
           (compute-likelihood-of-words (rest list-of-states) (rest list-of-words) o-dists))))))
;; (compute-likelihood-of-words '(Start N V) '(Call me) theta-observation-dists-1)

;; Problem 8  - compute-hidden-posterior

(defn compute-hidden-posterior
  "computes log posterior probability of the 'list-of-states' 
   given the observed 'list-of-words'"
  [list-of-states list-of-words t-dists o-dists]
  (let [list-of-states-with-start (cons 'Start list-of-states)
        log-prior (compute-hidden-prior list-of-states-with-start t-dists)
        log-likelihood (compute-likelihood-of-words (rest list-of-states-with-start) list-of-words o-dists)
        log-marginal (compute-next-words-marginal 'Start list-of-words t-dists o-dists)]
    (- (+ log-prior log-likelihood) log-marginal)))
;; (compute-hidden-posterior '(N V) '(Call me) theta-transition-dists-1 theta-observation-dists-1)

;; Problem 10 - compute-next-words-marginal-mem

(def compute-next-words-marginal-mem
  "(like compute-next-words-marginal, but with memoization)
  gets the log marginal prob of sequence 'next-words' given 'curr-state'"
  (memoize compute-next-words-marginal))
;; (compute-next-words-marginal-mem 'Start '(Call me) theta-transition-dists-1 theta-observation-dists-1)
