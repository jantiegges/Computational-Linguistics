
;; Problem 1: define a function `absval` 
(defn absval [x] (Math/sqrt (* x x)))
(absval -3)

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x] (* x x))
(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))
(sum-of-squares 3 4)

;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(+ 4 4 5))
(def exp-13-2 '(+ 12 1))
(def exp-13-3 '(+ 1 1 1 1 1 1 1 1 1 1 1 1 1))
(def exp-13-4 '(- 13 0))
(+ 4 4 5)
(+ 12 1)
(+ 1 1 1 1 1 1 1 1 1 1 1 1 1)
(- 13 0)

;; Problem 4: define a function `third`
(defn third [x] (first (rest (rest x))))
(third [1 2 3 4 5])


;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
;; define a function `compose` 
(defn compose [f g] (fn [x] (f (g x))))
((compose sqrt abs) -36)

;; Problem 6: define a function `first-two`
(defn first-two [l] (take 2 l))
(first-two [1 2 3 4 5])

;; Problem 7: define a function `remove-second`
(defn remove-second [l] (concat (take 1 l) (drop 2 l)))
(remove-second [1 2 3 4 5])

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x] (concat lst (list x)))
(add-to-end (list 5 6 4) 0)

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function. 
;;            This is okay.)
(defn reverse-list [lst] (reduce conj '() lst))
(reverse-list [1 2 3 4])
;; allowed as higher-order function?

;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n] (reverse-list (take n (iterate inc 1))))
(take 3 (iterate inc 1))
(count-to-1 3)

;; Problem 11: define a function `count-to-n` in a different way
(defn count-to-n [n] (take n (iterate inc 1)))


;; Problem 12: define a function `get-max` without using max and reduce. do not use max and reduce. no reduce. no max.
(defn get-max [lst] (last (sort lst)))


;; Problem 13: define a function `greater-than-five?`
(> 6 5)
(defn greater-than-five? [lst] (map (fn [x] (> x 5)) lst))
(greater-than-five? (list 5 4 7))

;; Problem 14: define a function `concat-three`
(defn concat-three [x y z] (concat x y z))
(concat-three [1 2 3] [4 5 6] [7 8 9])

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n] (reduce concat '() (take n (iterate (fn [x] x) x))))
(sequence-to-power (list 'a 'b) 3)

;; Problem 16: define a function `in-L-star?`
(def L '(a))
(defn in-L-star? [lst] (= L lst))
(in-L-star? '(a))
;; what about empty sequence
;; is this too easy?