;; Problem 1: define a function `absval` 
(defn absval [x] (Math/sqrt (* x x)))
(absval -3)

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x] (* x x))
(defn sum-of-squares [x y]
  (+ (take-square x) (take-square y)))
(sum-of-squares 3 4)

;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(- 13 0))
(def exp-13-2 '(* 2 6.5))
(def exp-13-3 '(/ 26 2))
(def exp-13-4 '(Math/sqrt 169))
(eval exp-13-1)
(eval exp-13-2)
(eval exp-13-3)
(eval exp-13-4)

;; Problem 4: define a function `third`
(defn third [x] (first (rest (rest x))))
(third [1 2])


;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
;; define a function `compose` 
(defn compose [f g] (fn [x] (f (g x))))
((compose sqrt abs) -36)

;; Problem 6: define a function `first-two`
(defn first-two [l] (list (first l) (first (rest l))))
(first-two [1 2 3])

;; Problem 7: define a function `remove-second`
(defn remove-second [l] (cons (first l) (rest (rest l))))
(remove-second [1 2 3 4 5])

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x]
  (if (empty? lst)
    (list x)
    (cons (first lst) (add-to-end (rest lst) x))))
(add-to-end (list 5 6 4) 0)

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function. 
;;            This is okay.)
(defn reverse-list [lst] 
  (if (empty? lst)
    '()
    (concat (reverse-list (rest lst)) (list (first lst)))))
(reverse-list [1 2 3 4])

;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n] 
  (if (<= n 1) 
     (list 1) 
     (cons n (count-to-1 (- n 1))     
)))
(count-to-1 1)

;; Problem 11: define a function `count-to-n` in a different way
(defn count-to-n [n] (reverse-list (count-to-1 n)))
(count-to-n 5)

;; Problem 12: define a function `get-max` without using max and reduce. do not use max and reduce. no reduce. no max.
(defn get-max [lst]
  (if (empty? lst)
    nil
    (if (empty? (rest lst))
      (first lst)
        (if (> (first lst) (get-max (rest lst)))
          (first lst)
          (get-max (rest lst))))))
(get-max '(7 2 5 3 10))

;; Problem 13: define a function `greater-than-five?`
(defn greater-than-five? [lst] (map (fn [x] (> x 5)) lst))
(greater-than-five? (list 5 4 7))

;; Problem 14: define a function `concat-three`
(defn concat-three [x y z]
  (if (empty? x)
    (if (empty? y)
      z
      (cons (first y) (concat-three x (rest y) z)))
    (cons (first x) (concat-three (rest x) y z))))
(concat-three (list 'a 'b) (list 'b 'c)  (list 'd 'e))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n] 
  (if (empty? x)
    '()
    (if (= n 0)
      '()
      (if (= n 1)
        x
        (concat x (sequence-to-power x (- n 1)))))))
(sequence-to-power (list 'a 'b) 3)

;; Problem 16: define a function `in-L-star?`
(def L '(a))
(defn in-L-star? [lst] 
  (if (empty? lst) 
    true 
    (if (= (first lst) 'a) 
      (in-L-star? (rest lst)) 
      false)))
(in-L-star? '())