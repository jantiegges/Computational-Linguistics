
;; Problem 1: define a function `absval`
(defn absval [x] (Math/sqrt (* x x)))

;; Problem 2: fix the functions `take-square` and `sum-of-squares`
(defn take-square [x] (* x x))
(defn sum-of-squares [x y] (+ (take-square x) (take-square y)))

;; Problem 3: define expressions `exp-13-1`, `exp-13-2`, `exp-13-3`, and `exp-13-4`
(def exp-13-1 '(- 13 0))
(def exp-13-2 '(* 2 6.5))
(def exp-13-3 '(/ 26 2))
(def exp-13-4 '(Math/sqrt 169))

;; Problem 4: define a function `third`
(defn third [x] (first (rest (rest x))))

;; Problem 5:
;; [let's import define `sqrt` and `abs` from Java.lang.Math for the example]
(defn sqrt [x] (Math/sqrt x))
(defn abs [x] (Math/abs x))
;; define a function `compose` 
(defn compose [f g] (fn [x] (f (g x))))

;; Problem 6: define a function `first-two`
(defn first-two [l] (list (first l) (first (rest l))))

;; Problem 7: define a function `remove-second`
(defn remove-second [l] (cons (first l) (rest (rest l))))

;; Problem 8: define a function `add-to-end`
(defn add-to-end [lst x]
  (if (empty? lst)
    (list x)
    (cons (first lst) (add-to-end (rest lst) x))))

;; Problem 9: define a function `reverse`
;;           (Note that this funciton will overwrite a built in function. 
;;            This is okay.)
(defn reverse-list [lst]
  (if (empty? lst)
    '()
    (concat (reverse-list (rest lst)) (list (first lst)))))

;; Problem 10: define a function `count-to-1`
(defn count-to-1 [n]
  (if (<= n 1)
    (list 1)
    (cons n (count-to-1 (- n 1)))))

;; Problem 11: define a function `count-to-n`
(defn count-to-n [n] (reverse-list (count-to-1 n)))

;; Problem 12: define a function `get-max`
(defn get-max [lst]
  (if (empty? lst)
    nil
    (if (empty? (rest lst))
      (first lst)
      (if (> (first lst) (get-max (rest lst)))
        (first lst)
        (get-max (rest lst))))))

;; Problem 13: define a function `greater-than-five?`
(defn greater-than-five? [lst] (map (fn [x] (> x 5)) lst))

;; Problem 14: define a function `concat-three`
(defn concat-three [x y z]
  (if (empty? x)
    (if (empty? y)
      z
      (cons (first y) (concat-three x (rest y) z)))
    (cons (first x) (concat-three (rest x) y z))))

;; Problem 15: define a function `sequence-to-power`
(defn sequence-to-power [x n]
  (if (empty? x)
    '()
    (if (= n 0)
      '()
      (if (= n 1)
        x
        (concat x (sequence-to-power x (- n 1)))))))

;; Problem 16: define a function `in-L-star?`
(def L '(a))
(defn in-L-star? [lst]
  (if (empty? lst)
    true
    (if (= (first lst) 'a)
      (in-L-star? (rest lst))
      false)))