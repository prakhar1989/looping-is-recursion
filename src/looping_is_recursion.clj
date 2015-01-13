(ns looping-is-recursion)

(defn power [base exp]
  (if (zero? exp) 1 
    (let [helper (fn [acc base exp]
                    (if (zero? exp) acc
                     (recur (* acc base) base (dec exp))))]
           (helper 1 base exp))))

(defn last-element [a-seq]
  (if-not (seq a-seq) nil
    (let [helper (fn [n a-seq] 
                    (if (= 1 n) (first a-seq)
                      (recur (dec n) (rest a-seq))))]
      (helper (count a-seq) a-seq))))

(defn seq= [a-seq b-seq]
  (cond 
    (and (empty? a-seq) (empty? b-seq)) true
    (or 
      (and (empty? a-seq) (not (empty? b-seq)))
      (and (empty? b-seq) (not (empty? a-seq)))
      (not (= (first a-seq) (first b-seq)))) false
    :else (recur (rest a-seq) (rest b-seq))))

(defn find-first-index [pred a-seq]
  (loop [n (count a-seq)
         xs a-seq]
    (cond 
      (zero? n) nil
      (pred (first xs)) (- (count a-seq) n)
      :else (recur (dec n) (rest xs)))))

(defn avg [a-seq]
  (loop [sum 0 n 0 xs a-seq]
    (if (empty? xs) (/ sum n)
      (recur (+ sum (first xs)) (inc n) (rest xs)))))

(defn parity [a-seq]
  (let [counts (frequencies a-seq)]
    (set (for [[k v] counts :when (odd? v)] k))))

(defn fast-fibo [n]
  (loop [c n x 0 y 1]
    (if (zero? c) x
      (recur (dec c) y (+ x y)))))

(defn cut-at-repetition [a-seq]
  (loop [xs a-seq 
         ys [] 
         acc #{}]
    (let [elem (first xs)]
      (if (or (contains? acc elem)
              (empty? xs))  ys
        (recur (rest xs) 
               (conj ys elem) 
               (conj acc elem))))))
