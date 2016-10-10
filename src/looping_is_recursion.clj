(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [prd e]
                 (if (zero? e)
                   prd
                   (recur (* prd base) (dec e))))]
    (helper 1 exp)))

(defn last-element [a-seq]
  (let [helper (fn [cur-last rem]
                 (if (empty? rem)
                   cur-last
                   (recur (first rem) (rest rem))))]
    (helper nil a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [s1 s2]
                 (cond
                   (and (empty? s1) (empty? s2)) true
                   (or (empty? s1) (empty? s2)) false
                   (not (= (first s1) (first s2))) false
                   :else (recur (rest s1) (rest s2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [a-s a-seq
         i 0]
    (cond
      (empty? a-s) nil
      (pred (first a-s)) i
      :else (recur (rest a-s) (inc i)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         a-s a-seq]
    (if (empty? a-s)
      (if (zero? n)
        nil
        (/ acc n))
      (recur (+ acc (first a-s)) (inc n) (rest a-s)))))

(defn parity [a-seq]
  (let [toggle (fn [a-set elem]
                 (if (contains? a-set elem)
                   (disj a-set elem)
                   (conj a-set elem)))]
    (loop [odd-set #{}
           a-s a-seq]
      (if (empty? a-s)
        odd-set
        (recur (toggle odd-set (first a-s)) (rest a-s))))))

(defn fast-fibo [n]
  (cond
    (zero? n) 0
    (== n 1) 1
    :else (loop [c 0
                 fn-1 0
                 fn 1]
            (if (>= c n)
              fn-1
              (recur (inc c) fn (+ fn fn-1))))))

(defn cut-at-repetition [a-seq]
  (loop [a-s a-seq
         a-set #{}
         res []]
    (cond
      (empty? a-s) res
      (contains? a-set (first a-s)) res
      :else (recur (rest a-s) (conj a-set (first a-s)) (conj res (first a-s))))))

