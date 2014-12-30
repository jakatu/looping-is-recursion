(ns looping-is-recursion)

(defn power [base exp]
  (let [pow (fn [add, e] (if (= e 0) add (recur (* add base) (dec e))))]
    (pow 1 exp)))

(defn last-element [a-seq]
  (let [last (fn [a-seq, e] (if (empty? a-seq) e (recur (rest a-seq) (first a-seq))))]
    (last a-seq nil)))

(defn seq= [seq1 seq2]
  (let [cmp (fn [s1 s2]
            (if (not (= (count s1) (count s2)))
              false 
              (if (not (= (first s1) (first s2))) 
                false 
                (if (and (empty? (rest s1)) (empty? (rest s2)))
                  true
                  (recur (rest s1) (rest s2))))))]
      (cmp seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [n 0
         a a-seq]
         (if (= n (count a-seq))
           nil
           (if (pred (first a))
             n
             (recur (inc n) (rest a))))))

(defn avg [a-seq]
  (loop [n 0
         sum 0
         a a-seq]
         (if (= (first a) nil)
           (/ sum n)
           (recur (inc n) (+ sum (first a)) (rest a)))))

(defn parity [a-seq]
  (loop [add []
         n 0
         all (set a-seq)
         e (first all)
         a a-seq]
         (if (empty? a)
           (if (= (count all) 0)
             (set add)
             (if (not (= (mod n 2) 0))
               (recur (concat add (vector e)) 0 (disj all e) (first (disj all e)) a-seq)
               (recur add 0 (disj all e) (first (disj all e)) a-seq)))
           (if (= e (first a))
             (recur add (inc n) all e (rest a))
             (recur add n all e (rest a))))))
             

(defn fast-fibo [n]
  (loop [ne 1
         nn 0
         k 1
         fib 0]
         (if (= n 0)
           0
           (if (= n 1)
             1
            (if (= k n)
              fib
              (recur (+ nn ne) ne (inc k) (+ nn ne)))))))

(defn cut-at-repetition [a-seq]
  (loop [add [(first a-seq)] 
         e (first a-seq)
         a (rest a-seq)]
         (if (= (first a) nil)
            add
           (if (= (first a) e)
             add
             (recur (concat add (vector (first a))) e (rest a))))))
         

