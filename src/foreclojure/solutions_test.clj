(ns foreclojure.solutions-test
  (:require [clojure.test :refer :all]))

(defmacro problem
  "Based on clojure.test/with-test.

  Takes any number of spec expressions followed by the definition form which
  implements the solution and returns a Var.

  Spec expressions are repacked into assertions and added as the :test metadata
  function to the solution Var.

  Example: (problem
            (= (__ [1 2 3 4 5]) 5)
            (= (__ '(5 4 3)) 3)

            (def my-solution
              (comp first reverse)))"
  [& exps]
  (let [[definition & specs] (reverse exps)
        solution (gensym)
        assertions (map #(list 'is %) specs)]
    `(let [~solution ~definition]
       (doto ~solution (alter-meta! assoc :test (fn [] (let [~'__ (deref ~solution)] ~@assertions)))))))

(problem
 ;; 19. Write a function which returns the last element in a sequence.

 (= (__ [1 2 3 4 5]) 5)
 (= (__ '(5 4 3)) 3)
 (= (__ ["b" "c" "d"]) "d")

 (def solution-19
   (comp first reverse)))

(problem
 ;; 20. Write a function which returns the second to last element from a sequence.

 (= (__ (list 1 2 3 4 5)) 4)
 (= (__ ["a" "b" "c"]) "b")
 (= (__ [[1 2] [3 4]]) [1 2])

 (def solution-20
   (comp last butlast)))

(problem
 ;; 21. Write a function which returns the Nth element from a sequence.

 (= (__ '(4 5 6 7) 2) 6)
 (= (__ [:a :b :c] 0) :a)
 (= (__ [1 2 3 4] 1) 2)
 (= (__ '([1 2] [3 4] [5 6]) 2) [5 6])

 (def solution-21
   #(first (drop %2 %))))

(problem
 ;; 22. Write a function which returns the total number of elements in a sequence.

 (= (__ '(1 2 3 3 1)) 5)
 (= (__ "Hello World") 11)
 (= (__ [[1 2] [3 4] [5 6]]) 3)
 (= (__ '(13)) 1)
 (= (__ '(:a :b :c)) 3)

 (def solution-22
   #(apply + (map (constantly 1) %))))

(problem
 ;; 23. Write a function which reverses a sequence.

 (= (__ [1 2 3 4 5]) [5 4 3 2 1])
 (= (__ (sorted-set 5 7 2 7)) '(7 5 2))
 (= (__ [[1 2][3 4][5 6]]) [[5 6][3 4][1 2]])

 (def solution-23
   #(reduce conj () %)))

(problem
 ;; 24. Write a function which returns the sum of a sequence of numbers.

 (= (__ [1 2 3]) 6)
 (= (__ (list 0 -2 5 5)) 8)
 (= (__ #{4 2 1}) 7)
 (= (__ '(0 0 -1)) -1)
 (= (__ '(1 10 3)) 14)

 (def solution-24
   #(apply + %)))

(problem
 ;; 25. Write a function which returns only the odd numbers from a sequence.

 (= (__ #{1 2 3 4 5}) '(1 3 5))
 (= (__ [4 2 1 6]) '(1))
 (= (__ [2 2 4 6]) '())
 (= (__ [1 1 1 3]) '(1 1 1 3))

 (def solution-25
   #(filter odd? %)))

(problem
 ;; 26. Write a function which returns the first X fibonacci numbers.

 (= (__ 3) '(1 1 2))
 (= (__ 6) '(1 1 2 3 5 8))
 (= (__ 8) '(1 1 2 3 5 8 13 21))

 (def solution-26
   #(take % (map first (iterate (fn [[x y]] [y (+ x y)]) [1 1])))))

(problem
 ;; 27. Write a function which returns true if the given sequence is a palindrome.

 (false? (__ '(1 2 3 4 5)))
 (true?  (__ "racecar"))
 (true?  (__ [:foo :bar :foo]))
 (true?  (__ '(1 1 3 3 1 1)))
 (false? (__ '(:a :b :c)))

 (def solution-27
   #(= (seq %) (reverse (seq %)))))

(problem
 ;; 28. Write a function which flattens a sequence.

 (= (__ '((1 2) 3 [4 [5 6]])) '(1 2 3 4 5 6))
 (= (__ ["a" ["b"] "c"]) '("a" "b" "c"))
 (= (__ '((((:a))))) '(:a))

 (def solution-28
   #(remove sequential? (rest (tree-seq sequential? seq %)))))

(problem
 ;; 29. Write a function which takes a string and returns a new string
 ;; containing only the capital letters.

 (= (__ "HeLlO, WoRlD!") "HLOWRD")
 (empty? (__ "nothing"))
 (= (__ "$#A(*&987Zf") "AZ")

 (def solution-29
   #(apply str (re-seq #"[A-Z]" %))))

(problem
 ;; 30. Write a function which removes consecutive duplicates from a sequence.

 (= (apply str (__ "Leeeeeerrroyyy")) "Leroy")
 (= (__ [1 1 2 3 3 2 2 3]) '(1 2 3 2 3))
 (= (__ [[1 2] [1 2] [3 4] [1 2]]) '([1 2] [3 4] [1 2]))

 (def solution-30
   #(map last (partition-by identity %))))

(problem
 ;; 31. Write a function which packs consecutive duplicates into sub-lists.

 (= (__ [1 1 2 1 1 1 3 3]) '((1 1) (2) (1 1 1) (3 3)))
 (= (__ [:a :a :b :b :c]) '((:a :a) (:b :b) (:c)))
 (= (__ [[1 2] [1 2] [3 4]]) '(([1 2] [1 2]) ([3 4])))

 (def solution-31
   #(partition-by identity %)))

(problem
 ;; 32. Write a function which duplicates each element of a sequence.

 (= (__ [1 2 3]) '(1 1 2 2 3 3))
 (= (__ [:a :a :b :b]) '(:a :a :a :a :b :b :b :b))
 (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))
 (= (__ [[1 2] [3 4]]) '([1 2] [1 2] [3 4] [3 4]))

 (def solution-32
   (fn [c] (mapcat #(list % %) c))))

(problem
 ;; 33. Write a function which replicates each element of a sequence a variable
 ;; number of times.

 (= (__ [1 2 3] 2) '(1 1 2 2 3 3))
 (= (__ [:a :b] 4) '(:a :a :a :a :b :b :b :b))
 (= (__ [4 5 6] 1) '(4 5 6))
 (= (__ [[1 2] [3 4]] 2) '([1 2] [1 2] [3 4] [3 4]))
 (= (__ [44 33] 2) [44 44 33 33])

 (def solution-33
   (fn [c n] (mapcat #(repeat n %) c))))

(problem
 ;; 34. Write a function which creates a list of all integers in a given range.

 (= (__ 1 4) '(1 2 3))
 (= (__ -2 2) '(-2 -1 0 1))
 (= (__ -2 2) '(-2 -1 0 1))

 (def solution-34
   #(take (- %2 %1) (iterate inc %1))))

(problem
 ;; 38. Write a function which takes a variable number of parameters and returns
 ;; the maximum value.

 (= (__ 1 8 3 4) 8)
 (= (__ 30 20) 30)
 (= (__ 45 67 11) 67)

 (def solution-38
   #(last (sort %&))))

(problem
 ;; 39. Write a function which takes two sequences and returns the first item
 ;; from each, then the second item from each, then the third, etc.

 (= (__ [1 2 3] [:a :b :c]) '(1 :a 2 :b 3 :c))
 (= (__ [1 2] [3 4 5 6]) '(1 3 2 4))
 (= (__ [1 2 3 4] [5]) [1 5])
 (= (__ [30 20] [25 15]) [30 25 20 15])

 (def solution-39
   #(mapcat list %1 %2)))

(problem
 ;; 40. Write a function which separates the items of a sequence by an arbitrary value.

 (= (__ 0 [1 2 3]) [1 0 2 0 3])
 (= (apply str (__ ", " ["one" "two" "three"])) "one, two, three")
 (= (__ :z [:a :b :c :d]) [:a :z :b :z :c :z :d])

 (def solution-40
   #(drop 1 (interleave (repeat %) %2))))

(problem
 ;; 41. Write a function which drops every Nth item from a sequence.

 (= (__ [1 2 3 4 5 6 7 8] 3) [1 2 4 5 7 8])
 (= (__ [:a :b :c :d :e :f] 2) [:a :c :e])
 (= (__ [1 2 3 4 5 6] 4) [1 2 3 5 6])

 (def solution-41
   #(apply concat (partition-all (dec %2) %2 %))))

(problem
 ;; 42. Write a function which calculates factorials.

 (= (__ 1) 1)
 (= (__ 3) 6)
 (= (__ 5) 120)
 (= (__ 8) 40320)

 (def solution-42
   #(apply * (range 1 (inc %)))))

(problem
 ;; 43. Write a function which reverses the interleave process into x number of
 ;; subsequences.

 (= (__ [1 2 3 4 5 6] 2) '((1 3 5) (2 4 6)))
 (= (__ (range 9) 3) '((0 3 6) (1 4 7) (2 5 8)))
 (= (__ (range 10) 5) '((0 5) (1 6) (2 7) (3 8) (4 9)))

 (def solution-43
   #(for [i (range %2)] (take-nth %2 (drop i %)))))

(problem
 ;; 44. Write a function which can rotate a sequence in either direction.

 (= (__ 2 [1 2 3 4 5]) '(3 4 5 1 2))
 (= (__ -2 [1 2 3 4 5]) '(4 5 1 2 3))
 (= (__ 6 [1 2 3 4 5]) '(2 3 4 5 1))
 (= (__ 1 '(:a :b :c)) '(:b :c :a))
 (= (__ -4 '(:a :b :c)) '(:c :a :b))

 (def solution-44
   #(take (count %2) (drop (mod % (count %2)) (cycle %2)))))

(problem
 ;; 45. The iterate function can be used to produce an infinite lazy sequence.

 (= __ (take 5 (iterate #(+ 3 %) 1)))

 (def solution-45
   [1 4 7 10 13]))

(problem
 ;; 46. Write a higher-order function which flips the order of the arguments of
 ;; an input function.

 (= 3 ((__ nth) 2 [1 2 3 4 5]))
 (= true ((__ >) 7 8))
 (= 4 ((__ quot) 2 8))
 (= [1 2 3] ((__ take) [1 2 3 4 5] 3))

 (def solution-46
   (fn [f] #(f %2 %))))

(problem
 ;; 47. The contains? function checks if a KEY is present in a given collection.

 (contains? #{4 5 6} __)
 (contains? [1 1 1 1 1] __)
 (contains? {4 :a 2 :b} __)
 (not (contains? [1 2 4] __))

 (def solution-47 4))

(problem
 ;; 48. The some function takes a predicate function and a collection. It
 ;; returns the first logical true value of (predicate x) where x is an item in
 ;; the collection.

 (= __ (some #{2 7 6} [5 6 7 8]))
 (= __ (some #(when (even? %) %) [5 6 7 8]))

 (def solution-48 6))

(problem
 ;; 49. Write a function which will split a sequence into two parts.

 (= (__ 3 [1 2 3 4 5 6]) [[1 2 3] [4 5 6]])
 (= (__ 1 [:a :b :c :d]) [[:a] [:b :c :d]])
 (= (__ 2 [[1 2] [3 4] [5 6]]) [[[1 2] [3 4]] [[5 6]]])

 (def solution-49
   #(list (take % %2) (drop % %2))))

(problem
 ;; 50. Write a function which takes a sequence consisting of items with
 ;; different types and splits them up into a set of homogeneous sub-sequences.
 ;; The internal order of each sub-sequence should be maintained, but the
 ;; sub-sequences themselves can be returned in any order (this is why 'set' is
 ;; used in the test cases).

 (= (set (__ [1 :a 2 :b 3 :c])) #{[1 2 3] [:a :b :c]})
 (= (set (__ [:a "foo"  "bar" :b])) #{[:a :b] ["foo" "bar"]})
 (= (set (__ [[1 2] :a [3 4] 5 6 :b])) #{[[1 2] [3 4]] [:a :b] [5 6]})

 (def solution-50
   #(vals (group-by type %))))

(problem
 ;; 51. Here is an example of some more sophisticated destructuring.

 (= [1 2 [3 4 5] [1 2 3 4 5]] (let [[a b & c :as d] __] [a b c d]))

 (def solution-51
   (range 1 6)))

(problem
 ;; 54. Write a function which returns a sequence of lists of x items each.
 ;; Lists of less than x items should not be returned.

 (= (__ 3 (range 9)) '((0 1 2) (3 4 5) (6 7 8)))
 (= (__ 2 (range 8)) '((0 1) (2 3) (4 5) (6 7)))
 (= (__ 3 (range 8)) '((0 1 2) (3 4 5)))

 (def solution-54
   (fn p [n c]
     (when (and (seq c) (>= (count c) n))
       (cons (take n c) (p n (drop n c)))))))

(problem
 ;; 55. Write a function which returns a map containing the number of
 ;; occurences of each distinct item in a sequence.

 (= (__ [1 1 2 3 2 1 1]) {1 4, 2 2, 3 1})
 (= (__ [:b :a :b :a :b]) {:a 2, :b 3})
 (= (__ '([1 2] [1 3] [1 3])) {[1 2] 1, [1 3] 2})

 (def solution-55
   #(apply merge-with + (for [k %] {k 1}))))

(problem
 ;; 56. Write a function which removes the duplicates from a sequence.
 ;; Order of the items must be maintained.

 (= (__ [1 2 1 3 1 2 4]) [1 2 3 4])
 (= (__ [:a :a :b :b :c :c]) [:a :b :c])
 (= (__ '([2 4] [1 2] [1 3] [1 3])) '([2 4] [1 2] [1 3]))
 (= (__ (range 50)) (range 50))

 (def solution-56
   (fn [c]
     (reduce #(if ((set %) %2) % (conj % %2)) [] c))))

(problem
 ;; 58. Write a function which allows you to create function compositions. The
 ;; parameter list should take a variable number of functions, and create a
 ;; function that applies them from right-to-left.

 (= [3 2 1] ((__ rest reverse) [1 2 3 4]))
 (= 5 ((__ (partial + 3) second) [1 2 3 4]))
 (= true ((__ zero? #(mod % 8) +) 3 5 7 9))
 (= "HELLO" ((__ #(.toUpperCase %) #(apply str %) take) 5 "hello world"))

 (def solution-58
   (fn [& fs]
     (reduce (fn [f g] #(f (apply g %&))) fs))))

(problem
 ;; 59. Take a set of functions and return a new function that takes a variable
 ;; number of arguments and returns a sequence containing the result of
 ;; applying each function left-to-right to the argument list.

 (= [21 6 1] ((__ + max min) 2 3 5 1 6 4))
 (= ["HELLO" 5] ((__ #(.toUpperCase %) count) "hello"))
 (= [2 6 4] ((__ :a :c :b) {:a 2, :b 4, :c 6, :d 8 :e 10}))

 (def solution-59
   (fn [& fs]
     (fn [& c] (map #(apply % c) fs)))))

(problem
 ;; 60. Write a function which behaves like reduce, but returns each
 ;; intermediate value of the reduction. Your function must accept either two
 ;; or three arguments, and the return sequence must be lazy.

 (= (take 5 (__ + (range))) [0 1 3 6 10])
 (= (__ conj [1] [2 3 4]) [[1] [1 2] [1 2 3] [1 2 3 4]])
 (= (last (__ * 2 [3 4 5])) (reduce * 2 [3 4 5]) 120)

 (def solution-60
   (fn rd
     ([f c]
      (rd f (first c) (rest c)))
     ([f x c]
      (cons x
            (lazy-seq
             (when-let [s (seq c)]
               (rd f (f x (first s)) (rest s)))))))))

(problem
 ;; 61. Write a function which takes a vector of keys and a vector of values
 ;; and constructs a map from them.

 (= (__ [:a :b :c] [1 2 3]) {:a 1, :b 2, :c 3})
 (= (__ [1 2 3 4] ["one" "two" "three"]) {1 "one", 2 "two", 3 "three"})
 (= (__ [:foo :bar] ["foo" "bar" "baz"]) {:foo "foo", :bar "bar"})

 (def solution-61
   #(apply hash-map (interleave % %2))))

(problem
 ;; 62. Given a side-effect free function f and an initial value x write a
 ;; function which returns an infinite lazy sequence of x, (f x), (f (f x)),
 ;; (f (f (f x))), etc.

 (= (take 5 (__ #(* 2 %) 1)) [1 2 4 8 16])
 (= (take 100 (__ inc 0)) (take 100 (range)))
 (= (take 9 (__ #(inc (mod % 3)) 1)) (take 9 (cycle [1 2 3])))

 (def solution-62
   (fn i [f x] (lazy-seq (cons x (i f (f x)))))))

(problem
 ;; 63. Given a function f and a sequence s, write a function which returns a
 ;; map. The keys should be the values of f applied to each item in s. The
 ;; value at each key should be a vector of corresponding items in the order
 ;; they appear in s.

 (= (__ #(> % 5) [1 3 6 8]) {false [1 3], true [6 8]})
 (= (__ #(apply / %) [[1 2] [2 4] [4 6] [3 6]])
    {1/2 [[1 2] [2 4] [3 6]], 2/3 [[4 6]]})
 (= (__ count [[1] [1 2] [3] [1 2 3] [2 3]])
    {1 [[1] [3]], 2 [[1 2] [2 3]], 3 [[1 2 3]]})

 (def solution-63
   #(apply merge-with into (for [v %2] {(% v) [v]}))))

(problem
 ;; 66. Given two integers, write a function which returns the greatest common divisor.

 (= (__ 2 4) 2)
 (= (__ 10 5) 5)
 (= (__ 5 7) 1)
 (= (__ 1023 858) 33)

 (def solution-66
   (fn [x y]
     (last
      (for [i (range 1 (max x y)) :when (= 0 (mod x i) (mod y i))]
        i)))))

(problem
 ;; 70. Write a function that splits a sentence up into a sorted list of words.
 ;; Capitalization should not affect sort order and punctuation should be ignored.

 (= (__  "Have a nice day.")
    ["a" "day" "Have" "nice"])
 (= (__  "Clojure is a fun language!")
    ["a" "Clojure" "fun" "is" "language"])
 (= (__  "Fools fall for foolish follies.")
    ["fall" "follies" "foolish" "Fools" "for"])

 (def solution-70
   #(sort-by clojure.string/lower-case (clojure.string/split % #"\W"))))

(problem
 ;; 74. Given a string of comma separated integers, write a function which
 ;; returns a new comma separated string that only contains the numbers which
 ;; are perfect squares.

 (= (__ "4,5,6,7,8,9") "4,9")
 (= (__ "15,16,25,36,37") "16,25,36")

 (def solution-74
   (fn [s]
     (clojure.string/join "," (filter #(integer? (rationalize (Math/sqrt (Integer/parseInt %1)))) (clojure.string/split s #","))))))

(problem
 ;; 81. Write a function which returns the intersection of two sets. The
 ;; intersection is the sub-set of items that each set has in common.

 (= (__ #{0 1 2 3} #{2 3 4 5}) #{2 3})
 (= (__ #{0 1 2} #{3 4 5}) #{})
 (= (__ #{:a :b :c :d} #{:c :e :a :f :d}) #{:a :c :d})

 (def solution-81
   (fn [x y] (set (filter #(contains? x %) y)))))

(problem
 ;; 83. Write a function which takes a variable number of booleans. Your
 ;; function should return true if some of the parameters are true, but not all
 ;; of the parameters are true. Otherwise your function should return false.

 (= false (__ false false))
 (= true  (__ true false))
 (= false (__ true))
 (= true  (__ false true false))
 (= false (__ true true true))
 (= true  (__ true true true false))

 (def solution-83 not=))

(problem
 ;; 85. Write a function which generates the power set of a given set.

 (= (__ #{1 :a}) #{#{1 :a} #{:a} #{} #{1}})
 (= (__ #{}) #{#{}})
 (= (__ #{1 2 3})
    #{#{} #{1} #{2} #{3} #{1 2} #{1 3} #{2 3} #{1 2 3}})
 (= (count (__ (into #{} (range 10)))) 1024)

 (def solution-85
   (fn [s]
     (reduce #(into % (for [ss %] (conj ss %2))) #{#{}} s))))

(problem
 ;; 88. Write a function which returns the symmetric difference of two sets. The
 ;; symmetric difference is the set of items belonging to one but not both of
 ;; the two sets.

 (= (__ #{1 2 3 4 5 6} #{1 3 5 7}) #{2 4 6 7})
 (= (__ #{:a :b :c} #{}) #{:a :b :c})
 (= (__ #{} #{4 5 6}) #{4 5 6})
 (= (__ #{[1 2] [2 3]} #{[2 3] [3 4]}) #{[1 2] [3 4]})

 (def solution-88
   #(clojure.set/union (clojure.set/difference % %2) (clojure.set/difference %2 %))))

(problem
 ;; 90. Write a function which calculates the Cartesian product of two sets.

 (= (__ #{"ace" "king" "queen"} #{"♠" "♥" "♦" "♣"})
    #{["ace"   "♠"] ["ace"   "♥"] ["ace"   "♦"] ["ace"   "♣"]
      ["king"  "♠"] ["king"  "♥"] ["king"  "♦"] ["king"  "♣"]
      ["queen" "♠"] ["queen" "♥"] ["queen" "♦"] ["queen" "♣"]})
 (= (__ #{1 2 3} #{4 5})
    #{[1 4] [2 4] [3 4] [1 5] [2 5] [3 5]})
 (= 300 (count (__ (into #{} (range 10))
                   (into #{} (range 30)))))

 (def solution-90
   #(set (for [x % y %2] (vector x y)))))

(problem
 ;; 95. Write a predicate which checks whether or not a given sequence
 ;; represents a binary tree. Each node in the tree must have a value, a left
 ;; child, and a right child.

 (= (__ '(:a (:b nil nil) nil)) true)
 (= (__ '(:a (:b nil nil))) false)
 (= (__ [1 nil [2 [3 nil nil] [4 nil nil]]]) true)
 (= (__ [1 [2 nil nil] [3 nil nil] [4 nil nil]]) false)
 (= (__ [1 [2 [3 [4 nil nil] nil] nil] nil]) true)
 (= (__ [1 [2 [3 [4 false nil] nil] nil] nil]) false)
 (= (__ '(:a nil ())) false)

 (def solution-95
   (fn tree? [t]
     (cond
       (or (seq? t) (vector? t))
       (and (= 3 (count t)) (every? tree? t))
       (nil? t) true
       :else t))))

(problem
 ;; 96. Write a predicate to determine whether or not a given binary tree is symmetric.

 (= (__ '(:a (:b nil nil) (:b nil nil))) true)
 (= (__ '(:a (:b nil nil) nil)) false)
 (= (__ '(:a (:b nil nil) (:c nil nil))) false)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] [5 nil nil]]] nil]])
    true)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [5 nil nil] [6 nil nil]]] nil]])
    false)
 (= (__ [1 [2 nil [3 [4 [5 nil nil] [6 nil nil]] nil]]
        [2 [3 nil [4 [6 nil nil] nil]] nil]])
    false)

 (def solution-96
   #(= % ((fn mirror [[n l r :as t]] (when t [n (mirror r) (mirror l)])) %))))

(problem
 ;; 97. Write a function which returns the nth row of Pascal's Triangle.

 (= (__ 1) [1])
 (= (map __ (range 1 6)
         [[1]
         [1 1]
        [1 2 1]
       [1 3 3 1]
      [1 4 6 4 1]]))
 (= (__ 11) [1 10 45 120 210 252 210 120 45 10 1])

 (def solution-97
   (fn [n]
     (last (take n (iterate #(map + `(0 ~@%) `(~@% 0)) [1]))))))

(problem
 ;; 98. Write a function with arguments f and D that computes the equivalence
 ;; classes of D with respect to f.

 (= (__ #(* % %) #{-2 -1 0 1 2})
    #{#{0} #{1 -1} #{2 -2}})
 (= (__ #(rem % 3) #{0 1 2 3 4 5 })
    #{#{0 3} #{1 4} #{2 5}})
 (= (__ identity #{0 1 2 3 4})
    #{#{0} #{1} #{2} #{3} #{4}})
 (= (__ (constantly true) #{0 1 2 3 4})
    #{#{0 1 2 3 4}})

 (def solution-98
   #(set (map set (vals (group-by % %2))))))

(problem
 ;; 99. Write a function which multiplies two numbers and returns the result as
 ;; a sequence of its digits.

 (= (__ 1 1) [1])
 (= (__ 99 9) [8 9 1])
 (= (__ 999 99) [9 8 9 0 1])

 (def solution-99
   #(for [d (str (* % %2))] (Character/digit d 10))))

(problem
 ;; 100. Write a function which calculates the least common multiple. Your
 ;; function should accept a variable number of positive integers or ratios.

 (== (__ 2 3) 6)
 (== (__ 5 3 7) 105)
 (== (__ 1/3 2/5) 2)
 (== (__ 3/4 1/6) 3/2)
 (== (__ 7 5/7 2 3/5) 210)

 (def solution-100
   (fn [& c]
     (let [gcd (fn [a b] (if (zero? b) a (recur b (mod a b))))]
       (/ (apply * c) (reduce gcd c))))))

(problem
 ;; 103. Given a sequence S consisting of n elements generate all
 ;; k-combinations of S, i. e. generate all possible sets consisting of k
 ;; distinct elements taken from S.

 (= (__ 1 #{4 5 6}) #{#{4} #{5} #{6}})
 (= (__ 10 #{4 5 6}) #{})
 (= (__ 2 #{0 1 2}) #{#{0 1} #{0 2} #{1 2}})
 (= (__ 3 #{0 1 2 3 4}) #{#{0 1 2} #{0 1 3} #{0 1 4} #{0 2 3} #{0 2 4}
                          #{0 3 4} #{1 2 3} #{1 2 4} #{1 3 4} #{2 3 4}})
 (= (__ 4 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a "abc" "efg"}})
 (= (__ 2 #{[1 2 3] :a "abc" "efg"}) #{#{[1 2 3] :a} #{[1 2 3] "abc"} #{[1 2 3] "efg"}
                                       #{:a "abc"} #{:a "efg"} #{"abc" "efg"}})

 (def solution-103
   (fn [k s]
     (clojure.set/select #(= k (count %)) (reduce #(into % (for [ss %] (conj ss %2))) #{#{}} s)))))

(problem
 ;; 107. Given a positive integer n, return a function (f x) which computes
 ;; x^n. Observe that the effect of this is to preserve the value of n for use
 ;; outside the scope in which it is defined.

 (= 256 ((__ 2) 16),
    ((__ 8) 2))
 (= [1 8 27 64] (map (__ 3) [1 2 3 4]))
 (= [1 2 4 8 16] (map #((__ %) 2) [0 1 2 3 4]))

 (def solution-107
   (fn [n]
     #(int (Math/pow % n)))))

(problem
 ;; 118. Given a function f and an input sequence s, return a lazy sequence
 ;; of (f x) for each element x in s.

 (= [3 4 5 6 7]
    (__ inc [2 3 4 5 6]))
 (= (repeat 10 nil)
    (__ (fn [_] nil) (range 10)))
 (= [1000000 1000001]
    (->> (__ inc (range))
         (drop (dec 1000000))
         (take 2)))

 (def solution-118
   (fn m [f c]
     (lazy-seq
      (when-let [s (seq c)]
        (cons (f (first s)) (m f (rest s))))))))

(problem
 ;; 120. Write a function which takes a collection of integers as an argument.
 ;; Return the count of how many elements are smaller than the sum of their
 ;; squared component digits.

 (= 8 (__ (range 10)))
 (= 19 (__ (range 30)))
 (= 50 (__ (range 100)))
 (= 50 (__ (range 1000)))

 (def solution-120
   (fn [c]
     (let [digs #(for [d (str %)] (Character/digit d 10))
           sqr #(* % %)
           sum #(apply + (map sqr (digs %)))]
       (count (filter #(< % (sum %)) c))))))

(problem
 ;; 122. Convert a binary number, provided in the form of a string, to its
 ;; numerical value.

 (= 0     (__ "0"))
 (= 7     (__ "111"))
 (= 8     (__ "1000"))
 (= 9     (__ "1001"))
 (= 255   (__ "11111111"))
 (= 1365  (__ "10101010101"))
 (= 65535 (__ "1111111111111111"))

 (def solution-122
   #(Integer/parseInt % 2)))

(problem
 ;; 126. Enter a value which satisfies the following:

 (let [x __]
   (and (= (class x) x) x))

 (def solution-126 Class))

(problem
 ;; 128. Write a function which converts (for example) the string "SJ" into a
 ;; map of {:suit :spade, :rank 9}. A ten will always be represented with the
 ;; single character "T", rather than the two characters "10".

 (= {:suit :diamond :rank 10} (__ "DQ"))
 (= {:suit :heart :rank 3} (__ "H5"))
 (= {:suit :club :rank 12} (__ "CA"))
 (= (range 13) (map (comp :rank __ str)
                    '[S2 S3 S4 S5 S6 S7
                      S8 S9 ST SJ SQ SK SA]))

 (def solution-128
   (fn [[s r]]
     (let [suits (zipmap "SHCD" [:spade :heart :club :diamond])
           ranks (zipmap "23456789TJQKA" (range 13))]
       {:suit (suits s) :rank (ranks r)}))))

(problem
 ;; 135. Write a function that accepts a variable length mathematical expression
 ;; consisting of numbers and the operations +, -, *, and /. Assume a simple
 ;; calculator that does not do precedence and just calculates left to right.

 (= 7  (__ 2 + 5))
 (= 42 (__ 38 + 48 - 2 / 2))
 (= 8  (__ 10 / 2 - 1 * 2))
 (= 72 (__ 20 / 2 + 2 + 4 + 8 - 6 - 10 * 9))

 (def solution-135
   (fn [& e]
     (reduce #(if (fn? %) (% %2) (partial %2 %)) e))))

(problem
 ;; 143. Create a function that computes the dot product of two sequences.
 ;; You may assume that the vectors will have the same length.

 (= 0   (__ [0 1 0] [1 0 0]))
 (= 3   (__ [1 1 1] [1 1 1]))
 (= 32  (__ [1 2 3] [4 5 6]))
 (= 256 (__ [2 5 6] [100 10 1]))

 (def solution-143
   #(apply + (map * % %2))))

(problem
 ;; 144. Write an oscillating iterate: a function that takes an initial value
 ;; and a variable number of functions. It should return a lazy sequence of the
 ;; functions applied to the value in order, restarting from the first function
 ;; after it hits the end.

 (= (take 3 (__ 3.14 int double)) [3.14 3 3.0])
 (= (take 5 (__ 3 #(- % 3) #(+ 5 %))) [3 0 5 2 7])
 (= (take 12 (__ 0 inc dec inc dec inc)) [0 1 0 1 0 1 2 1 2 1 2 3])

 (def solution-144
   (fn [x & fs]
     (reductions #(%2 %) x (cycle fs)))))

(problem
 ;; 146. Your goal is to "flatten" a map of hashmaps. Each key in your output
 ;; map should be the "path" that you would have to take in the original map to
 ;; get to a value, so for example {1 {2 3}} should result in {[1 2] 3}.

 (= (__ '{a {p 1, q 2}
          b {m 3, n 4}})
    '{[a p] 1, [a q] 2
      [b m] 3, [b n] 4})
 (= (__ '{[1] {a b c d}
          [2] {q r s t u v w x}})
    '{[[1] a] b, [[1] c] d,
      [[2] q] r, [[2] s] t,
      [[2] u] v, [[2] w] x})
 (= (__ '{m {1 [a b c] 3 nil}})
    '{[m 1] [a b c], [m 3] nil})

 (def solution-146
   #(into {} (for [[k v] % [q w] v] [[k q] w]))))

(problem
 ;; 147. Write a function that, for any given input vector of numbers, returns
 ;; an infinite lazy sequence of vectors, where each next one is constructed
 ;; from the previous following the rules used in Pascal's Triangle.

 (= (second (__ [2 3 2])) [2 5 5 2])
 (= (take 5 (__ [1])) [[1] [1 1] [1 2 1] [1 3 3 1] [1 4 6 4 1]])
 (= (take 2 (__ [3 1 2])) [[3 1 2] [3 4 3 2]])
 (= (take 100 (__ [2 4 2])) (rest (take 101 (__ [2 2]))))

 (def solution-147
   (fn [r]
     (iterate #(map +' `(0 ~@%) `(~@% 0)) r))))

(problem
 ;; 153. Given a set of sets, create a function which returns true if no two of
 ;; those sets have any elements in common and false otherwise.

 (= (__ #{#{\U} #{\s} #{\e \R \E} #{\P \L} #{\.}})
    true)
 (= (__ #{#{:a :b :c :d :e}
          #{:a :b :c :d}
          #{:a :b :c}
          #{:a :b}
          #{:a}})
    false)
 (= (__ #{#{[1 2 3] [4 5]}
          #{[1 2] [3 4 5]}
          #{[1] [2] 3 4 5}
          #{1 2 [3 4] [5]}})
    true)
 (= (__ #{#{'a 'b}
          #{'c 'd 'e}
          #{'f 'g 'h 'i}
          #{''a ''c ''f}})
    true)
 (= (__ #{#{'(:x :y :z) '(:x :y) '(:z) '()}
          #{#{:x :y :z} #{:x :y} #{:z} #{}}
          #{'[:x :y :z] [:x :y] [:z] [] {}}})
    false)
 (= (__ #{#{(= "true") false}
          #{:yes :no}
          #{(class 1) 0}
          #{(symbol "true") 'false}
          #{(keyword "yes") ::no}
          #{(class '1) (int \0)}})
    false)
 (= (__ #{#{distinct?}
          #{#(-> %) #(-> %)}
          #{#(-> %) #(-> %) #(-> %)}
          #{#(-> %) #(-> %) #(-> %)}})
    true)
 (= (__ #{#{(#(-> *)) + (quote mapcat) #_ nil}
          #{'+ '* mapcat (comment mapcat)}
          #{(do) set contains? nil?}
          #{, , , #_, , empty?}})
    false)

 (def solution-153
   #(= (apply + (map count %))
       (count (apply clojure.set/union %)))))

(problem
 ;; 157. Transform a sequence into a sequence of pairs containing the original
 ;; elements along with their index.

 (= (__ [:a :b :c]) [[:a 0] [:b 1] [:c 2]])
 (= (__ [0 1 3]) '((0 0) (1 1) (3 2)))
 (= (__ [[:foo] {:bar :baz}]) [[[:foo] 0] [{:bar :baz} 1]])

 (def solution-157
   #(map vector % (range))))

(problem
 ;; 166. Write a function that takes three arguments, a less than operator for
 ;; the data and two items to compare. The function should return a keyword
 ;; describing the relationship between the two items.

 (= :gt (__ < 5 1))
 (= :eq (__ (fn [x y] (< (count x) (count y))) "pear" "plum"))
 (= :lt (__ (fn [x y] (< (mod x 5) (mod y 5))) 21 3))
 (= :gt (__ > 0 2))

 (def solution-166
   (fn [f x y]
     (cond
       (f x y) :lt
       (f y x) :gt
       :else :eq))))
