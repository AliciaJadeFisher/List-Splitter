;; Input trees
(def my-list '(1 (-2 dog 17 (4) cat) -8 (rat -6 13) mango))
(def my-list2 '(4 5 (dog cat (6 (hello) 9) mango spam (rat))10 11))
(def my-list3 '( 9 ((3 5 dog rat (hello 7 8 9 )) spam) cat))

;; Test method
(defn test-a [lis]
  (println "TESTING METHOD : split"
  (split lis))
  (println "TESTING METHOD : split-recur"
  (split-recur lis)))

;; Takes a sequence as an input and flattens it first
;; Then the list is grouped into numbers if true and symbols if false
;; Destructuring is used to assign true and false to numbers and symbols
;; The list for numbers and symbols is then converted to a map for output, with the keys :numbers and :symbols
(defn split [lis]
  (let [{numbers true symbols false} (group-by number? (flatten lis))]
  (assoc {} :numbers numbers :symbols symbols)))

;; Tail Recursion
;; The function is passed a list, which is flattened when re-called with the two empty lists for nunbers and symbols
;; The function first checks if the list is empty, if it is then is associates the two lists into a map and returns it 
;; It then checks if the first element in the list is a number
;; If it is then it adds the element to the number list and recurs on the rest of the initial list
;; The last check is if the first element in the list is a symbol
;; If it is then it adds the element to the symbol list and recurs on the rest of the initial list
;; This will repeat until the base case is reached
(defn split-recur 
  ([lis] (split-recur (flatten lis) [] []))
  ([lis num sym]
  (cond
  (empty? lis) 
    (assoc {} :numbers num :symbols sym)
  (number? (first lis))
    (split-recur (rest lis) (conj num (first lis)) sym)
  (symbol? (first lis))
    (split-recur (rest lis) num (conj sym (first lis))))))


