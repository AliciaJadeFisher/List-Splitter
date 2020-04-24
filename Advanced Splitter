;; Input trees
(def my-list4 '(1 (-2 "string" dog [this is a vector]  17 (4) cat) -8 [and another vector] (rat -6 13) "blah" mango))
(def my-list5 '(3 3 ([a vector] (blah cat 9 "hello"([another vector])spam dog)"no") 5 8))
(def my-inst {:nums	{:test	number?},	:vects	{:test	vector?}:syms	{:test	symbol?}})
(def my-inst2 {:vects	{:test	vector?}:syms	{:test	symbol?}})

;; Test method
(defn test-b [lis how]
  (println "TESTING METHOD : adv-split"
  (adv-split lis how)))

;; Takes a tree as an input and calls tree-seq to return a list of each branching path within the tree
;; Then each branch is searched through for a vector data structure
;; If a vector structure is found, then it is appened to a list
(defn get-vectors [lis]
(let [tree (tree-seq sequential? seq lis)]
  (keep #(if (vector? %)  %) tree)))

;; Takes a list as an input and removes all the vectors from the list recursively
(defn remove-vectors [coll]
  (map #(if (coll? %) (remove-vectors %) %) (remove vector? coll)))

;; Takes a list as an input and returns a list of only numbers
(defn get-numbers [lis]
  (let [{numbers true} (group-by number? lis)]
  numbers))

;; Takes a list as an input and returns a list of only symbols
(defn get-symbols [lis]
  (let [{symbols true} (group-by symbol? lis)]
  symbols))

;; Tail Recursion
;; The function is passed a list and a set of instructions, which are then re-called with four empty lists for each sorted list
;; The function first checks if the list is empty, if it is then associates the sorted lists into a map and returns it
;; It then checks if the istructions contain a vector instruction. If it does the the vectors are separated from the list and passed to the next recursive call with the vector instructions removed
;; The next check is if the instructions contain a number instruction. If it does then the numbers are separated from the list and passed to the next recursive call with the number instructions removed.
;; The last check is if the instructions contain a symbol instruction. If it does then the symbols are separated from the list and passed to the next recursive call with the symbols removed.
;; This will repeat until the base case is reached
(defn adv-split 
  ([lis how] (adv-split lis how [] [] [] []))
  ([lis how nums syms vects other]
  (cond
  (empty? how)(assoc{} :nums nums :syms syms :vects vects :other {:data (vec lis)})
  (contains? how :vects)
    (adv-split (flatten (remove-vectors lis)) (dissoc how :vects) nums syms (assoc {} :test 'vector? :data(get-vectors lis)) other)
  (contains? how :nums)
    (adv-split (remove number? lis)(dissoc how :nums)(assoc {} :test 'number? :data (get-numbers lis)) syms vects other)
  (contains? how :syms)
    (adv-split (remove symbol? lis)(dissoc how :syms) nums (assoc {} :test 'symbol? :data(get-symbols lis)) vects other))))
