(ns task01.core
  (:require [pl.danieljanus.tagsoup :refer :all])
  (:gen-class))

(defn get-link-from-result [children]
  (let [attrs (second (first children))]
    (list (get attrs :href))))

; can't find anything built-in
(defn flatmap [f seq]
  (reduce #(concat %1 (f %2)) () seq))

(defn get-links-1 [node]
"find all class r elements under a given node"
  (cond 
    (string? node) 
      ()
    (= (get (attributes node) :class) "r") 
      (get-link-from-result (children node))
    :else
      (flatmap get-links-1 (children node))))

(defn get-links []
" 1) Find all elements containing {:class \"r\"}.

Example:
[:h3 {:class \"r\"} [:a {:shape \"rect\", :class \"l\",
                         :href \"https://github.com/clojure/clojure\",
                         :onmousedown \"return rwt(this,'','','','4','AFQjCNFlSngH8Q4cB8TMqb710dD6ZkDSJg','','0CFYQFjAD','','',event)\"}
                     [:em {} \"clojure\"] \"/\" [:em {} \"clojure\"] \" · GitHub\"]]

   2) Extract href from the element :a.

The link from the example above is 'https://github.com/clojure/clojure'.

  3) Return vector of all 10 links.

Example: ['https://github.com/clojure/clojure', 'http://clojure.com/', . . .]
"
  (let [data (parse "clojure_google.html")]
    (get-links-1 data)))
  
(defn -main []
  (println (str "Found " (count (get-links)) " links!")))


