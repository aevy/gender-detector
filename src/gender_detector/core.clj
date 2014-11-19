(ns gender-detector.core
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader resource]]))

(defn highest-score [scores]
  (-> scores
      sort
      last
      str
      (Integer/parseInt 16)))

(defn parse-line [s]
  (let [[gender-and-name] (remove empty? (string/split s #"\s{3}"))
        [gender & name] (string/split gender-and-name #"\s+")
        scores (remove #{\$ \space} (drop 30 s))
        name (string/join " " name)]
    [name gender (highest-score scores)]))

(defn lines []
  (with-open [rdr (reader (resource "nam_dict.txt") :encoding "ISO-8859-1")]
    (let [lines (line-seq rdr)]
      (remove (partial re-find #"\A[#=]") (doall (line-seq rdr))))))

(defonce names
  (->> (map parse-line (lines))
       (group-by first)
       (map (fn [[name values]]
              [(string/lower-case name) (-> (sort-by second values)
                                            reverse
                                            first
                                            (nth 1))]))
       (into {})))

(defn get-gender [name]
  (get names (string/lower-case name)))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))