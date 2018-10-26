(ns gender-detector.core
  (:require [clojure.string :as string]
            [clojure.java.io :refer [reader resource]]))

(defn gender-desc [s]
  (case s
    "M"  :male
    "?M" :mostly-male
    "1M" :mostly-male
    "F"  :female
    "?F" :mostly-female
    "1F" :mostly-female
    :unknown))

(defn hex->int [s]
  (Integer/parseInt s 16))

(defn highest-score [scores]
  (-> scores
      sort
      last
      str
      hex->int))

(defn parse-line [s]
  (let [gender-and-name (first (remove empty? (string/split s #"\s{3}")))
        [gender & name-parts] (string/split gender-and-name #"\s+")
        name (string/join " " name-parts)
        scores (remove #{\$ \space} (drop 30 s))]
    [name gender (highest-score scores)]))

(defn lines []
  (with-open [rdr (reader (resource "nam_dict.txt") :encoding "ISO-8859-1")]
    (remove #(re-find #"\A[#=]" %) (doall (line-seq rdr)))))

(defonce names
  (->> (map parse-line (lines))
       (group-by first)
       (map (fn [[name values]]
              [(string/lower-case name) (-> (sort-by second values)
                                            reverse
                                            first
                                            (nth 1)
                                            gender-desc)]))
       (into {})))

(defn guess-gender [name]
  (get names (string/lower-case name)))
