(ns github-names.core-test
  (:require [clojure.test :refer :all]
            [github-names.core :refer :all]))

(deftest a-test
  (testing "stats"
    (is (= (stats ["a" "b"]) nil))))

(deftest get-data-simple-name
  (testing "simple name"
    (let [d (get-data ["simple-name"])]
      (is (= (count stat-filters) (count d)))
      (is (= 1 (-> d (map :count) (reduce +))))
      )))

