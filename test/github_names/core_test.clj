(ns github-names.core-test
  (:require [clojure.test :refer :all]
            [github-names.core :refer :all]))

(deftest a-test
  (testing "stats"
    (is (= (stats ["a" "b"]) nil))))
