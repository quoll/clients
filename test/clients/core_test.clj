(ns clients.core-test
  (:require [clojure.test :refer :all]
            [clients.core :refer :all])
  (:import [clojure.lang ExceptionInfo]))

(defn fail-fn
  [n retry?]
  (let [fail-log (atom [])]
    (fn [attempt]
      (if (= n (count @fail-log))
        @fail-log
        (do
          (swap! fail-log conj attempt)
          (throw (ex-info "Exception from fail-fn"
                          {:retry retry?
                           :count (count @fail-log)})))))))

(deftest basic-fail
  (testing "simple retry fail"
    (let [f (fail-fn 5 true)
          plan5 [0 0 0 0 0]]
      (try
        (let [unused (retry-plan plan5 "basic fail" (f :x))]
          (is false))
        (catch ExceptionInfo e
          (let [data (ex-data e)]
            (is (= 1 (:count data))))
          (is (= [:x :x :x :x :x] (f :x))))))))

(deftest fail-without-retry
  (testing "simple fail"
    (let [f (fail-fn 5 false)
          plan5 [0 0 0 0 0]]
      (try
        (let [unused (retry-plan plan5 "fail-no-retry" (f :x))]
          (is false))
        (catch ExceptionInfo e
          (let [data (ex-data e)]
            (is (= 1 (:count data)))))))))

(deftest basic-retry
  (testing "simple retry"
    (let [f (fail-fn 4 true)
          plan5 [0 0 0 0 0]
          r (retry-plan plan5 "basic retry" (f :x))]
      (is (= r [:x :x :x :x] r)))))

