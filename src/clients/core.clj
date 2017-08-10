(ns ^{:doc "Helper utilities for communicating with services."
      :author "Paula Gearon"}
  clients.core
  (:require [clojure.string :as s]
            [clojure.tools.logging :as log])
  (:import [clojure.lang ExceptionInfo]))

(def delay-seed-property "clients.delay.seed")

(defn get-system-long
  "Reads a system property as a Long value"
  ([param] (get-system-long param nil))
  ([param default]
   (or
     (if-let [v (System/getProperty param)]
       (try
         (Long/parseLong v)
         (catch NumberFormatException e
           (log/error (str "Bad numerical value for parameter [" param "]: '" v "'")))))
     default)))

(def delay-seed (get-system-long delay-seed-property 100))

(defn get-root-cause
  "Determine the throwable that caused a given exception condition"
  [^Throwable ex]
  (if-let [cause (.getCause ex)]
    (recur cause)
    ex))

(defn retryable?
  "Tests if an exception meets conditions for attempting an operation again"
  [^Throwable e]
  (let [ex (get-root-cause e)]
    (cond
      (instance? ExceptionInfo ex) (:retry (ex-data ex))
      (instance? RuntimeException ex) true
      (s/includes? (.getMessage ex) ":retry") true)))

(defn try-loop
  "Attempt a thunk, retrying according to the backoff plan"
  [msg retry-test backoff-plan thunk]
  (loop [[next-retry-delay & retries :as r] backoff-plan current 1 first-error nil]
    (if-not (seq r)
      (do
        (log/error (str "Unable to " msg))
        (throw first-error))
      (let [[result error] (try [(thunk) nil] (catch Throwable t [nil t]))]
        (if-not error
          result
          (if (retry-test error)
            (do
              (when (seq retries)
                (log/warn (str "Unsuccessful attempt to " msg ". Retry " current)))
              (when next-retry-delay (Thread/sleep next-retry-delay))
              (recur retries (inc current) (or first-error error)))
            (throw (or first-error error))))))))

(defn jitter
 "Modifies a number to randomly change by some percentage.
  Default jitter is 5%."
 ([x] (jitter 5 x))
 ([p x]
  (let [r (/ (* x p) 100)]
    (+ (- x r) (rand-int (inc (* 2 r)))))))

(def exponential-plan
  "Exponentially increasing delays starting at the delay seed, and doubling each time."
  (iterate (partial * 2) delay-seed))

(def linear-plan
  "Linearly increating delays starting at the delay-seed, and increasing by this amount each time."
  (iterate (partial + delay-seed) delay-seed))

(def exponential-jitter-plan
  "Exponentially increasing delays with +-5% jitter"
  (map jitter exponential-plan))

(def linear-jitter-plan
  "Linearly increasing delays with +-5% jitter"
  (map jitter linear-plan))

(def ^:dynamic *default-plan* exponential-jitter-plan)

(def ^:dynamic *default-repeat* 10)

(defmacro retry
  "Same as retry-for with all parameters required.
  Attempts an operation, reattempting on failure according to a provided plan.
  Plans are a sequence of backoff delays to use, and must be limited if they are not to be repeated indefinitely.
  The operations are only retried when an exception passes a provided predicate.
  Other exception types are propagated.
  The description argument is used for logging the errors.
  If the plan is exhausted, then the first exception before all other retries is thrown."
  {:arglists '([plan description & body])}
  [plan exception-test description & form]
  `(try-loop ~description ~exception-test ~plan (fn [] ~@form)))

(defmacro retry-plan
  "Attempts an operation, retrying according to a plan.
   An optional literal string argument can be used to describe failures in the log. "
  {:arglists '([plan description &body] [plan & body])}
  [plan & form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))]
    `(try-loop ~msg# retryable? ~plan (fn [] ~@form#))))

(defmacro retry-for
  "Attempts an operation, retrying for exceptions that pass a given predicate.
   An optional literal string argument can be used to describe failures in the log.
   See retry for more details."
  {:arglists '([plan exception-test description &body] [plan exception-test & body])}
  [plan exception-test & form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))]
    `(try-loop ~msg# ~exception-test ~plan (fn [] ~@form#))))

(defmacro retry-exp
  "Attempts an operation using the default plan up to *default-repeat* times.
   Uses default tests for the retryable exception.
   An optional literal string argument can be used to describe failures in the log."
  {:arglists '([op-descrption &body] [& body])}
  [& form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))
        plan# (take *default-repeat* *default-plan*)]
    `(try-loop ~msg# retryable? ~plan# (fn [] ~@form#))))

(defmacro retry-for-exp
  "Attempts an operation using the default plan up to *default-repeat* times.
   Exception are retried if they pass a given predicate.
   An optional literal string argument can be used to describe failures in the log."
  {:arglists '([exception-test description &body] [exception-test & body])}
  [exception-test & form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))
        plan# (take *default-repeat* *default-plan*)]
    `(try-loop ~msg# ~exception-test ~plan# (fn [] ~@form#))))

(defmacro retry-n
  "Attempts an operation, repeating up to n times using the default plan.
   An optional literal string argument can be used to describe failures in the log."
  {:arglists '([n description &body] [n & body])}
  [n & form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))
        plan# (take n *default-plan*)]
    `(try-loop ~msg# retryable? ~plan# (fn [] ~@form#))))

(defmacro retry-n-for
  "Attempts an operation, repeating up to n times using the default plan.
   Exception are retried if they pass a given predicate.
   An optional literal string argument can be used to describe failures in the log."
  {:arglists '([n exception-test description &body] [n exception-test & body])}
  [n exception-test & form]
  (let [[msg# & form#] (if (string? (first form)) form (cons "do operation" form))
        plan# (take n *default-plan*)]
    `(try-loop ~msg# ~exception-test ~plan# (fn [] ~@form#))))
