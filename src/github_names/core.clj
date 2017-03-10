(ns github-names.core
  (:require
    [clj-http.client   :as client]
    [clojure.string    :as string]
    [clojure.set       :as sets]
    [clojure.pprint    :as pp]
    [clojure.data.json :as json]
    [clojure.tools.cli :refer [parse-opts]]
    ))

(defn- get-names
  "fetch some github project names from url"
  [url]
  (println url)
  (let [response (client/get url)
        status   (:status response)
        headers  (:headers response)
        next-url (-> response :links :next :href)
        body     (-> response :body json/read-str)]
    (when (== status 200)
      [(map (fn [x] (.get x "name")) body) next-url]
      )
    )
  )

(defn exit [status msg]
  (println msg)
  (System/exit status))

(def cli-options
  [
   ["-n" "--number NUMBER" "Min number of names to extract"
    :default 80
    :parse-fn #(Integer/parseInt %)]
   ["-s" "--since NUMBER" "starting point for github query"
    :default 0
    :parse-fn #(Integer/parseInt %)]
   ["-f" "--file FILE" "File in which to write the names"
    :default nil]
   ["-i" "--input FILE" "file in which finding names"
    :default nil]
   ["-h" "--help"]
   ]
  )

(defn- process-names
  "handle names"
  [names file]
  (if file
    (doall (map (fn [n] (spit file (str n "\n") :append true)) names))
    (println "names" names)
    )
  )

(def stat-filters
  [
   {
    :description "pure lowercase alphanumeric"
    :filter       (fn [n] (re-matches #"[a-z][a-z0-9]*" n))
    }
   {
    :description "lowercase alphanumeric with minus"
    :filter       (fn [n] (and
                            (re-matches #"[a-z][a-z0-9-]*" n)
                            (re-find #"-" n)))
    }
   {
    :description "lowercase alphanumeric with underscore"
    :filter       (fn [n] (and
                            (re-matches #"[a-z][a-z0-9_]*" n)
                            (re-find #"_" n)))
    }
   {
    :description "contains a dot"
    :filter       (fn [n] (and
                            (re-find #"[.]" n)
                            (re-matches #"[a-zA-Z0-9-_.]*" n)))
    }
   {
    :description "caml case with no separators"
    :filter       (fn [n] (and
                            (re-matches #"[a-zA-Z][A-Za-z0-9]*" n)
                            (re-find #"[A-Z]" n)))
    }
   {
    :description "caml case with minus or underscore separators"
    :filter       (fn [n] (and
                            (re-matches #"[a-zA-Z][A-Za-z0-9-_]*" n)
                            (re-find #"[A-Z]" n)
                            (re-find #"[-_]" n)))
    }
   {
    :description "alphanumeric starting with a number"
    :filter       (fn [n] (re-matches #"[0-9][A-Za-z0-9-_]*" n))
    }
   {
    :description "alphanumeric starting with a minus or underscore"
    :filter       (fn [n] (re-matches #"[-_][A-Za-z0-9-_]*" n))
    }
   {
    :description "lowercase alphanumeric using both minus and underscore"
    :filter       (fn [n] (and
                            (re-matches #"[a-z][a-z0-9-_]*" n)
                            (re-find #"-" n)
                            (re-find #"_" n) ))
    }
   {
    :description "name containing charaters that are not alphanumeric, minus, underscore or dot"
    :filter       (fn [n] (re-find #"[^a-zA-Z0-9-_.]" n))
    }
   ]
  )

(defn stats
  "statistics on a name list"
  [names]
  (let [count-names (count names)
        matched-count (atom 0)]
    (printf "count, %s%n" count-names)
    (let [data (sort
        (fn [l r] (compare (:count r) (:count l)))
        (map
          (fn [{k :description v :filter}]
            (let [s (filter v names)
                  value (count s)]
              (swap! matched-count + value)
              {
               :count value
               :ratio (float (/ (* 100 value) count-names))
               :set (set s)
               :description k
               }))
          stat-filters))]
      (doall (map (fn [v]
                    (printf "%s, %s, %s%n" (:count v) (:ratio v) (:description v)))
                  data))
      (prn :matched @matched-count :missed (- count-names @matched-count))
      (pp/pprint (remove nil?
        (for [x data y data :when (not= x y)]
          (if-let [inter (seq (sets/intersection (:set x) (:set y)))]
            [ "intersect" (:description x) (:description y) inter]))))
      (pp/pprint (reduce sets/difference (set names) (map :set data)))
      )
    )
  )

(defn -main
  "fetch some github projet names"
  [& args]
  (let [{:keys [options arguments errors summary]} (parse-opts args cli-options)]
    (cond
      (:help options) (exit 0 summary)
      errors          (exit 1 (string/join \newline errors)))
    (let [since (:since options)
          start-url (str "https://api.github.com/repositories?since=" since)
          names
          (if-let [input-file (:input options)]
            (-> input-file slurp string/split-lines)
            (loop [aux []
                   url start-url]
              (let [[names next-url] (get-names url)
                    allnames (concat aux names)]
                (process-names names (:file options))
                (if (>= (count allnames) (:number options))
                  allnames
                  (recur allnames next-url)
                  )
                )))]
      (stats names)
      )
    )
  )

