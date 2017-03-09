(ns github-names.core
  (:require
    [clj-http.client :as client]
    [clojure.string :as string]
    [clojure.set :as sets]
    [clojure.pprint :as pp]
    [clojure.data.json :as json]
    [clojure.tools.cli :refer [parse-opts]]
    ))

(defn- get-names
  "fetch some github project names from url"
  [url]
  (println url)
  (let [response (client/get url)
        status (:status response)
        headers (:headers response)
        next-url (-> response :links :next :href)
        body (-> response :body json/read-str)]
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
  {
   :alphanum      (fn [n] (re-matches #"[a-z][a-z0-9]*" n)),
   :anum-w-minus  (fn [n] (and
                            (re-matches #"[a-z][a-z0-9-]*" n)
                            (re-find #"-" n))),
   :underscore    (fn [n] (and
                            (re-matches #"[a-z][a-z0-9_]*" n)
                            (re-find #"_" n))),
   :contain-dot   (fn [n] (and
                            (re-find #"[.]" n)
                            (re-matches #"[a-zA-Z0-9-_.]*" n))),
   :caml-case     (fn [n] (and
                            (re-matches #"[a-zA-Z][A-Za-z0-9]*" n)
                            (re-find #"[A-Z]" n))),
   :caml-case-sep (fn [n] (and
                            (re-matches #"[a-zA-Z][A-Za-z0-9-_]*" n)
                            (re-find #"[A-Z]" n)
                            (re-find #"[-_]" n))),
   :numeric-start (fn [n] (re-matches #"[0-9][A-Za-z0-9-_]*" n)),
   :sep-start     (fn [n] (re-matches #"[-_][A-Za-z0-9-_]*" n)),
   :mix-chars'-_' (fn [n] (and
                            (re-matches #"[a-z][a-z0-9-_]*" n)
                            (re-find #"-" n)
                            (re-find #"_" n) )),
   :strange-chars (fn [n] (re-find #"[^a-zA-Z0-9-_.]" n)),
   }
  )

(defn stats
  "statistics on a name list"
  [names]
  (let [count-names (count names)
        matched-count (atom 0)]
    (prn :count count-names)
    (pp/pprint
      (map
        (fn [[k v]]
          (let [value (-> (filter v names) count)]
            (swap! matched-count + value)
            {
             :key k
             :count value
             :ratio (float (/ (* 100 value) count-names))
             }))
        stat-filters)
      )
    (prn :matched @matched-count :missed (- count-names @matched-count))

    (let [sets
          (map (fn [[k v]]
                 (let [value (filter v names)]
                   {
                    :key k
                    :set (set value)
                    })) stat-filters)]
      (pp/pprint (remove nil?
        (for [x sets y sets :when (not= x y)]
          (if-let [inter (seq (sets/intersection (:set x) (:set y)))]
            [ "intersect" (:key x) (:key y) inter]))))
      (pp/pprint (reduce sets/difference (set names) (map :set sets)))
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

