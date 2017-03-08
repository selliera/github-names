(ns github-names.core
  (:require
    [clj-http.client :as client]
    [clojure.string :as string]
    [clojure.data.json :as json]
    [clojure.tools.cli :refer [parse-opts]]
    ))

(defn- get-names
  "fetch some github project names from url"
  [url]
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
    (doall (map (fn [n] (spit "names.txt" (str n "\n") :append true)) names))
    (println "names" names)
    )
  )

(defn- anum-filter [n]
  (re-matches #"[a-z][a-z0-9-]*" n))

(defn- pascal-case-filter [n]
  (re-matches #"[A-Z][A-Za-z0-9-]*" n))

(defn- underscore-filter [n]
  (re-matches #"[a-z][a-z0-9_]*" n))

(defn stats
  "statistics on a name list"
  [names]
  (println "count" (count names))
  (let [alphanum-minus (-> (filter anum-filter names) count)
        pascal-case    (-> (filter pascal-case-filter names) count)
        underscore     (-> (filter underscore-filter names) count)]
    (println "alphanum" alphanum-minus)
    (println "pascal case" pascal-case)
    (println "underscore" underscore)
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

