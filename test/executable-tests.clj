#!/usr/bin/env bb

(require '[babashka.process :refer [pipeline shell process exec]])
(require '[clojure.string :refer [trim ends-with?]])

(defn build-tree-surgeon []
  (shell "cabal install --installdir=\"./test\""))

(defn get-base-dir []
  (prn (str "pwd is " (-> (shell {:out :string} "pwd") :out)))
  (-> (shell {:out :string} "pwd") :out trim ))

(defn get-executable-path []
  (str (get-base-dir) "/test/tree-surgeon"))

(defn get-bash-cmd [cmd-str]
  (let [exec (get-executable-path)]
    (str "bash -c \"" exec " " cmd-str "\"")))

(defn -main [& args]
  (build-tree-surgeon)
  (let [base-dir (get-base-dir) exec (get-executable-path)]
    ((prn (str "Base directory is " base-dir))
      (prn (str "Executable path is " exec))
      (prn "Test that tree-surgeon can be used to filter a directory after copying")
      (shell {:dir "test"} "cp -rf test-data test-data-temp")
      (shell {:dir "test/test-data-temp"} (get-bash-cmd "to-bash -f 'nameEndsWith \\\".cpp\\\"' -s ./ | xargs echo")))))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

