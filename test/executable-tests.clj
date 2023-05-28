#!/usr/bin/env bb

(require '[babashka.process :refer [pipeline shell process exec]])

(defn build-tree-surgeon []
  (shell "cabal install --installdir=\"./test\" --overwrite-policy=always"))

(def base-dir
  (-> (shell {:out :string} "pwd") :out str/trim ))

(def executable-path
  (str base-dir "/test/tree-surgeon"))

(defn nest-quotes [input-str]
  (str/replace input-str "\"" "\\\""))

(defn get-bash-cmd [cmd-str]
  (str "bash -c \"" executable-path " " (nest-quotes cmd-str) "\""))

(defn -main [& args]
  (assert
    (= (last (str/split base-dir #"/")) "tree-surgeon")
    "Please run executable tests from tree-surgeon base directory")
  (build-tree-surgeon)
  (prn "Test that tree-surgeon can be used to filter a directory after copying")
  (shell {:dir "test"} "cp -rf test-data test-data-temp")
  (let [args "to-bash -f 'nameEndsWith \".cpp\"' -s ./ | xargs rm"]
    (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
    (let [cpp-files (->
        (shell {:out :string :dir "test/test-data-temp"} "find ./ -iregex '.*\\.cpp'")
        :out)]
      (prn
        (if (= cpp-files "")
          "Test passed: No .cpp files found"
          (str "Test failed: Files " cpp-files " were found")))))
  (prn "Tests finished"))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

