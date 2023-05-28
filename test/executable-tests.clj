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

(defn recreate-test-data-temp []
  (let [dir-temp "test-data-temp"]
    (shell {:dir "test"} (str "cp -rf test-data " dir-temp))
    dir-temp))

(defn -main [& args]
  (assert
    (= (last (str/split base-dir #"/")) "tree-surgeon")
    "Please run executable tests from tree-surgeon base directory")
  (build-tree-surgeon)
  (prn "Test that tree-surgeon can be used to filter a directory after copying")
  (recreate-test-data-temp)
  (let [args "to-bash -f 'nameEndsWith \".cpp\"' -s ./ | xargs rm"
        dir (recreate-test-data-temp)]
    (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
    (let [files (->
        (shell {:out :string :dir "test/test-data-temp"} "find ./ -iregex '.*\\.cpp'")
        :out)]
      (prn
        (if (= files "")
          "Test passed: No .cpp files found"
          (str "Test failed: Files " files " were found")))))
  (let [args "to-bash -f 'nameEndsWith \".tmp\" & ancestorNameIs \".cache\"' -s ./ | xargs rm"
        dir (recreate-test-data-temp)]
    (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
    (let [files (->
        (shell {:out :string :dir "test/test-data-temp"} "find ./ -iregex '.*/.cache.*/.*tmp'")
        :out)]
      (prn
        (if (= files "")
          "Test passed: No .*/.cache.*/.*tmp files found"
          (str "Test failed: Files " files " were found")))))
  (prn "Tests finished"))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

