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
    (= (last (str/split base-dir #"/")) "src")
    "Please run executable tests from src directory")
  (build-tree-surgeon)
  (prn "Test that tree-surgeon can be used to filter a directory after copying")
  (recreate-test-data-temp)
  (let [args "to-bash -f 'endsWith \".cpp\" (basename file)' -s ./ | xargs rm"
        dir (recreate-test-data-temp)]
    (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
    (let [files (->
        (shell {:out :string :dir "test/test-data-temp"} "find ./ -iregex '.*\\.cpp'")
        :out)]
      (prn
        (if (= files "")
          "Test passed: No .cpp files found"
          (str "Test failed: Files " files " were found")))))
  (let [args "to-bash -f 'endsWith \".tmp\" (basename file) & elem \".cache\" (parents file)' -s ./ | xargs rm"
        dir (recreate-test-data-temp)]
    (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
    (let [files (->
        (shell {:out :string :dir "test/test-data-temp"} "find ./ -iregex '.*/.cache.*/.*tmp'")
        :out)]
      (prn
        (if (= files "")
          "Test passed: No .*/.cache.*/.*tmp files found"
          (str "Test failed: Files " files " were found")))))
  #_ (let [args "to-bash -f 'endsWith [\".cpp\", \".hpp\"]' -s ./ | xargs sed -i 's/Apple/Orange/g'"
  #_       dir (recreate-test-data-temp)]
  #_   (let [before-files (->
  #_       (shell {:out :string :dir "test/test-data-temp"} "grep -ir 'Apple'")
  #_       :out)]
  #_     (prn
  #_       (if (not= before-files "")
  #_         (do
  #_           (shell {:dir "test/test-data-temp"} (get-bash-cmd args))
  #_           (let [files (->
  #_               (shell {:out :string :continue :true :dir "test/test-data-temp"} "grep -ir 'Apple'")
  #_               :out)]
  #_             (if (= files "")
  #_               "Test passed: No files containing Apple found"
  #_               (str "Test failed: Files " files " were found"))))
  #_         "Test error: No files containing Apple were found in test data"))))
  (prn "Tests finished")
  (recreate-test-data-temp))

(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

