(def project 'adventofcode)
(def version "1.0.0-SNAPSHOT")

(set-env! :resource-paths #{"resources" "src"}
          :source-paths   #{"test"}
          :dependencies   '[[org.clojure/clojure "RELEASE"]
                            [adzerk/boot-test "RELEASE" :scope "test"]])

(deftask run
  "Run the project."
  [d day INT int "day to run"]
  (require (read-string (format "[adventofcode.day%s :as app]" day)))
  (apply (resolve 'app/-main) []))

(require '[adzerk.boot-test :refer [test]])
