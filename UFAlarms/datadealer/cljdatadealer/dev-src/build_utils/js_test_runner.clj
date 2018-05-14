(ns build-utils.js-test-runner
  (:require [clojure.java.shell :as sh]
            [clojure.tools.logging :refer :all]
            [utils.fixtures :as fixtures]))

(def api-dir
  "../../api_service/ui_api")

(def set-nvm
  "[ -f ~/.nvm/nvm.sh ] && . ~/.nvm/nvm.sh && nvm use stable; ")

(defn -main
  [& args]
  (println "Starting test fixtures.")
  (fixtures/-main)

  (println "Running `npm install`")
  (let [{:keys [exit]
         :as shelled} (sh/sh "/bin/sh" "-c"
                             (str set-nvm
                                  "npm install")
                             :dir api-dir)]
    (when-not (zero? exit)
      (println "Encountered error. See logs for details.")
      (error (ex-info "npm install failed?"
                      shelled))
      (System/exit 1)))

  (let [mocha-args (or (and args
                            (apply str (interpose " " args)))
                       "--recursive --timeout 15000")
        command (str set-nvm
                     "time mocha " mocha-args)]
    (println "Running mocha tests. This may take a while. (Arguments to mocha are: \""
             mocha-args
             "\")")
    (let [{:keys [exit out err]
           :as shelled} (sh/sh "/bin/sh" "-c"
                               command
                               :dir api-dir)]
      (when-not (zero? exit)
        (println "STDOUT:")
        (println out)
        (println "STDERR:")
        (binding [*out* *err*]
          (println err))
        (System/exit exit))))

  (println "JS tests passed successfully.")
  (System/exit 0))
