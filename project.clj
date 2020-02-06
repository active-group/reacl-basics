(defproject de.active-group/reacl-basics "0.1.2-SNAPSHOT"
  :description "Reusable Reacl classes, auxiliary functions and macros."
  
  :url "https://github.com/active-group/reacl-basics"
  
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [org.clojure/clojurescript "1.10.238"]
                 [active-clojure "0.26.0"]
                 [reacl "2.2.0"]
                 [cljs-ajax "0.8.0" :scope "provided"]
                 [clout "2.2.1"]
                 [com.cemerick/url "0.1.1"]
                 [venantius/accountant "0.2.5"]]

  :plugins [[lein-cljsbuild "1.1.7"]]

  :profiles {:dev {:plugins [[lein-codox "0.10.5"]]
                   :dependencies [[codox-theme-rdash "0.1.2"]
                                  [com.bhauman/figwheel-main "0.2.0"]
                                  [com.bhauman/rebel-readline-cljs "0.1.4"]]

                   :codox {:themes [:rdash]
                           :metadata {:doc/format :markdown}
                           :language :clojurescript}
                   
                   :resource-paths ["target" "resources"]}

             :test {:source-paths ["src" "test"]}}

  :source-paths ["src"]

  :clean-targets ^{:protect false} ["target"]

  ;; open http://localhost:9500/figwheel-extra-main/auto-testing for the tests.
  :aliases {"fig" ["trampoline" "with-profile" "+dev,+test" "run" "-m" "figwheel.main" "-b" "dev" "-r"]}

  :cljsbuild {:builds []}
  )
