(defproject cxl-cinema-backend "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "EPL-2.0 OR GPL-2.0-or-later WITH Classpath-exception-2.0"
            :url  "https://www.eclipse.org/legal/epl-2.0/"}
  :dependencies [[org.clojure/clojure "1.10.0"]
                 [http-kit "2.3.0"]
                 [org.clojure/tools.logging "1.0.0"]
                 [compojure "1.6.1"]
                 [org.clojure/data.json "1.0.0"]]
  :min-lein-version "2.9.3"
  :plugins [[environ/environ.lein "0.3.1"]]
  :hooks [environ.leiningen.hooks]
  :uberjar-name "my-app.jar"

  :main cxl-cinema-backend.core
  :repl-options {:init-ns cxl-cinema-backend.core})
