(defproject karyotype-parser "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[uk.org.russet/tawny-owl "1.3.0"]
  				[org.clojure/clojure "1.6.0"]
  				[ncl.karyotype "1.0.0-SNAPSHOT"]]
  :main ^:skip-aot karyotype-parser.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
