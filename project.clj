(defproject NER "1.0.0-SNAPSHOT"
     :description "FIXME: NER in Clojure"
     :dependencies [
     			[org.clojure/clojure "1.4.0"]
     			[org.clojure/data.zip "0.1.1"]
     			[clojure-opennlp "0.1.9"]
     			[org.apache.opennlp/opennlp-tools "1.5.3-SNAPSHOT"]
     			[weissjeffm/clojure.prxml "1.3.0-SNAPSHOT"]
     	           ]
      			    
      :jvm-opts ["-Xmx1024m"]
      ;:main NER.core
      )
