(ns NER.core
     (:use     [clojure.pprint]
               [clojure.inspector :only [inspect-tree, inspect]] )
     (:require [opennlp.nlp           :as nlp]
               [opennlp.treebank      :as trbank]
               [opennlp.tools.filters :as filt]
               [opennlp.tools.train   :as trainer]
               [opennlp.tools.lazy    :as laz]
               [clojure.zip           :as zip]
               [clojure.data.zip.xml  :as zxml]
               [clojure.data.zip      :as zf]  
               [clojure.xml           :as xml]
               [clojure.string        :as strutil]
               [clojure.prxml         :as easy-xml])
     (:import  (opennlp.tools.cmdline.dictionary DictionaryBuilderTool)
               (opennlp.tools.cmdline.namefind NameEvaluationErrorListener
                                               TokenNameFinderDetailedFMeasureListener)
               (opennlp.tools.dictionary  Dictionary)
     	       (opennlp.tools.namefind    DictionaryNameFinder2
     	                                  TokenNameFinder
     	                                  AggregateNameFinder 
     	                                  TokenNameFinderEvaluator
     	                                  TokenNameFinderCrossValidator
     	                                  TokenNameFinderModel
     	                                  NameSampleDataStream
     	                                  TokenNameFinderEvaluationMonitor 
     	                                  NameFinderME)
     	       (opennlp.tools.util        Span
     	                                  StringList
     	                                  PlainTextByLineStream
     	                                  TrainingParameters)
     	       ))
     
(def sentence-detector (nlp/make-sentence-detector "models/V1.5/en-sent.bin"));memoize it if it's called frequently
(def tokenizer      (nlp/make-tokenizer            "models/V1.5/en-token.bin"))
(def Mdrug-find     (nlp/make-name-finder          "models/FINALLY4.bin"))
(def pos-tagger     (nlp/make-pos-tagger           "models/V1.5/en-pos-maxent.bin"))
(def chunker        (trbank/make-treebank-chunker  "models/V1.5/en-chunker.bin"))
(def detokenize     (nlp/make-detokenizer          "models/english-detokenizer.xml"))
(def orig-dictionary  "dictionaries/DRUGBANK-OPENNLP.xml")
(def mod-dictionary   "dictionaries/PHARMA-OPENNLP.xml")
(def drugbank-list        "dictionaries/CRAP/ORIGINAL-DIC.TXT")
(def ^:dynamic *paper*   (slurp "small-test.txt"))
     
;(def newData1 "XML/invt_all_mastertagged.xml")
;(def newData2 "XML/invivo_all_mastertagged.xml")
(def dictionary (Dictionary.  
                (java.io.FileInputStream. orig-dictionary) "drug"))
;(def dictionary2 (Dictionary.  
                ; (java.io.FileInputStream. "dictionaries/dic2.xml") "protein"))
     
(def dictionary-recogniser (DictionaryNameFinder. (into-array Dictionary [dictionary])))
     
     
(defn regexize ^String [ch]
  (cond (zero? (compare ch \[)) "\\["
        (zero? (compare ch \()) "\\("
  	(zero? (compare ch \))) "\\)"
 	(zero? (compare ch \+)) "\\+"
 	(zero? (compare ch \^)) "\\^"
  	(zero? (compare ch \.)) "\\."
        (zero? (compare ch \$)) "\\$"
 	(zero? (compare ch \*)) "\\*"
 	(zero? (compare ch \|)) "\\|"
 	(zero? (compare ch \\)) "\\\\" ))  
     
  
(defn zip-str [s];parses an .xml file and returns a zip structure
  (zip/xml-zip  (xml/parse 
  (java.io.ByteArrayInputStream. (.getBytes  s))))) 
     
(defn parse-str [s];same as above using different input stream
  (zip/xml-zip (xml/parse 
  (org.xml.sax.InputSource. (java.io.StringReader. s)))))
      		    

(defn add-1-entry [^Dictionary original
                    ^String modified
                    & tokens]
 (do (.put original (StringList.  (into-array tokens))) 
     (println "\nEntry was added successfully...") 
     (.serialize original (java.io.FileOutputStream. modified))  
     (println "Serializing...\nDone!\n\nLook for" (str modified ".xml in your classpath"))))
  
  
(defn  un-capitalize [^String s] 
  (let [Cfirst (subs s 0 1)
        Crest  (subs s 1) ]
  (str (.toLowerCase Cfirst) Crest)))
  
  
(defn validate-finder 
[model test-data details] 
  (if model (println "\nEvaluating Model on test-data:\n------------------------------")
            (println "\nEvaluating Dictionary on test-data:\n-----------------------------------"))
  (let [judge     (TokenNameFinderEvaluator.
                  (if model (NameFinderME.
                             (TokenNameFinderModel. 
                             (java.io.FileInputStream. model))) 
                   dictionary-recogniser) (when details (into-array TokenNameFinderEvaluationMonitor 
                                                        [(NameEvaluationErrorListener.)])))
         _         (time (.evaluate judge
                   (NameSampleDataStream.
                   (PlainTextByLineStream.  
                   (java.io.FileInputStream. test-data) "UTF-8"))))          
         result    (.getFMeasure judge)]
   (do (println   "\nSTATISTICS FOLLOW:\n") 
       (println    (str result) "\n"))))
       
       
(defn validate-aggregate 
[model test-data details] 
  (println "\nEvaluating combined name-finder on test-data:\n------------------------------") 
  (let [judge     (TokenNameFinderEvaluator.
                  (AggregateNameFinder. (into-array TokenNameFinder
                                         [(NameFinderME.
                                         (TokenNameFinderModel. 
                                         (java.io.FileInputStream. model))) dictionary-recogniser]))
                                         (when details (into-array TokenNameFinderEvaluationMonitor 
                                                        [(NameEvaluationErrorListener.)])))
                 
         _         (time (.evaluate judge
                   (NameSampleDataStream.
                   (PlainTextByLineStream.  
                   (java.io.FileInputStream. test-data) "UTF-8"))))          
         result    (.getFMeasure judge)]
   (do (println   "\nSTATISTICS FOLLOW:\n") 
       (println    (str result) "\n"))))   
       
 
(defn cross-validate 
[train-data k details]
  (println (str "\nInitiating " k "-fold Cross-Validation:\n----------------------------------"))
  (let [judge (TokenNameFinderCrossValidator. "en" 
                                                nil
                                                (TrainingParameters.)
                                                nil
                                                (java.util.HashMap.) ;empty map does the trick!
                                                (when details (into-array TokenNameFinderEvaluationMonitor 
                                                              [(NameEvaluationErrorListener.)
                                                              (TokenNameFinderDetailedFMeasureListener.)])))
         _             (.evaluate judge (NameSampleDataStream.
                                        (PlainTextByLineStream. 
                                        (.getChannel (java.io.FileInputStream. train-data)) "UTF-8")) k)       
         result  (.getFMeasure judge)]
    (do (println   "\nSTATISTICS FOLLOW:\n") 
        (println       (str result) "\n"))))  		    
     
     
(defn annotate ^String
  "Overloaded function. 
  First one takes some text and a single entity name 
  and annotates the text with openNLP compatible sgml tags for that entity.
  Second one loops through all the entries in the dictionary."
([^String ele ^String text]
  	 (.replaceAll 
        	(re-matcher 
                (re-pattern (str "(?i)\\b+" (java.util.regex.Pattern/quote ele) "+\\b")) text)  
             (str "<START:drug> " (if (Character/isUpperCase (first ele)) (un-capitalize ele) ele) " <END>")))
([^String text] 
           (loop [s     text
                  names (map #(strutil/escape (.trim %) regexize)
                                              (.split (slurp drugbank-list) "\n"))]
  	     (if-let [name1 (first names)] 
           (recur (try (do (println (str "ANNOTATING " name1)) ;then clause
                             (annotate name1 s)) 
             (catch java.util.regex.PatternSyntaxException _ 
             (do (println (str "--- CANNOT BE PROCESSED! --->" name1)) s)))
             (rest names)) 
                          s)))  ) ;else clause        
             
                                                 
 
     
(defn xmlize [t] ;produce appropriate xml tag for a given token
 (with-out-str            ;keep the string instead of displaying it in sdout
 (easy-xml/prxml
       [:entry 
       [:token t]]))) ;<entry><token> t </token></entry>
        
    
   (comment  (defn produceDict [] ;produce dictionary from a .txt drug list - only works for single-word entities
     (map (comp xmlize #(.trim %)) 
     (.split (slurp drug-list) "\n")))
   )
          
       
(defn produce-dictionary [^String inf 
                          ^String outf]
 (let [builder (DictionaryBuilderTool.)]
  (do (println "Generating compatible Dictionary...\n")
   (.run builder (into-array String ["-inputFile"  inf 
                                      "-outputFile" outf]))
     (println "Done!"))))
       
(defn train-serialize [^String train-data 
                             ^String model-file] 
 (trainer/write-model 
 (trainer/train-name-finder "en" train-data 100 5) model-file))
                
                     
(defn  convert-spans 
 ^"[java.util.String" [span-array token-array];returns array of Strings
 (Span/spansToStrings span-array token-array))  
                
              
(defn  Ddrug-find ^"[java.util.String" [s]
(let [token-array (into-array String s)] 
 (convert-spans
 (.find dictionary-recogniser token-array) token-array))) ;;find() expects array of strings
     
(defn find-names [text fmethod];the 2 methods are "Mdrug-find" --> maxent model                                       
(distinct                                        ;"Ddrug-find" --> dictionary lookup
(mapcat fmethod  
   (laz/lazy-tokenize (sentence-detector (slurp text)) tokenizer))))
      

(defn find-both [s] ;; #{(find-names s Ddrug-find) (find-names s Mdrug-find)}
  (println "Dictionary lookup:-----------------")
            (pprint (find-names s Ddrug-find))
  (println "Maxent model deployment:---------------")
            (pprint (find-names s Mdrug-find)))
                   
;;---version with futures:
;(let [dl (future (find-names s Ddrug-find))
;             mm (future (find-names s Mdrug-find))]
;             (do (println "Dictionary lookup:-------------")
;                 (pprint @dl)
;                 (println "MaXent model deployment:------------")
;                 (pprint @mm)))
                         
      		    
(defn fetch-names [fname attr-value];extracts names from a zip structure
(let [data     (slurp fname)
      zipper   (zip-str data)
      entities (->> (zxml/xml-> zipper :article);drug
               (mapcat (juxt #(zxml/xml-> % :title :sentence :cons [(zxml/attr= :sem attr-value)] zxml/text) 
           	             #(zxml/xml-> % :abstract :sentence :cons [(zxml/attr= :sem attr-value)] zxml/text)
           	             #(zxml/xml-> % :abstract :annotation :sentence :cons [(zxml/attr= :sem attr-value)] zxml/text))))
          ]
           entities)) ;return entities
           
(defn fetch-sentences [fname];extracts sentences from a zip structure
(let [data    (slurp fname)
      zipper  (zip-str data)
      sentences  (->> (zxml/xml-> zipper :article)
           	      (mapcat (juxt  #(zxml/xml1-> % :title :sentence zxml/text)
           	                     #(zxml/xml-> %  :abstract :sentence  zxml/text)
           	                     #(zxml/xml-> %  :abstract :annotation :sentence  zxml/text))))] 
           	                     
           sentences));return sentences
                 
(defn save [input output];writes "input" (a data sructure) to a file called "output" using pprint.   
 (with-open [wrtr (clojure.java.io/writer  output)]
    (binding [*out* wrtr]
        (pprint input))))   
      
         
     
(defn process [s];main workflow
 (->> (laz/lazy-get-sentences s sentence-detector) 
      (map (comp (laz/lazy-chunk chunker)
                 (laz/lazy-tag  pos-tagger) 
     		 (laz/lazy-tokenize tokenizer)))))
      
(defn process-small [inf outf] ;load file to memory as a single string (for files less than 100MB)
 (with-open [out (clojure.java.io/writer  outf)]
  (binding  [*out* out]
    (pprint (process (slurp inf))))))
     
(defn process-big [input output]  
 (with-open [in  (clojure.java.io/reader    input)
             out (clojure.java.io/writer   output)]
      (binding [*out* out]                 
      (doseq [line (line-seq in)] 
                   (pprint (process line)))))) 	
                                    
                   
(defn noun-strings [s] 
(trbank/phrase-strings  (filt/noun-phrases (process s)))) 
      
     
     
(defn -main [] ;(validate-finder nil "small.txt" true)) 
               ;(validate-finder nil "PROPER-TRAIN/PROPER-TEST-ALL4.txt" true) )          
               ;(train-serialize "PROPER-TRAIN/MERGED-PROPER-MAXENT.txt" "models/FINALLY5.bin"))         
	       ;(cross-validate "PROPER-TRAIN/MERGED-PROPER-MAXENT.txt" 10))
	       ;(validate "models/FINALLY5.bin" "PROPER-TRAIN/MERGED-PROPER-MAXENT.txt"))
;------------------------------------------------------------------------------------------------
;---------- some fun and games here -------------------------------------------------------------

(defn find-names-swing [text fmethod]
  (let [findings (distinct
                 (mapcat fmethod  
                 (laz/lazy-tokenize (sentence-detector (slurp text)) tokenizer)))
        total    (count findings)]
   (inspect      (zipmap (range (inc total)) findings))))
   
 (defn find-both-swing [s view]
 (let [d (find-names s Ddrug-find)
       m (find-names s Mdrug-find)
       b (distinct (concat d m))]
 (inspect-tree {:Dictionary d 
                :MaXent     m
                :Overall    b}))) 
	
(def giannis-demo
(let [ s "PROPER-TRAIN/PROPER-TEST.txt"
       d (find-names s Ddrug-find)
       m (find-names s Mdrug-find)
       b (distinct (concat d m))]
       {:Dictionary d 
        :MaXent     m
        :Both       b}))	
			                                      
   (comment    (defn fetch-names [fname] 
      (zxml/xml-> (zip-str (slurp fname)) 
                    :drug :name zxml/text))
                    
(defn fetch-synonyms [fname] 
  (zxml/xml-> (zip-str (slurp fname)) 
               :drug :name zxml/text))  ) 					                                      
     

		   

