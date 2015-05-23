(ns ^:slow tawny.reflect-test
  (:use [clojure.test]))

(defmacro eval-in-temp-ns [& forms]
  `(binding [*ns* *ns*]
     (in-ns (gensym))
     (clojure.core/use 'clojure.core)
     (eval
      '(do ~@forms))))

(defmacro with-err-print-writer
  "Evaluate with err pointing to a temporary PrintWriter, and
return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)
         p# (java.io.PrintWriter. s#)]
     (binding [*err* p#]
       ~@body
       (str s#))))

(defmacro with-err-string-writer
  "Evaluate with err pointing to a temporary StringWriter, and
return err contents as a string."
  [& body]
  `(let [s# (java.io.StringWriter.)]
     (binding [*err* s#]
       ~@body
       (str s#))))


(defmacro should-not-reflect
  "Turn on all warning flags, and test that reflection does not occur
as identified by messages to *err*."
  [form]
  `(binding [*warn-on-reflection* true]
     (is (nil? (re-find #"^Reflection warning" (with-err-string-writer (eval-in-temp-ns ~form)))))
     (is (nil? (re-find #"^Reflection warning" (with-err-print-writer (eval-in-temp-ns ~form)))))))

(deftest debug []
  (should-not-reflect
   (require 'tawny.debug :reload)))

(deftest emacs []
  (should-not-reflect
   (require 'tawny.emacs :reload)))

(deftest lookup []
  (should-not-reflect
   (require 'tawny.lookup :reload)))

(deftest obo []
  (should-not-reflect
   (require 'tawny.obo :reload)))

(deftest owl []
  (should-not-reflect
    (require 'tawny.owl :reload)))

(deftest pattern []
  (should-not-reflect
   (require 'tawny.pattern :reload)))

(deftest poly []
  (should-not-reflect
   (require 'tawny.polyglot :reload)))

(deftest profile []
  (should-not-reflect
   (require 'tawny.profile :reload)))

(deftest tawnyread []
  (should-not-reflect
   (require 'tawny.read :reload)))

(deftest reasoner []
  (should-not-reflect
    (require 'tawny.reasoner :reload)))

(deftest render []
  (should-not-reflect
    (require 'tawny.render :reload)))

(deftest repl []
  (should-not-reflect
    (require 'tawny.repl :reload)))

(deftest util []
  (should-not-reflect
   (require 'tawny.util :reload)))
