;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, 2013, Newcastle University

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see http://www.gnu.org/licenses/.


(ns tawny.owl-test
  (:import
   (org.semanticweb.owlapi.model OWLOntologyManager OWLOntology IRI
                                 OWLClassExpression OWLClass OWLAnnotation
                                 OWLIndividual OWLDatatype
                                 OWLNamedObject OWLOntologyID
                                 OWLAnnotationProperty OWLObjectProperty
                                 OWLDataProperty
                                 ))
  (:require [tawny.owl :as o]
            [tawny.util])
  (:require [tawny.debug])
  [:use clojure.test])


(def to nil)

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/" :prefix "iri"))))

(defn createandsavefixture[test]
  (let [exp
        #(throw (Exception. "default ontology used"))
        trace
        #(tawny.debug/tracing-println "default ontology used")
        ]
    (when true
      (tawny.util/add-hook
       o/default-ontology-hook exp
       ))
    (when false
      (tawny.util/add-hook
       o/default-ontology-hook trace))
    (createtestontology)
    (test)
    (tawny.util/remove-hook o/default-ontology-hook exp)
    (tawny.util/remove-hook o/default-ontology-hook trace)))

(use-fixtures :each createandsavefixture)

(deftest ontology
  (is true)
  (is
   (not
    (nil?
     (o/ontology
      :iri "http://iri/"
      :prefix "iri:"
      :comment "This is a comment"
      :versioinfo "This is some versioninfo")))))

(deftest get-iri
  (is (= "http://iri/"
         (.toString (o/get-iri (o/ontology :iri "http://iri/")))))
  (is (= "http://iri/"
         (.toString (o/get-iri to)))))

(deftest declare-classes
  (is
   (-> (o/declare-classes a :ontology to)
       (nil?)
       (not))))


(deftest ontology-options

  (is
   (instance? clojure.lang.Ref
              (o/ontology-options to)))

  (is
   (let [options {:a 1 :b 2}]
     (dosync
      (alter (o/ontology-options to)
             merge options))

     (= @(o/ontology-options to)
            options)))

  (is
   (do
     ;; need a clean slate to start with!
     (reset! o/ontology-options-atom {})
     (o/ontology :iri "http://iri" :prefix "dfda:")
     (o/ontology-options to)
     (o/ontology :iri "http://iri" :prefix "dfda:")
     (o/ontology-options to)
     (o/ontology :iri "http://iri" :prefix "dfda:")
     (o/ontology-options to)
     (= 1
          (count @o/ontology-options-atom)))))



(deftest save-ontology []
  (is (do (o/save-ontology to "test.omn")
          true)))

(deftest iriforname []
  (is (= (.toString (#'o/iriforname to "test"))
         "http://iri/#test")))

(deftest get-create-object-property []
  (is (instance? org.semanticweb.owlapi.model.OWLObjectProperty
                 (#'o/get-create-object-property to "hello")))
  (is (instance? org.semanticweb.owlapi.model.OWLObjectProperty
                 (#'o/get-create-object-property
                  to "hello"))))


(deftest ensure-object-property []
  (is
   ;; check whether it makes an object out of a string
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (#'o/ensure-object-property to "hello")))
  (is
   ;; check whether it makes keeps an object as an object
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (#'o/ensure-object-property to
               (#'o/get-create-object-property to "hello")))))

(deftest defoproperty
  (is
   (var?
    (o/defoproperty a :ontology to))))


(deftest oproperty
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (o/objectproperty to "r"))))

(deftest get-create-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (#'o/get-create-class to "hello")))
  (is  (=  (.hashCode (#'o/get-create-class to "hello"))
           (.hashCode (#'o/get-create-class to "hello")))))


(deftest ensure-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class to "hello")))
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class to (#'o/get-create-class to "hello")))))


(deftest add-subclass
  (is
   (do
     (o/add-subclass to "a" "b")
     (= 2 (.size (.getClassesInSignature
                  to)))))
  (is
   (do
     (o/add-subclass to "a" "b")
     (o/superclass? to "a" "b")))
  )

(deftest add-equivalent []
  (let [equiv (#'o/add-equivalent to
               (#'o/ensure-class to "a")
               (list (#'o/ensure-class to "b")))]
    (is (not (nil? equiv)))
))

(deftest add-class []
  (is (not
       (nil?
        (o/add-class to "a")))))

(deftest add-haskey
  (is
   (not (nil?
         (do
           (o/with-probe-entities
             [p (o/objectproperty to "p")
              c (o/owlclass to "c")]
             (o/add-haskey to c (list p))))))))

(deftest add-subpropertychain
  (is
   (not
    (nil?
     (do
       (let [p1 (o/objectproperty to "p1")
             p2 (o/objectproperty to "p2")
             p3 (o/objectproperty to "p3")
             ]
         (o/add-subpropertychain to
          p1 (list p2 p3)))))))

  (is
   (not
    (nil?
     (do
       (let [p1 (o/objectproperty to "p1")
             p2 (o/objectproperty to "p2")
             p3 (o/objectproperty to "p3")
             ]
         (o/add-subpropertychain to
          p1 (list [p2 p3])))))))

  (is
   (not
    (nil?
     (do (let [p1 (o/objectproperty to "p1")
               p2 (o/objectproperty to "p2")
               p3 (o/objectproperty to "p3")
               p4 (o/objectproperty to "p4")
               p5 (o/objectproperty to "p5")
             ]
         (o/add-subpropertychain to
          p1 (list p2 p3 [p4 p5])))))))
  )


(deftest objectproperty []
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectProperty
       (o/objectproperty to "b")))
  )

(deftest owlsome []
  (is (not (nil? (o/owlsome to (o/objectproperty to "b") "a"))))
  (is (o/object-some
       to "has" "leg"))
  ;; failing test
  (is (thrown? clojure.lang.ArityException
               (o/owlsome to (ensure-class to "hasLeg")))))

(deftest owlonly []
  (is (not (nil? (o/only to (o/objectproperty to "b") "a")))))


(deftest owland []
  (is (not (nil? (o/object-and to "a" "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectIntersectionOf
       (o/owland to "c" (o/owlclass to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owland to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataIntersectionOf
       (o/owland to :XSD_INTEGER :XSD_FLOAT))))

(deftest owlor
  (is (not (nil? (o/object-or to "a" "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectUnionOf
       (o/owlor to "c" (o/owlclass to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owlor to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/data-or to :XSD_INTEGER :XSD_FLOAT)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/owlor to :XSD_INTEGER :XSD_FLOAT))))

(deftest owlnot
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (do
         (o/owlclass "b" :ontology to)
         (o/owlnot to "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (o/owlnot to (o/owlclass "d" :ontology to))))
  (is (thrown? IllegalArgumentException
               (o/owlnot to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/data-not to :XSD_INTEGER)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/owlnot to :XSD_FLOAT)))
)

(deftest someonly []
  (is
   (not
    (nil?
     (o/someonly to
      (o/objectproperty "p" :ontology to) "a"))))

  (is
   (not
    (nil?
     (o/someonly to
                 (o/objectproperty "p" :ontology to) "a" "b")))))


(deftest data-some
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (first
               (o/owlsome to
                          (o/datatypeproperty "p" :ontology to)
                          :XSD_INTEGER)))))

(deftest data-some-datarange
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (first
               (o/data-some to
                            (o/datatypeproperty "p" :ontology to)
                            (o/span < 1))))))

(deftest disjointclasses []
  (is
   (do (#'o/disjointclasses to "a" "b" "c")))

  (is
   (do (#'o/disjointclasses to
        (o/owlclass "a" :ontology to) (o/owlclass "b" :ontology to)))))

(deftest owlclass
  (is (= 1
         (do (o/owlclass "test" :ontology to)
             (.size (.getClassesInSignature to)))))
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (o/owlclass "test" :ontology to))))


(deftest defclass
  (is (= 1
         (do (o/defclass a :ontology to)
             (.size (.getClassesInSignature to))))))



(defn- test-class-with-hierarchy
  "Some test classes

Assumes that fixture has been run
"
  []

  (o/owlclass "a" :ontology to)
  (o/owlclass "b" :subclass "a" :ontology to)
  (o/owlclass "c" :subclass "b" :ontology to)

  (o/owlclass "d" :ontology to)
  (o/owlclass "e" :subclass "b" "d" :ontology to)
  )

(deftest superclass? []
  (is (not
       (nil?
        (do
          (test-class-with-hierarchy)
          (o/direct-superclasses to "c")))))
  (is (do
        (test-class-with-hierarchy)
        (o/superclass? to "e" "a")))
  (is (not
       (do
         (test-class-with-hierarchy)
         (o/superclass? to "c" "e")))))


(deftest subclass? []
  (is
   (do (test-class-with-hierarchy)
       (o/subclass? to "a" "c")))
  (is
   (not
    (do (test-class-with-hierarchy)
        (o/subclass? to "c" "e")))))


(deftest disjointclasses []
  (is (not (nil? (#'o/disjointclasses to "a" "b" "c")))))

(deftest individual []
  (is (o/individual to "ind"))
  (is (not (nil? (o/individual to "indA" :type "a"))))
  (is (thrown? IllegalArgumentException
               (o/individual to "indA" :nottypes "a"))))

(deftest defindividual []
  (is (do (o/defindividual testind :ontology to)
          testind)))

(deftest remove-entity []
  (is
   (= 0
      (do
        (let [clazz (o/owlclass to "a")]
          (o/remove-entity to clazz)
          (.size (.getClassesInSignature to))))))

  (is
   (= 0
      (do
        (let [prop (o/objectproperty to "a")]
          (o/remove-entity to prop)
          (.size (.getClassesInSignature to)))))))



(deftest with-probe-entities
  ;; are the classes created correctly
  (is
   (= 3
      (o/with-probe-entities to
        [a (o/owlclass to "a")
         b (o/owlclass to "b")
         c (o/owlclass to "c")]
        (-> to
            (.getClassesInSignature)
            (.size))
        )))

  (is
   (= 0
      (do
        (o/with-probe-entities to
            [a (o/owlclass to "a")
             b (o/owlclass to "b")
             c (o/owlclass to "c")
             ])
        ;; and have they gone again afterwards
        (-> to
            (.getClassesInSignature)
            (.size))))))


(defn ontology-c-with-two-parents []
  (o/owlclass to "a")
  (o/owlclass to "b")
  (o/owlclass to "c" :subclass "a" "b"))


(deftest with-probe-axioms
  ;; add a disjoint see whether it breaks
  (is
   (= 1
      (do
        (ontology-c-with-two-parents)
        (o/with-probe-axioms
          [a (#'o/disjointclasses to "a" "b")]
          (-> to
              (.getDisjointClassesAxioms
               (o/owlclass to "a"))
              (.size))))))

  ;; add a disjoint test whether it breaks after
  (is
   (= 0
      (do
        (ontology-c-with-two-parents)
        (o/with-probe-axioms to
          [a (#'o/disjointclasses to "a" "b")])

        (-> to
            (.getDisjointClassesAxioms
             (o/owlclass to "a"))
            (.size))))))

(deftest owlimport
  (is
   (not
    (nil?
     (o/owlimport to to)))))


(deftest annotation
  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/label to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/owlcomment to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/isdefinedby to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/seealso to "hello")))
  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/backwardcompatiblewith to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/incompatiblewith to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotationProperty
              (o/annotation-property to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/annotation to
               (o/annotation-property to "hello")
               "hello1"))))



(deftest add-annotation
  (is
   (not
    (nil? (#'o/add-annotation to
           (o/owlclass to "a")
           (list (o/owlcomment to "comment"))))))

  (is
   (not
    (nil? (#'o/add-annotation
           to
           (list (o/owlcomment to "comment")))))))

(deftest add-annotation2
  (is
   (=
    "hello"
    (do
      (let [b (o/owlclass to "b")]
        (o/add-annotation to
         b (list (o/label to "hello")))
        (.getLiteral
         (.getValue
          (first
           (filter
            #(-> %
                 (.getProperty)
                 (.isLabel))
            (.getAnnotations b to))))))))))


(deftest dataproperty
  (is (instance? org.semanticweb.owlapi.model.OWLDataProperty 
                 (o/datatypeproperty "hello" :ontology to))))



(deftest disjoint?
  (is
   (let [a (o/owlclass to "a")
         b (o/owlclass to "b")]
     (#'o/disjointclasses to a b)
     (o/disjoint? to a b))))



;; TODO lots of macros are in serious need of a test

(deftest as-subclasses
  (is
   (let [x (o/owlclass to "x")]
     (o/as-subclasses to
      x
      (o/owlclass to "y")
      (o/owlclass to "z"))

     ;; now for the test
     (and (o/superclass? to
           (o/owlclass to "y")
           x)
          (o/superclass? to
           (o/owlclass to "z")
           x))))
  (is
   (let [x (o/owlclass to "x")]
     (o/as-subclasses to
      x :disjoint
      (o/owlclass to "y")
      (o/owlclass to "z"))
     (o/disjoint? to
      (o/owlclass to "y")
      (o/owlclass to "z"))))

  (is
   (let [x (o/owlclass to "x")]
        (o/as-subclasses to
         x :cover
         (o/owlclass to "y")
         (o/owlclass to "z"))

        (o/equivalent?
         to
         (o/owlclass to "x")
         (o/owlor to (o/owlclass to "z")
                  (o/owlclass to "y")))))


  (is
   (let [x (o/owlclass to "x")]
     (o/as-subclasses to
      x :cover
      (o/owlclass to "y")
      (o/owlclass to "z"))

     (and
      (o/disjoint? to
       (o/owlclass to "y")
       (o/owlclass to "z"))

      (o/equivalent? to
       (o/owlclass to "x")
       (o/owlor to (o/owlclass to "z")
                (o/owlclass to "y")))))))


(deftest as-disjoint-subclasses
  (is
   (let [x (o/owlclass to "x")]
     (o/as-disjoint-subclasses to
      x
      (o/owlclass to "y")
      (o/owlclass to "z"))

     (o/save-ontology to "to.omn" :omn)

     (and
      (o/disjoint? to
       (o/owlclass to "y")
       (o/owlclass to "z"))
      
      (o/superclass? to
       (o/owlclass to "y")
       x)

      (o/superclass? to
       (o/owlclass to "z")
       x)))))

(deftest prefix-suffix-symbol
  (is (= 'helloworld
         (#'o/prefix-symbol "hello" 'world)))
  (is (= 'helloworld
         (#'o/suffix-symbol "world" 'hello))))


(deftest guess
  (is (= :object
         (o/guess-type to
          (o/owlclass to "a"))))

  (is (= :annotation
         (o/guess-type to
          (o/annotation-property to "b"))))

  (is (= :object
         (do
           (o/owlclass to "c")
           (o/guess-type to "c"))))


  (is (= :object
         (o/guess-type to
          (list (o/owlclass to "d") "e" "f"))))

  (is (= :object
         (o/guess-type to
          (list "e" "f" (o/owlclass to "d"))))))


(deftest veggiepizza
  (is (= :object
         (do
           (o/guess-type to
            (o/with-probe-entities
              [r (o/objectproperty to "hasTopping")
               c (o/owlclass to "MeatTopping")]
              (o/owlnot to
               (o/owlsome to r c))))))
      "A regression tester from the pizza ontology"))

(deftest oneof
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLObjectOneOf
    (o/oneof to (o/individual to "a"))))

  (is
   (instance?
    org.semanticweb.owlapi.model.OWLDataOneOf
    (o/oneof to (o/literal to "hello")))))


(deftest add-different
  (is
   (let
       [i1 (o/individual to "i1")
        i2 (o/individual to "i2")]
     (o/add-different to i1 i2)
     (some #{i2}
           (.getDifferentIndividuals i1 to)))))


(deftest add-data-super
  (is
   (o/with-probe-entities
     [dp (o/datatypeproperty to "a")
      sdp (o/datatypeproperty to "b" :subproperty dp)]
     true))
  (is
   (every? #(instance?
             org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom %1)
           (o/with-probe-entities
             [dp (o/datatypeproperty to "a")
              sdp (o/datatypeproperty to "b")]
             (o/add-data-superproperty
              to
              dp sdp)))))

(deftest hasself
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectHasSelf
              (o/hasself to (o/objectproperty to "r")))))

(deftest owlmin
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owlmin 10))))

(deftest owlmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owlmax 10))))

(deftest minmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/minmax 10 10))))

(deftest mininc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/mininc 10))))

(deftest maxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/maxinc 10))))

(deftest minmaxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/minmaxinc 10 20))))

(deftest literal
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal to "bob" :lang "en")))
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal to "bob")))
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal to "bob" :type :RDF_PLAIN_LITERAL))))


(deftest owlcomment
  (is
   (instance?
    OWLAnnotation
    (o/owlcomment to
     (o/literal to "comment"
                 :type :RDF_PLAIN_LITERAL)))))
