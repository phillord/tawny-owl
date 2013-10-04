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

  ;; create the same ontology several times, and check that ontology-options
  ;; is not growing
  (is
   (do
     ;; need a clean slate to start with!
     (reset! o/ontology-options-atom {})
     (o/ontology :iri "http://iri/some-more" :prefix "dfda:" :noname true)
     (o/ontology :iri "http://iri/some-more" :prefix "dfda:" :noname true)
     (o/ontology :iri "http://iri/some-more" :prefix "dfda:" :noname true)
     (= 1
          (count @o/ontology-options-atom)))))



(deftest save-ontology []
  (is (do (o/save-ontology to "test.omn")
          true))
  (is (do (o/save-ontology
           (o/ontology :name "bob") "test2.omn")
          true)))

(deftest iri-for-name []
  (is (= (.toString (#'o/iri-for-name to "test"))
         "http://iri/#test")))

(deftest ensure-object-property []
  (is
   ;; check whether it makes an object out of a string
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (#'o/ensure-object-property to "hello"))))

(deftest defoproperty
  (is
   (var?
    (o/defoproperty a :ontology to))))


(deftest oproperty
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (o/object-property to "r"))))

(deftest ensure-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (#'o/ensure-class to "hello"))))


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

(deftest add-has-key
  (is
   (not (nil?
         (do
           (o/with-probe-entities to
             [p (o/object-property to "p")
              c (o/owl-class to "c")]
             (o/add-has-key to c (list p))))))))

(deftest add-subpropertychain
  (is
   (not
    (nil?
     (do
       (let [p1 (o/object-property to "p1")
             p2 (o/object-property to "p2")
             p3 (o/object-property to "p3")
             ]
         (o/add-subpropertychain to
          p1 (list p2 p3)))))))

  (is
   (not
    (nil?
     (do
       (let [p1 (o/object-property to "p1")
             p2 (o/object-property to "p2")
             p3 (o/object-property to "p3")
             ]
         (o/add-subpropertychain to
          p1 (list [p2 p3])))))))

  (is
   (not
    (nil?
     (do (let [p1 (o/object-property to "p1")
               p2 (o/object-property to "p2")
               p3 (o/object-property to "p3")
               p4 (o/object-property to "p4")
               p5 (o/object-property to "p5")
             ]
         (o/add-subpropertychain to
          p1 (list p2 p3 [p4 p5])))))))
  )


(deftest object-property []
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectProperty
       (o/object-property to "b")))
  )

(deftest owl-some []
  (is (not (nil? (o/owl-some to (o/object-property to "b") "a"))))
  (is (o/object-some
       to "has" "leg"))
  ;; failing test
  (is (thrown? clojure.lang.ArityException
               (o/owl-some to (ensure-class to "hasLeg")))))

(deftest owlonly []
  (is (not (nil? (o/only to (o/object-property to "b") "a")))))


(deftest owl-and []
  (is (not (nil? (o/object-and to "a" "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectIntersectionOf
       (o/owl-and to "c" (o/owl-class to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owl-and to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataIntersectionOf
       (o/owl-and to :XSD_INTEGER :XSD_FLOAT))))

(deftest owl-or
  (is (not (nil? (o/object-or to "a" "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectUnionOf
       (o/owl-or to "c" (o/owl-class to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owl-or to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/data-or to :XSD_INTEGER :XSD_FLOAT)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/owl-or to :XSD_INTEGER :XSD_FLOAT))))

(deftest owl-not
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (do
         (o/owl-class "b" :ontology to)
         (o/owl-not to "b"))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (o/owl-not to (o/owl-class "d" :ontology to))))
  (is (thrown? IllegalArgumentException
               (o/owl-not to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/data-not to :XSD_INTEGER)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/owl-not to :XSD_FLOAT)))
)

(deftest some-only []
  (is
   (not
    (nil?
     (o/some-only to
      (o/object-property "p" :ontology to) "a"))))

  (is
   (not
    (nil?
     (o/some-only to
                 (o/object-property "p" :ontology to) "a" "b")))))


(deftest data-some
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (first
               (o/owl-some to
                          (o/datatype-property "p" :ontology to)
                          :XSD_INTEGER)))))

(deftest data-some-data-range
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (first
               (o/data-some to
                            (o/datatype-property "p" :ontology to)
                            (o/span < 1))))))

(deftest disjoint-classes []
  (is
   (do (#'o/disjoint-classes to "a" "b" "c")))

  (is
   (do (#'o/disjoint-classes to
        (o/owl-class "a" :ontology to) (o/owl-class "b" :ontology to)))))

(deftest owl-class
  (is (= 1
         (do (o/owl-class "test" :ontology to)
             (.size (.getClassesInSignature to)))))
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (o/owl-class "test" :ontology to))))


(deftest defclass
  (is (= 1
         (do (o/defclass a :ontology to)
             (.size (.getClassesInSignature to))))))



(defn- test-class-with-hierarchy
  "Some test classes

Assumes that fixture has been run
"
  []

  (o/owl-class "a" :ontology to)
  (o/owl-class "b" :subclass "a" :ontology to)
  (o/owl-class "c" :subclass "b" :ontology to)

  (o/owl-class "d" :ontology to)
  (o/owl-class "e" :subclass "b" "d" :ontology to)
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


(deftest disjoint-classes []
  (is (not (nil? (#'o/disjoint-classes to "a" "b" "c")))))

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
        (let [clazz (o/owl-class to "a")]
          (o/remove-entity to clazz)
          (.size (.getClassesInSignature to))))))

  (is
   (= 0
      (do
        (let [prop (o/object-property to "a")]
          (o/remove-entity to prop)
          (.size (.getClassesInSignature to)))))))



(deftest with-probe-entities
  ;; are the classes created correctly
  (is
   (= 3
      (o/with-probe-entities to
        [a (o/owl-class to "a")
         b (o/owl-class to "b")
         c (o/owl-class to "c")]
        (-> to
            (.getClassesInSignature)
            (.size))
        )))

  (is
   (= 0
      (do
        (o/with-probe-entities to
            [a (o/owl-class to "a")
             b (o/owl-class to "b")
             c (o/owl-class to "c")
             ])
        ;; and have they gone again afterwards
        (-> to
            (.getClassesInSignature)
            (.size))))))


(defn ontology-c-with-two-parents []
  (o/owl-class to "a")
  (o/owl-class to "b")
  (o/owl-class to "c" :subclass "a" "b"))


(deftest with-probe-axioms
  ;; add a disjoint see whether it breaks
  (is
   (= 1
      (do
        (ontology-c-with-two-parents)
        (o/with-probe-axioms to
          [a (#'o/disjoint-classes to "a" "b")]
          (-> to
              (.getDisjointClassesAxioms
               (o/owl-class to "a"))
              (.size))))))

  ;; add a disjoint test whether it breaks after
  (is
   (= 0
      (do
        (ontology-c-with-two-parents)
        (o/with-probe-axioms to
          [a (#'o/disjoint-classes to "a" "b")])

        (-> to
            (.getDisjointClassesAxioms
             (o/owl-class to "a"))
            (.size))))))

(deftest owl-import
  (is
   (not
    (nil?
     (o/owl-import to to)))))


(deftest annotation
  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/label to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/owl-comment to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/is-defined-by to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/see-also to "hello")))
  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/backward-compatible-with to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/incompatible-with to "hello")))

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
           (o/owl-class to "a")
           (list (o/owl-comment to "comment"))))))

  (is
   (not
    (nil? (#'o/add-annotation
           to
           (list (o/owl-comment to "comment")))))))

(deftest add-annotation2
  (is
   (=
    "hello"
    (do
      (let [b (o/owl-class to "b")]
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
                 (o/datatype-property "hello" :ontology to))))



(deftest disjoint?
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")]
     (#'o/disjoint-classes to a b)
     (o/disjoint? to a b))))



;; TODO lots of macros are in serious need of a test

(deftest as-subclasses
  (is
   (let [x (o/owl-class to "x")]
     (o/as-subclasses to
      x
      (o/owl-class to "y")
      (o/owl-class to "z"))

     ;; now for the test
     (and (o/superclass? to
           (o/owl-class to "y")
           x)
          (o/superclass? to
           (o/owl-class to "z")
           x))))
  (is
   (let [x (o/owl-class to "x")]
     (o/as-subclasses to
      x :disjoint
      (o/owl-class to "y")
      (o/owl-class to "z"))
     (o/disjoint? to
      (o/owl-class to "y")
      (o/owl-class to "z"))))

  (is
   (let [x (o/owl-class to "x")]
        (o/as-subclasses to
         x :cover
         (o/owl-class to "y")
         (o/owl-class to "z"))

        (o/equivalent?
         to
         (o/owl-class to "x")
         (o/owl-or to (o/owl-class to "z")
                  (o/owl-class to "y")))))


  (is
   (let [x (o/owl-class to "x")]
     (o/as-subclasses to
      x :cover
      (o/owl-class to "y")
      (o/owl-class to "z"))

     (and
      (o/disjoint? to
       (o/owl-class to "y")
       (o/owl-class to "z"))

      (o/equivalent? to
       (o/owl-class to "x")
       (o/owl-or to (o/owl-class to "z")
                (o/owl-class to "y")))))))


(deftest as-disjoint-subclasses
  (is
   (let [x (o/owl-class to "x")]
     (o/as-disjoint-subclasses to
      x
      (o/owl-class to "y")
      (o/owl-class to "z"))

     (o/save-ontology to "to.omn" :omn)

     (and
      (o/disjoint? to
       (o/owl-class to "y")
       (o/owl-class to "z"))
      
      (o/superclass? to
       (o/owl-class to "y")
       x)

      (o/superclass? to
       (o/owl-class to "z")
       x)))))

(deftest prefix-suffix-symbol
  (is (= 'helloworld
         (#'o/prefix-symbol "hello" 'world)))
  (is (= 'helloworld
         (#'o/suffix-symbol "world" 'hello))))


(deftest guess
  (is (= :object
         (o/guess-type to
          (o/owl-class to "a"))))

  (is (= :annotation
         (o/guess-type to
          (o/annotation-property to "b"))))

  (is (= :object
         (do
           (o/owl-class to "c")
           (o/guess-type to "c"))))


  (is (= :object
         (o/guess-type to
          (list (o/owl-class to "d") "e" "f"))))

  (is (= :object
         (o/guess-type to
          (list "e" "f" (o/owl-class to "d"))))))


(deftest veggiepizza
  (is (= :object
         (do
           (o/guess-type to
            (o/with-probe-entities to
              [r (o/object-property to "hasTopping")
               c (o/owl-class to "MeatTopping")]
              (o/owl-not to
               (o/owl-some to r c))))))
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
   (do
     (o/with-probe-entities to
       [dp (o/datatype-property to "a")
        sdp (o/datatype-property to "b" :subproperty dp)]
       true)))
  (is
   (every? #(instance?
             org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom %1)
           (o/with-probe-entities to
             [dp (o/datatype-property to "a")
              sdp (o/datatype-property to "b")]
             (o/add-data-superproperty
              to
              dp sdp)))))

(deftest hasself
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectHasSelf
              (o/has-self to (o/object-property to "r")))))

(deftest owlmin
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owl-min 10))))

(deftest owlmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owl-max 10))))

(deftest minmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-max 10 10))))

(deftest mininc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-inc 10))))

(deftest maxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/max-inc 10))))

(deftest minmaxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-max-inc 10 20))))

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


(deftest owl-comment
  (is
   (instance?
    OWLAnnotation
    (o/owl-comment to
     (o/literal to "comment"
                 :type :RDF_PLAIN_LITERAL)))))
