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
                                 OWLDataProperty)
   [org.semanticweb.owlapi.search  EntitySearcher])
  (:require [tawny.owl :as o]
            [tawny.render :as r]
            [tawny.protocol :as p]
            [tawny.util])
  (:require [tawny.debug])
  [:use clojure.test])


(def to nil)

(defn createtestontology[]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/" :prefix "iri" :noname true))))

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
  (is
   (not
    (nil?
     (o/ontology
      :iri "http://iri/"
      :prefix "iri:"
      :comment "This is a comment"
      :versioninfo "This is some versioninfo")))))

(defn to-form [entity]
  (r/as-form entity :terminal :object :keyword true))

(deftest ontology-two-iri
  (is
   (thrown? IllegalArgumentException
            (o/ontology :iri "iri1" :iri "iri2"))))

(deftest ontology-viri
  (is
   (= "http://example.com/viri"
      (let [o
            (o/ontology :iri "http://example.com/iri"
                        :viri "http://example.com/viri")
            iri
            (.. o
                getOntologyID getVersionIRI get toString)]
        (o/remove-ontology-maybe (.getOntologyID o))
        iri))))

(deftest ontology-annotation
  (is
   (= 2
      (let [
            o (o/ontology
               :iri "http://iri")
            ;; we don't need an ontology to create an owl-comment
            ;; but unfortunately it does trigger the default ontology hook.
            ;; so we a dubious side-step instead
            c1 (o/owl-comment "1")
            c2 (o/owl-comment "2")
            o2 (o/ontology :iri "http://iri"
                           :prefix "pre"
                           :annotation c1 c2)]
        (.size
         (.getAnnotations o2))))))

(deftest ontology-import
  (is
   (= 2
      (let [o1 (o/ontology)
            o2 (o/ontology)
            o (o/ontology :noname true :import
                          (p/as-iri "http://iri1")
                          (p/as-iri "http://iri2"))]
        (.size
         ;; have to use getImportsDeclarations because these imports are not
         ;; loaded, so are not returned by getDirectImports
         (.getImportsDeclarations o)))))

  (is
   (= 2
      (let [o1 (o/ontology)
            o2 (o/ontology)
            o (o/ontology :noname true :import "http://iri1" "http://iri2")]
        (.size
         (.getImportsDeclarations o)))))

  (is
   (= 2
      (let [o1 (o/ontology)
            o2 (o/ontology)
            o (o/ontology :noname true :import o1 o2)]
        (.size
         (.getDirectImports o))))))


(deftest as-iri
  (is (= "http://iri/"
         (.toString (p/as-iri (o/ontology :iri "http://iri/")))))
  (is (= "http://iri/"
         (.toString (p/as-iri to)))))


(deftest broadcast-ontology
  ;; simple non-broadcast version doesn't return a list
  (is (o/object-some
        (o/object-property to "a")
       (o/owl-class to "b")))
  (is (= 2 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c"])))))
  (is (= 3 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d"])))))
  (is (= 4 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d" "e"])))))
  (is (= 5 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d" "e" "f"])))))
  (is (= 6 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d" "e" "f" "g"])))))
  (is (= 7 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d" "e" "f" "g" "h"])))))
  (is (= 8 (count (o/object-some
                   (o/object-property to "a")
                   (map (partial o/owl-class to)
                        ["b" "c" "d" "e" "f" "g" "h" "i"]))))))

(defn brd [x a] (list x a))

(deftest broadcasting
  (is
   (= [1 2]
      ((o/broadcast brd)
       1 2)))
  (is
   (= [[1 2] [1 3]]
      ((o/broadcast brd)
       1 2 3)))
  (is
   (= [[1 2] [1 3]]
      ((o/broadcast brd) 1 [2 3])))
  (is
   (= [[1 2] [1 3] [1 4]]
      ((o/broadcast brd) 1 2 3 4)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5]]
      ((o/broadcast brd) 1 2 3 4 5)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5] [1 6]]
      ((o/broadcast brd) 1 2 3 4 5 6)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5] [1 6] [1 7]]
      ((o/broadcast brd) 1 2 3 4 5 6 7)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8]]
      ((o/broadcast brd) 1 2 3 4 5 6 7 8)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8] [1 9]]
      ((o/broadcast brd) 1 2 3 4 5 6 7 8 9)))
  (is
   (= [[1 2] [1 3] [1 4] [1 5] [1 6] [1 7] [1 8] [1 9] [1 10]]
      ((o/broadcast brd) 1 2 3 4 5 6 7 8 9 10))))

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
   (let [options {:a 1 :b 2 :noname true}]
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



(deftest save-ontology
  (testing "default format"
    (is (do (o/save-ontology to "target/test.omn")
            true))
    (is (do (o/save-ontology
             (o/ontology :prefix "bob") "target/test2.omn")
            true)))
  (testing "turtle format"
    (is (do (o/save-ontology to "target/test.ttl" :ttl)
            true))
    (is (.exists (java.io.File. "target/test.ttl")))))

(deftest iri-for-name
  (is (= (.toString (#'o/iri-for-name to "test"))
         "http://iri/#test")))

(deftest defoproperty
  (is
   (var?
    (o/defoproperty a :ontology to))))


(deftest oproperty []
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectProperty
              (o/object-property to "r"))))

(deftest ensure-class []
  (is (instance? org.semanticweb.owlapi.model.OWLClassExpression
                 (o/owl-class to "hello"))))


(deftest add-subclass []
  (is
   (do
     (o/add-subclass
      to
      (o/owl-class to "a")
      (o/owl-class to "b"))
     (= 2 (.size (.getClassesInSignature
                  to)))))

  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")]
     (o/add-subclass
      to a b)
     (o/subclass? to a b))))

(deftest add-superclass []
  (is
   (do
     (o/add-superclass
      to
      (o/owl-class to "a")
      (o/owl-class to "b"))
     (= 2 (.size (.getClassesInSignature
                  to)))))

  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")]
     (o/add-superclass
      to a b)
     (o/superclass? to a b))))


(deftest deprecated-add-subclass
  (is
   (do
     (o/deprecated-add-subclass
      to
      (o/owl-class to "a")
      (o/owl-class to "b"))
     (= 2 (.size (.getClassesInSignature
                  to)))))
  (is
   (let
       [a (o/owl-class to "a")
        b (o/owl-class to "b")]
     (o/deprecated-add-subclass to a b)
     (o/superclass? to a b))))


(deftest add-equivalent []
  (let [equiv (#'o/add-equivalent to
               (o/owl-class to "a")
               (list (o/owl-class to "b")))]
    (is (not (nil? equiv)))))

(deftest add-has-key
  (is
   (not (nil?
         (do
           (o/with-probe-entities to
             [p (o/object-property to "p")
              c (o/owl-class to "c")]
             (o/add-has-key to c (list p))))))))

(deftest add-subchain
  (is
   (not
    (nil?
     (do
       (let [p1 (o/object-property to "p1")
             p2 (o/object-property to "p2")
             p3 (o/object-property to "p3")
             ]
         (o/add-subchain to
          p1 (list p2 p3)))))))

  (is
   (not
    (nil?
     (do
       (let [p1 (o/object-property to "p1")
             p2 (o/object-property to "p2")
             p3 (o/object-property to "p3")
             ]
         (o/add-subchain to
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
         (o/add-subchain to
          p1 (list p2 p3 [p4 p5]))))))))

(deftest oproperty-subchain-test
  (is
   (let [
         p (o/object-property to "p")
         q (o/object-property to "q")
         r (o/object-property to "r")
         s (o/object-property to "s")
         t (o/object-property to "t" :subchain r s [p q])]
     (=
      ;; the ordering here comes from render and the semantics of set.
      ;; hopefully it will be consistent, because it is not functionally
      ;; important, but we do test it.1
      [:oproperty t :subchain [[r s][p q]]]
      (to-form t)))))

(deftest object-property []
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectProperty
       (o/object-property to "b")))
  )

(deftest owl-some []
  (is (not (nil? (o/owl-some
                  (o/object-property to "b")
                  (o/owl-class to "a")))))
  (is (o/object-some
       (o/object-property to "has")
       (o/owl-class to "leg")))
  ;; failing test
  (is (thrown? clojure.lang.ArityException
               (o/owl-some (o/owl-class to "hasLeg")))))

(deftest owlonly []
  (is (not (nil? (o/only
                  (o/object-property to "b")
                  (o/owl-class to "a"))))))


(deftest owl-and []
  (is (not (nil? (o/object-and
                  (o/owl-class to "a")
                  (o/owl-class to "b")))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectIntersectionOf
       (o/owl-and
        (o/owl-class to "c") (o/owl-class to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owl-and to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataIntersectionOf
       (o/owl-and :XSD_INTEGER :XSD_FLOAT))))

(deftest owl-or
  (is (not (nil? (o/object-or
                  (o/owl-class to "a")
                  (o/owl-class to "b")))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectUnionOf
       (o/owl-or
        (o/owl-class to "c")
        (o/owl-class to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owl-or to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/data-or :XSD_INTEGER :XSD_FLOAT)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataUnionOf
       (o/owl-or :XSD_INTEGER :XSD_FLOAT))))

(deftest owl-not
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (do
         (let [b (o/owl-class to "b")]
           (o/owl-not b)))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLObjectComplementOf
       (o/owl-not (o/owl-class to "d"))))
  (is (thrown? IllegalArgumentException
               (o/owl-not to)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/data-not :XSD_INTEGER)))
  (is (instance?
       org.semanticweb.owlapi.model.OWLDataComplementOf
       (o/owl-not :XSD_FLOAT)))
  (is (fn?
       (let [i (o/individual to "i")
             r (o/object-property to "r")]
         (o/owl-not r i))))
  (is (instance?
       org.semanticweb.owlapi.model.OWLNegativeObjectPropertyAssertionAxiom
       (let [i (o/individual to "i")
             j (o/individual to "j")
             r (o/object-property to "r")]
         ((o/owl-not r i) j #{})))))

(deftest some-only []
  (is
   (not
    (nil?
     (o/some-only
      (o/object-property to "p")
      (o/owl-class to "a")))))

  (is
   (not
    (nil?
     (o/some-only
      (o/object-property to "p")
      (o/owl-class to "a")
      (o/owl-class to "b"))))))


(deftest data-some
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (o/owl-some
               (o/datatype-property to "p")
               :XSD_INTEGER))))

(deftest data-some-data-range
  (is
   (instance? org.semanticweb.owlapi.model.OWLDataSomeValuesFrom
              (o/data-some
               (o/datatype-property to "p")
               (o/span < 1)))))

(deftest disjoint-classes []
  (is
   (do (#'o/disjoint-classes to ["a" "b" "c"])))

  (is
   (do (#'o/disjoint-classes
        to
        [(o/owl-class to "a")
         (o/owl-class to "b")]))))

(deftest owl-class
  (is (= 1
         (do (o/owl-class to "test")
             (.size (.getClassesInSignature to)))))
  (is (instance? org.semanticweb.owlapi.model.OWLClass
                 (o/owl-class to "test"))))


(deftest defclass
  (is (= 1
         (do (o/defclass a :ontology to)
             (.size (.getClassesInSignature to))))))



(defn- test-class-with-hierarchy
  "Some test classes

Assumes that fixture has been run"
  []
  (let [a (o/owl-class to "a" )
        b (o/owl-class to "b" :super a)
        c (o/owl-class to "c" :super b)
        d (o/owl-class to "d")
        e (o/owl-class to "e" :super b d)]))

(deftest superclass? []
  (is (not
       (nil?
        (do
          (test-class-with-hierarchy)
          (o/direct-superclasses
           to
           (o/owl-class to "c"))))))
  (is (do
        (test-class-with-hierarchy)
        (o/superclass?
         to
         (o/owl-class to "e")
         (o/owl-class to "a"))))
  (is (not
       (do
         (test-class-with-hierarchy)
         (o/superclass?
          to
          (o/owl-class to "c")
          (o/owl-class to "e"))))))

(deftest direct-instances []
  (is
   (o/with-probe-entities to
     [c (o/owl-class to "c")
      i (o/individual to "i" :type c)]
     (contains?
      (o/direct-instances to c)
      i))))


(deftest superclasses []
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")
         c (o/owl-class to "c")]
     (o/add-superclass to a b)
     (o/superclass? to a b))))

(deftest subclass? []
  (is
   (do (test-class-with-hierarchy)
       (o/subclass?
        to
        (o/owl-class to "a")
        (o/owl-class to "c"))))
  (is
   (not
    (do (test-class-with-hierarchy)
        (o/subclass?
         to
         (o/owl-class to "c")
         (o/owl-class to "e"))))))


(deftest disjoint-classes []
  (is (not (nil?
            (#'o/disjoint-classes
             to (map (partial o/owl-class to) ["a" "b" "c"]))))))

(deftest individual []
  (is (o/individual to "ind"))
  (is (not (nil? (o/individual to "indA"
                               :type (o/owl-class to "a")))))
  (is (thrown? IllegalArgumentException
               (o/individual to "indA" :nottypes "a"))))

(deftest individual-annotation []
  (is (o/individual to "ind"
                    :annotation (o/label "annotation")))
  (is (o/individual to "ind"
                    :label "label"))
  (is (o/individual to "ind"
                    :comment "comment")))

(deftest individual-type []
  (is
   (let [cls (o/owl-class to "cls")
         ind (o/individual to "ind" :type cls)]
     (tawny.util/in?
      (EntitySearcher/getTypes ind to)
      cls))))

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
  (o/owl-class
   to "c"
   :super
   (o/owl-class to "a")
   (o/owl-class to "b")))


(deftest with-probe-axioms
  ;; add a disjoint see whether it breaks
  (is
   (= 1
      (do
        (ontology-c-with-two-parents)
        (o/with-probe-axioms to
          [a (o/as-disjoint
              to
              (o/owl-class to "a")
              (o/owl-class to "b"))]
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
          [a (#'o/disjoint-classes
              to
              (map (partial o/owl-class to) ["a" "b"]))])

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
              (o/label "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/owl-comment "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/is-defined-by "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/see-also "hello")))
  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/backward-compatible-with "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/incompatible-with "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotationProperty
              (o/annotation-property to "hello")))

  (is
   (instance? org.semanticweb.owlapi.model.OWLAnnotation
              (o/annotation
               (o/annotation-property to "hello")
               "hello1"))))


(deftest add-version-info
  (is
   (#'o/add-version-info to
    "bob")))

(deftest add-annotation
  (is
   (not
    (nil? (#'o/add-annotation to
           (o/owl-class to "a")
           (o/owl-comment "comment"))))))

(deftest add-ontology-annotation
  (is
   (not
    (nil? (#'o/add-ontology-annotation
           to
           (list (o/owl-comment "comment")))))))

(deftest add-super-annotation
  (is
   (let [a (o/annotation-property to "a")
         b (o/annotation-property to "b")]
     (#'o/add-super-annotation to a b))))

(deftest add-annotation2
  (is
   (=
    "hello"
    (do
      (let [b (o/owl-class to "b")]
        (#'o/add-annotation to
         b (list (o/label "hello")))
        (.getLiteral
         (.getValue
          (first
           (filter
            #(-> %
                 (.getProperty)
                 (.isLabel))
            (EntitySearcher/getAnnotations b to))))))))))


(deftest dataproperty
  (is 
   (instance? org.semanticweb.owlapi.model.OWLDataProperty
              (o/datatype-property to "hello"))))

(deftest as-disjoint
  (is
   (o/as-disjoint to
    (o/owl-class to "a")
    (o/owl-class to "b")))
  (is
   (o/as-disjoint to
    (o/object-property to "r")
    (o/object-property to "s")))
  (is
   (o/as-disjoint to
    (o/datatype-property to "d")
    (o/datatype-property to "e"))))



(deftest disjoint?
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")]
     (#'o/disjoint-classes to [a b])
     (o/disjoint? to a b)))
  (is
   (not
    (let [a (o/owl-class to "a")
          b (o/owl-class to "b")
          c (o/owl-class to "c")]
      (#'o/disjoint-classes to [a b])
      (o/disjoint? to a c))))
  (is
   (let [r (o/object-property to "r")
         s (o/object-property to "s")
         t (o/object-property to "t")]
     (o/as-disjoint to r s)
     (o/disjoint? to r s)))
  (is
   (not
    (let [r (o/object-property to "r")
          s (o/object-property to "s")
          t (o/object-property to "t")]
      (o/as-disjoint to r s)
      (o/disjoint? to r t)))))



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
         (o/owl-or (o/owl-class to "z")
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
       (o/owl-or (o/owl-class to "z")
                 (o/owl-class to "y")))))))


(deftest as-disjoint-subclasses
  (is
   (let [x (o/owl-class to "x")]
     (o/as-disjoint-subclasses
      to
      x
      (o/owl-class to "y")
      (o/owl-class to "z"))

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
  (is (= :tawny.owl/class
         (o/guess-type
          (o/owl-class to "a"))))

  (is (isa?
       (o/guess-type
        (o/owl-class to "a"))
       :tawny.owl/object))

  (is (= :tawny.owl/annotation
         (o/guess-type
          (o/annotation-property to "b")))))


(deftest literal-args
  (is
   (= :tawny.owl/literal
       (o/guess-individual-literal 10)))
  (is
   (= :tawny.owl/literal
      (o/guess-individual-literal "bob"))))


(deftest veggiepizza
  (is (= :tawny.owl/class
         (do
           (o/guess-type
            (o/with-probe-entities to
              [r (o/object-property to "hasTopping")
               c (o/owl-class to "MeatTopping")]
              (o/owl-not
               (o/owl-some r c))))))
      "A regression tester from the pizza ontology"))

(deftest oneof
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLObjectOneOf
    (o/oneof (o/individual to "a"))))

  (is
   (instance?
    org.semanticweb.owlapi.model.OWLDataOneOf
    (o/oneof (o/literal "hello"))))

  (is
   (instance?
    org.semanticweb.owlapi.model.OWLDataOneOf
    (o/oneof 10)))

  (is
   (instance?
    org.semanticweb.owlapi.model.OWLDataOneOf
    (o/oneof 10 11 12 13)))

  (is
   (instance?
    org.semanticweb.owlapi.model.OWLDataOneOf
    (o/oneof true))))


(deftest add-different
  (is
   (let
       [i1 (o/individual to "i1")
        i2 (o/individual to "i2")]
     (o/add-different to i1 i2)
     (some #{i2}
           (EntitySearcher/getDifferentIndividuals i1 to)))))


(deftest add-data-super
  (is
   (do
     (o/with-probe-entities to
       [dp (o/datatype-property to "a")
        sdp (o/datatype-property to "b" :super dp)]
       true)))
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLSubDataPropertyOfAxiom
    (o/with-probe-entities to
      [dp (o/datatype-property to "a")
       sdp (o/datatype-property to "b")]
      (o/add-data-superproperty
       to
       dp sdp)))))

(deftest hasself
  (is
   (instance? org.semanticweb.owlapi.model.OWLObjectHasSelf
              (o/has-self (o/object-property to "r")))))

;; from bug report
(deftest span-double
  (is
   (not (.isInteger
         (.getDatatype (o/span > 0.1)))))
  (is
   (.isDouble (.getDatatype (o/span > 0.1)))))

(deftest owlmin
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owl-min 10)))
  (is
   (.isInteger
    (.getDatatype
     (o/owl-min 1))))
  (is
   (.isDouble
    (.getDatatype
     (o/owl-min 0.1)))))


(deftest owlmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/owl-max 10)))
  (is
   (.isInteger
    (.getDatatype
     (o/owl-max 1))))
  (is
   (.isDouble
    (.getDatatype
     (o/owl-max 0.1)))))

(deftest minmax
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-max 10 10)))
  (is
   (.isInteger
    (.getDatatype
     (o/min-max 1 1))))
  (is
   (.isDouble
    (.getDatatype
     (o/min-max 0.1 0.1)))))

(deftest mininc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-inc 10)))
  (is
   (.isInteger
    (.getDatatype
     (o/min-inc 1))))
  (is
   (.isDouble
    (.getDatatype
     (o/min-inc 0.1)))))

(deftest maxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/max-inc 10)))
  (is
   (.isInteger
    (.getDatatype
     (o/max-inc 1))))
  (is
   (.isDouble
    (.getDatatype
     (o/max-inc 0.1)))))

(deftest minmaxinc
  (is
   (instance? org.semanticweb.owlapi.model.OWLDatatypeRestriction
              (o/min-max-inc 10 20))))

(deftest literal
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal "bob" :lang "en")))
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal "bob")))
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLLiteral
    (o/literal "bob" :type :RDF_PLAIN_LITERAL))))


(deftest owl-comment
  (is
   (instance?
    OWLAnnotation
    (o/owl-comment
     (o/literal "comment"
                :type :RDF_PLAIN_LITERAL)))))

(deftest as-equivalent
  (is
   (o/as-equivalent to
    (o/owl-class to "a")
    (o/owl-class to "b")))
  (is
   (o/as-equivalent to
    (o/object-property to "r")
    (o/object-property to "s")))
  (is
   (o/as-equivalent to
    (o/datatype-property to "d")
    (o/datatype-property to "e"))))


(deftest equivalent?
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")
         c (o/owl-class to "c")]
     (o/as-equivalent
      to a b)
     (and (o/equivalent? to a b)
          (not (o/equivalent? to a c)))))
  (is
   (let [a (o/object-property to "r")
         b (o/object-property to "s")
         c (o/object-property to "t")]
     (o/as-equivalent to a b)
     (and (o/equivalent? to a b)
          (not (o/equivalent? to b c)))))
  (is
   (let [a (o/datatype-property to "r")
         b (o/datatype-property to "s")
         c (o/datatype-property to "t")]
     (o/as-equivalent to a b)
     (and (o/equivalent? to a b)
          (not (o/equivalent? to b c))))))


(deftest object-at-most
  (is
   (let [p (o/object-property to "p")
         c (o/owl-class to "c")]
     (= c
        (.getFiller
         (o/object-at-most 1 p c)))))
  (is
   (let [p (o/object-property to "p")]
     (=  (o/owl-thing)
         (.getFiller
          (o/object-at-most 1 p))))))


;; superclass (and subclass) is complicated
(deftest superclass-recurse []
  (is
   (= 0
      (count
       (let [a (o/owl-class to "a")]
         (o/superclasses to a)))))
  (is
   (= 1
      (count
       (let [a (o/owl-class to "a")]
         (o/add-subclass to a a)
         (o/superclasses to a)))))

  (is
   (= 2
      (count
       (let [a (o/owl-class to "a")
             b (o/owl-class to "b")
             ]
         (o/add-subclass to a b)
         (o/add-subclass to b a)
         (o/superclasses to a))))))

(deftest subclass-recurse []
  (is
   (= 0
      (count
       (let [a (o/owl-class to "a")]
         (o/subclasses to a)))))
  (is
   (= 1
      (count
       (let [a (o/owl-class to "a")]
         (o/add-subclass to a a)
         (o/subclasses to a)))))

  (is
   (= 2
      (count
       (let [a (o/owl-class to "a")
             b (o/owl-class to "b")
             ]
         (o/add-subclass to a b)
         (o/add-subclass to b a)
         (o/subclasses to a))))))


(deftest super-frame-class []
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b" :super a)]
     (o/superclass? to b a))))


(deftest sub-frame-class []
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b" :sub a)]
     (o/subclass? to b a))))


(deftest super-frame-property []
  (is
   (let [r (o/object-property to "r")
         s (o/object-property to "s"
                              :super r)]
     (o/superproperty? to s r))))

(deftest sub-frame-property []
  (is
   (let [r (o/object-property to "r")
         s (o/object-property to "s"
                              :sub r)]
     (o/subproperty? to s r))))

(deftest super-frame-data-property []
  (is
   (let [h (o/datatype-property to "h")
         j (o/datatype-property to "j"
                            :super h)]
     (o/superproperty? to j h))))

(deftest sub-frame-data-property []
  (is
   (let [h (o/datatype-property to "h")
         j (o/datatype-property to "j"
                              :sub h)]
     (o/subproperty? to j h)))

   (is
   (let [h (o/datatype-property to "h")
         j (o/datatype-property to "j"
                              :sub h)]
     (o/superproperty? to h j))))

(deftest refine []
  (is
   (let [cls (o/owl-class to "a")]
     (o/refine to cls :label "a"))))

;; testing :subchain frame
(deftest add-one-subchain-axiom1
  (let [to (o/ontology :noname true)
        A (o/object-property to "A")
        B (o/object-property to "B")]

    (let [before (count (.getAxioms to))]
      (o/object-property to A :subchain [A B])
      (is (= (+ before 1) (count (.getAxioms to)))))))

;; testing add-subchain function
(deftest add-one-subchain-axiom2
  (let [to (o/ontology :noname true)
        A (o/object-property to "A")
        B (o/object-property to "B")]

    (let [before (count (.getAxioms to))]
      (o/add-subchain to A [A B])
      (is (= (+ before 1) (count (.getAxioms to)))))))


;; annotations on axioms
(deftest annotation-on-superclass
  (is
   (let [a (o/owl-class to "a")
         b (o/owl-class to "b")
         ax (o/add-superclass to a b)]
     (= 0 (count (.getAnnotations ax))))))

;; these need to be in their own deftest because we only create one ontology
;; per deftest annotated axioms are not idepotment with unannotated.
(deftest annotation-on-superclass-1
  (let [a (o/owl-class to "a")
        b (o/owl-class to "b")
        ax (o/add-superclass
            to a
            (o/annotate
             b
             (o/label "annotate")
             ))]
    (is
     (= 1 (count (.getAnnotations ax))))))

(deftest annotation-on-superclass-2
  (let [a (o/owl-class to "a")
        b (o/owl-class
           to "b"
           :super
           (o/annotate a (o/label "l")))
        axs (filter
             #(instance? org.semanticweb.owlapi.model.OWLSubClassOfAxiom %)
             (seq (.getAxioms to)))]
    (is
     (= 1  (count axs)))
    (is
     (= 1 (count (.getAnnotations (first axs)))))))


(deftest annotation-on-superproperty
  (is
   (let [r (o/object-property to "r")
         s (o/object-property to "s")
         ax (o/add-superproperty to r s)]
     (= 0 (count (.getAnnotations ax))))))

(deftest annotation-on-superproperty-1
  (is
   (= 1
      (let [r (o/object-property to "r")
            s (o/object-property to "s")
            ax (o/add-superproperty
                to r
                (o/annotate
                 s
                 (o/label "annotatin")))]
        (count (.getAnnotations ax))))))

(deftest annotation-on-add-characteristics
  (is
   (let [r (o/object-property to "r")
         ax (o/add-characteristics
             to r
             (o/annotate :functional
                         (o/owl-comment to "f")))]
     (= 1
        (count (.getAnnotations ax))))))

(deftest annotation-on-add-characteristics
  (is
   (let [r (o/object-property to "r")
         ax (o/add-characteristics
             to r
             (o/annotate :functional
                         (o/owl-comment "f")))]
     (= 1
        (count (.getAnnotations ax))))))

(deftest annotation-on-fact
  (is
   (let [d (o/datatype-property to "d")
         i (o/individual to "i")
         ax (o/add-fact
             to
             i
             (o/annotate (o/is d (o/literal 10))
                         (o/owl-comment
                          (o/owl-class to "f"))))]
     (= 1
        (count (.getAnnotations ax))))))

;; copy from tawny.render which is crashing
(deftest individual
  (is
   (o/fact (o/datatype-property to "d") 10))
  (is
   (o/individual to "I"
                 :fact
                 (o/fact (o/datatype-property to "d")
                         10)
                 (o/fact (o/object-property to "r")
                         (o/individual to "I2")))))

(deftest broadcasting-annotate
  (is (seq? (o/annotate
             (o/object-some
              (o/object-property to "r")
              (o/owl-class to "A")
              (o/owl-class to "B"))
            (o/owl-comment "c")))))

(deftest inverse
  (is
   (instance?
    org.semanticweb.owlapi.model.OWLObjectInverseOf
    (o/inverse (o/object-property to "p")))))
