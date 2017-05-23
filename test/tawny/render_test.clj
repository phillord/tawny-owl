;; The contents of this file are subject to the LGPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2017, Phillip Lord, Newcastle University
;;
;; This program is free software: you can redistribute it and/or modify it
;; under the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or (at your
;; option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
;; for more details.
;;
;; You should have received a copy of the GNU Lesser General Public License
;; along with this program. If not, see http://www.gnu.org/licenses/.
(ns tawny.render-test
  (:use [clojure.test])
  (:require [tawny.render :as r]
            [tawny.owl :as o]
            [tawny.fixture]
            ))

(def to nil)

(defn createtestontology[test]
  (alter-var-root
   #'to
   (fn [x]
     (o/ontology :iri "http://iri/"
                 :noname true
                 :prefix "iri")))
  (test))

(use-fixtures :each createtestontology)

(defn double-as-form [entity]
  (vector
   (r/as-form entity :terminal :object)
   (r/as-form entity :terminal :object :keyword true)))

(defn multi-as-form [entity]
  (vector
   (r/as-form entity :terminal :object)
   (r/as-form entity :terminal :object :explicit true)
   (r/as-form entity :terminal :object :keyword true)
   (r/as-form entity :terminal :object :explicit true :keyword true)
   ))


(deftest datatype
  (is (= :XSD_INTEGER
         (r/as-form (#'o/ensure-datatype :XSD_INTEGER)))))

(defn lit-f
  ([val]
     (r/as-form (o/literal val)))
  ([val lang]
     (r/as-form (o/literal val :lang lang))))

(deftest literal
  (is
   (= '(literal "10" :type :XSD_INTEGER)
      (lit-f 10))))
;;   (is
;;    (= [10.0]
;;       (lit-f 10.0)))
;;   (is
;;    (= [true]
;;       (lit-f true)))
;;   (is
;;    (= ["bob"]
;;       (lit-f "bob")))
;;   (is
;;    (= ["bob" "en"]
;;       (lit-f "bob" "en"))))

(defn data-ontology []
  (o/datatype-property to "rD"))

(deftest datasome-datatype
  (is
   (=
    '(owl-some (iri "http://iri/#rD") :XSD_INTEGER)
    (do (data-ontology)
        (r/as-form
         (o/owl-some (o/datatype-property to "rD") :XSD_INTEGER))))))

(deftest datasome-range
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [['owl-some d '[span < 1]]
       [:some d [:span :< 1]]]
      (double-as-form
       (o/owl-some d (o/span < 1))))))
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [['owl-some d ['span '>< 1 1]]
       [:some d [:span :>< 1 1]]]
      (double-as-form
       (o/owl-some d (o/span >< 1 1))))))

(deftest dataonly-range
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [['only d '[span < 1]]
       [:only d [:span :< 1]]]
      (double-as-form
       (o/owl-only d (o/span < 1))))))
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [['only d ['span '>< 1 1]]
       [:only d [:span :>< 1 1]]]
      (double-as-form
       (o/owl-only d (o/span >< 1 1))))))))


(deftest individual-fact-1
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2")))
      (r/as-form
       (o/individual to "I"
                     :fact (o/fact
                            (o/object-property to "r")
                            (o/individual to "I2")))))))

(deftest individual-fact-2
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact-not (iri "http://iri/#r")
                             (iri "http://iri/#I2")))
      (r/as-form
       (o/individual
        to "I"
        :fact (o/fact-not
               (o/object-property to "r")
               (o/individual to "I2")))))))


(deftest individual-3
  (is
   (=
    '(individual (iri "http://iri/#I")
                :fact
                (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2"))
                (fact-not (iri "http://iri/#r")
                          (iri "http://iri/#I2")))
    (r/as-form
     (o/individual to "I"
                   :fact
                   (o/fact (o/object-property to "r")
                           (o/individual to "I2"))
                   (o/fact-not (o/object-property to "r")
                               (o/individual to "I2")))))))


(deftest individual-data
  (is
   (=
    '(individual
      (iri "http://iri/#I")
      :fact (fact (iri "http://iri/#d")
                  (literal "10" :type :XSD_INTEGER)))
    (r/as-form
     (o/individual to "I"
                   :fact
                   (o/fact (o/datatype-property to "d")
                           10))))))


(deftest individual-data-2
  (is
   (= '(individual (iri "http://iri/#I")
                   :fact
                   (fact (iri "http://iri/#r")
                         (iri "http://iri/#I2"))
                   (fact (iri "http://iri/#d")
                         (literal "10" :type :XSD_INTEGER)))
      (r/as-form
       (o/individual to "I"
                     :fact
                     (o/fact (o/datatype-property to "d")
                             10)
                     (o/fact (o/object-property to "r")
                             (o/individual to "I2")))))))

(deftest oproperty-super-test
  (is
   (= '(object-property
        (iri "http://iri/#r")
        :super
        (iri "http://iri/#s"))
      (r/as-form
       (o/object-property to "r"
                          :super
                          (o/iri-for-name to "s"))))))

(defn get-some-example-subproperty-chains []
  (let [r (o/object-property to "r")
        s (o/object-property to "s")
        t (o/object-property to "t" :subchain r s)
        chains
        (filter
         #(= t (.getSuperProperty
                ^org.semanticweb.owlapi.model.OWLSubPropertyChainOfAxiom %))
         (.getAxioms
          ^org.semanticweb.owlapi.model.OWLOntology to
          org.semanticweb.owlapi.model.AxiomType/SUB_PROPERTY_CHAIN_OF))]
    chains))

(deftest oproperty-form-subchain-render
  (is
   (get-some-example-subproperty-chains)))

(deftest oproperty-subchain-test
  (is
   (let [r (o/object-property to "r")
         s (o/object-property to "s")
         t (o/object-property to "t" :subchain r s)
         f (double-as-form t)]
     (and
      (=
       [['object-property t :subchain [r s]]
        [:oproperty t :subchain [[r s]]]]
       f)
      ;; we need to test type explicitly because we must have [r s] and not (r
      ;; s) for the clojure rendering
      (vector?
       (nth
        (first f) 3))))))

(deftest dproperty-super-test
  (is
   (= '(datatype-property
        (iri "http://iri/#g")
        :super
        (iri "http://iri/#h"))
      (r/as-form
       (o/datatype-property to "g"
                          :super
                          (o/iri-for-name to "h"))))))


(deftest entity-or-iri-as-iri
  (is
   (= ['owl-class ['iri "http://iri/#a"]]
      (r/as-form
       (o/owl-class to "a")
       :terminal :iri)))
  (is
   (= ['ontology
       :iri "http://iri/"
       :prefix "iri"]
      (r/as-form
       to
       :terminal :iri))))

(deftest entity-or-iri-object
  (is
   (tawny.owl/with-probe-entities to
     [a (o/owl-class to "a")]
     (=
      (r/as-form
       (o/owl-and a) :terminal :object)
      ['owl-and a]))))


;; :some
(deftest object-some
  (is
   (o/with-probe-entities to
     [p (o/object-property to "p")
      c (o/owl-class to "c")]
     (= [
         ['owl-some p c]
         ['object-some p c]
         [:some p c]
         [:object-some p c]]
        (multi-as-form
         (o/owl-some p c))))))


(deftest data-some
  (is
   (o/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [
       ['owl-some d :XSD_INTEGER]
       ['data-some d :XSD_INTEGER]
       [:some d :XSD_INTEGER]
       [:data-some d :XSD_INTEGER]]
      (multi-as-form
       (o/owl-some d :XSD_INTEGER))))

     ))

;; :only

(deftest object-only
  (is
   (tawny.owl/with-probe-entities to
     [p (o/object-property to "p")
      c (o/owl-class to "c")]
     (=
      [
       ['only p c]
       ['object-only p c]
       [:only p c]
       [:object-only p c]]
      (multi-as-form
       (o/only p c))))))

(deftest data-only
  (is
   (o/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [
       ['only d :XSD_INTEGER]
       ['data-only d :XSD_INTEGER]
       [:only d :XSD_INTEGER]
       [:data-only d :XSD_INTEGER]]
      (multi-as-form
       (o/only d :XSD_INTEGER))))))


;; :owl-and

(deftest object-and
  (is
   (o/with-probe-entities to
     [b (o/owl-class to "b")
      c (o/owl-class to "c")]
     (= [
         ['owl-and b c]
         ['object-and b c]
         [:and b c]
         [:object-and b c]]
        (multi-as-form
          (o/owl-and b c))))))


(deftest data-and
  (is
   (=
    [
     ['owl-and :XSD_INTEGER]
     ['data-and :XSD_INTEGER]
     [:and  :XSD_INTEGER]
     [:data-and  :XSD_INTEGER]]
    (multi-as-form
     (o/owl-and :XSD_INTEGER)))))


;; :owl-or
(deftest object-or
  (is
   (o/with-probe-entities to
     [b (o/owl-class to "b")
      c (o/owl-class to "c")]
     (= [
         ['owl-or b c]
         ['object-or b c]
         [:or b c]
         [:object-or b c]]
        (multi-as-form
          (o/owl-or b c))))))


(deftest data-or
  (is
   (=
     [
      ['owl-or :XSD_INTEGER]
      ['data-or :XSD_INTEGER]
      [:or :XSD_INTEGER]
      [:data-or :XSD_INTEGER]]
     (multi-as-form
      (o/owl-or :XSD_INTEGER)))))




;; :exactly
(deftest object-exactly
  (is
   (tawny.owl/with-probe-entities to
     [p (o/object-property to "p")
      c (o/owl-class to "c")]
     (=
      [
       ['exactly 1 p c]
       ['object-exactly 1 p c]
       [:exactly 1 p c]
       [:object-exactly 1 p c]]
      (multi-as-form
       (o/exactly 1 p c))))))

(deftest data-exactly
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [
       ['exactly 1 d :RDFS_LITERAL]
       ['data-exactly 1 d :RDFS_LITERAL]
       [:exactly 1 d :RDFS_LITERAL]
       [:data-exactly 1 d :RDFS_LITERAL]]
      (multi-as-form
       (o/exactly 1 d))))))

;; :oneof
(deftest object-oneof
  (is
   (tawny.owl/with-probe-entities to
     [i (o/individual to  "i")]
     (=
      [
       ['oneof i]
       ['object-oneof i]
       [:oneof i]
       [:object-oneof i]]
      (multi-as-form
       (o/oneof i))))))

(deftest data-oneof
  (is
   (=
    [
     ['oneof ['literal "10" :type :XSD_INTEGER]]
     ['data-oneof ['literal "10" :type :XSD_INTEGER]]
     [:oneof [:literal "10" :type :XSD_INTEGER]]
     [:data-oneof [:literal "10" :type :XSD_INTEGER]]]
    (multi-as-form
     (o/oneof (o/literal 10))))))

;; :at-least
(deftest object-at-least
  (is
   (tawny.owl/with-probe-entities to
     [p (o/object-property to "p")
      c (o/owl-class to "c")]
     (=
      [
       ['at-least 1 p c]
       ['object-at-least 1 p c]
       [:at-least 1 p c]
       [:object-at-least 1 p c]]
      (multi-as-form
       (o/at-least 1 p c))))))

(deftest data-at-least
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [
       ['at-least 1 d :RDFS_LITERAL]
       ['data-at-least 1 d :RDFS_LITERAL]
       [:at-least 1 d :RDFS_LITERAL]
       [:data-at-least 1 d :RDFS_LITERAL]]
      (multi-as-form
       (o/at-least 1 d))))))

;; :at-most
(deftest object-at-most
  (is
   (tawny.owl/with-probe-entities to
     [p (o/object-property to "p")
      c (o/owl-class to "c")]
     (=
      [
       ['at-most 1 p c]
       ['object-at-most 1 p c]
       [:at-most 1 p c]
       [:object-at-most 1 p c]]
      (multi-as-form
       (o/at-most 1 p c))))))

(deftest data-at-most
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [
       ['at-most 1 d :RDFS_LITERAL]
       ['data-at-most 1 d :RDFS_LITERAL]
       [:at-most 1 d :RDFS_LITERAL]
       [:data-at-most 1 d :RDFS_LITERAL]]
      (multi-as-form
       (o/at-most 1 d))))))

;; :has-value
(deftest object-has-value
  (is
   (tawny.owl/with-probe-entities to
     [r (o/object-property to "r")
      i (o/individual to "i")]
     (=
      [['has-value r i]
       ['object-has-value r i]
       [:has-value r i]
       [:object-has-value r i]]
      (multi-as-form
       (o/has-value r i))))))

(deftest data-has-value
  (is
   (tawny.owl/with-probe-entities to
     [d (o/datatype-property to "d")]
     (=
      [['has-value d ['literal "10" :type :XSD_INTEGER]]
       ['data-has-value d ['literal "10" :type :XSD_INTEGER]]
       [:has-value d [:literal "10" :type :XSD_INTEGER]]
       [:data-has-value d [:literal "10" :type :XSD_INTEGER]]]
      (multi-as-form
       (o/has-value d 10))))))

;; :owl-not
(deftest object-owl-not
  (is
   (tawny.owl/with-probe-entities to
     [c (o/owl-class to "c")]
     (=
      [['owl-not c]
       ['object-not c]
       [:not c]
       [:object-not c]]
      (multi-as-form
       (o/owl-not c))))))

(deftest data-owl-not
  (is
   (=
    [['owl-not :XSD_INTEGER]
     ['data-not :XSD_INTEGER]
     [:not :XSD_INTEGER]
     [:data-not :XSD_INTEGER]]
    (multi-as-form
     (o/owl-not :XSD_INTEGER)))))


;; :iri
(deftest iri
  (is
   (=
    [['iri "i"]
     ['iri "i"]
     [:iri "i"]
     [:iri "i"]]
    (multi-as-form
     (o/iri "i")))))

;; :label
(deftest label
  (is
   (=
    [['label ['literal "l" :lang "en"]]
     ['label ['literal "l" :lang "en"]]
     [:label [:literal "l" :lang "en"]]
     [:label [:literal "l" :lang "en"]]]
    (multi-as-form
     (o/label "l")))))

;; :comment
(deftest owlcomment
  (is
   (=
    [['owl-comment ['literal "l" :lang "en"]]
     ['owl-comment ['literal "l" :lang "en"]]
     [:comment [:literal "l" :lang "en"]]
     [:comment [:literal "l" :lang "en"]]]
    (multi-as-form
     (o/owl-comment "l")))))

;; :literal
(deftest literal
  (is
   (=
    [['literal "1" :type :XSD_INTEGER]
     ['literal "1" :type :XSD_INTEGER]
     [:literal "1" :type :XSD_INTEGER]
     [:literal "1" :type :XSD_INTEGER]]
    (multi-as-form
     (o/literal 1)))))

;; :<
(deftest span<
  (is
   (=
    [['span '< 0]
     ['span '< 0]
     [:span :< 0]
     [:span :< 0]]
    (multi-as-form
     (o/span < 0)))))

;; :<=
(deftest span<=
  (is
   (=
    [['span '<= 0]
     ['span '<= 0]
     [:span :<= 0]
     [:span :<= 0]]
    (multi-as-form
     (o/span <= 0)))))


;; :>
(deftest span>
  (is
   (=
    [['span '> 0]
     ['span '> 0]
     [:span :> 0]
     [:span :> 0]]
    (multi-as-form
     (o/span < 0)))))

;; :>=
(deftest span>
  (is
   (=
    [['span '>= 0]
     ['span '>= 0]
     [:span :>= 0]
     [:span :>= 0]]
    (multi-as-form
     (o/span >= 0)))))

;; :has-self
(deftest hasself
  (is
   (tawny.owl/with-probe-entities to
     [r (o/object-property to "r")]
     (=
      [['has-self r]
       ['has-self r]
       [:has-self r]
       [:has-self r]]
      (multi-as-form
       (o/has-self r))))))

;; :inverse
(deftest inverse
  (is
   (tawny.owl/with-probe-entities to
     [r (o/object-property to "r")]
     (=
      [['inverse r]
       ['inverse r]
       [:inverse r]
       [:inverse r]]
      (multi-as-form
       (o/inverse r))))))


(deftest ontology
  ;; simplest possible -- ontology with a specific IRI
  (is
   (let [o (o/ontology :noname true :iri "http://example.com/iri")]
     (= [
         ['ontology :iri "http://example.com/iri"]
         [:ontology o :iri "http://example.com/iri"]
         ]
        (double-as-form o))))
  (is
   (let [o (o/ontology :noname true
                       :iri "http://example.com/iri"
                       :viri "http://example.com/viri")]
     (= [
         ['ontology :iri "http://example.com/iri"
          :viri "http://example.com/viri"]
         [:ontology o :iri "http://example.com/iri"
          :viri "http://example.com/viri"]
         ]
        (double-as-form o))))  )


(deftest owl-class
  (is
   (tawny.owl/with-probe-entities to
     [c (o/owl-class to "c")
      d (o/owl-class to "d")
      e (o/owl-class to "e"
                     :super c d)]
     (=
      [
       ['owl-class e :super c d]
       [:class e :super [c d]]]
      (double-as-form e)))))


(deftest oproperty
  (is
   (tawny.owl/with-probe-entities to
     [r (o/object-property to "r")]
     (=
      [['object-property r]
       [:oproperty r]]
      (double-as-form r)))))

(deftest individ-many-facts
  (is
   (tawny.owl/with-probe-entities to
     [r (o/object-property to "r")
      s (o/object-property to "s")
      i (o/individual to "i")
      j (o/individual
         to "j"
         :fact (o/fact r i) (o/fact s i))]
     (=
      [['individual j
        :fact
        ['fact r i]
        ['fact s i]]
       [:individual j
        :fact
        [[:fact r i]
         [:fact s i]]]]
      (double-as-form j)))))


;; render-annotation-frame

(deftest ontology-annotation
  (is
   (= '(ontology
        :iri "http://iri/#x"
        :prefix "x"
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/ontology :iri "http://iri/#x"
                   :prefix "x"
                   :noname true
                   :annotation
                   (o/label "test label"))))))

(deftest class-annotation
  (is
   (= '(owl-class
        (iri "http://iri/#x")
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/owl-class to "x"
                    :label
                    "test label")))))

(deftest individual-annotation
  (is
   (= '(individual
        (iri "http://iri/#x")
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/individual to "x"
                     :label
                     "test label")))))

(deftest oproperty-annotation
  (is
   (= '(object-property
        (iri "http://iri/#x")
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/object-property to "x"
                          :label
                          "test label")))))

(deftest dproperty-annotation
  (is
   (= '(datatype-property
        (iri "http://iri/#x")
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/datatype-property to "x"
                            :label
                            "test label")))))

(deftest aproperty-annotation
  (is
   (= '(annotation-property
        (iri "http://iri/#x")
        :annotation
        (label (literal "test label" :lang "en")))
      (r/as-form
       (o/annotation-property to "x"
                            :label
                            "test label")))))

(deftest oproperty-characteristic []
  (let [o (o/object-property to "x" :characteristic :functional)]

    (is
     (=
      '(object-property
        (iri "http://iri/#x")
        :characteristic :functional)
      (r/as-form o)))))
