;; The contents of this file are subject to the LGPL License, Version 3.0.

;; Copyright (C) 2011, Newcastle University

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

(ns tawny.pattern
  (:require [tawny.owl :as o]))


(defmacro value-partition
  "Builds a value-partition pattern."
  [& definition]
  (let [documentation#
        (if (string? (first definition))
          (list :comment (first definition))
          ())
        superclass#
        (if (string? (first definition))
          (second definition)
          (first definition))
        values#
        (if (string? (first definition))
          (nth definition 2)
          (second definition))
        propertyname#
        (symbol
         (str "has"
              (name superclass#)))]

    ;; need to think what to do about the doc string.
    `(do
       ;; need a forward declaration
       (o/defclass ~superclass#)
       ;; haven't added documentation to defoproperty yet
       (o/defoproperty ~propertyname#
         :characteristic :functional
         )
       (o/declare-classes
        ~@values# :subclass ~superclass#)

       ;; put in covering axiom once we worked out how to do this.
       (o/defclass ~superclass#
         ~@documentation#
         )

       (o/add-disjoint-union ~superclass# ~values#))))
