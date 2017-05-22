(ns
    ^{:doc "Arabic translation of Tawny OWL. Currently in process"
      :author "Aisha Blfgeh"}
    tawny.arabic
  (:require
   [tawny.owl :as o]
   [tawny.polyglot :as p]))

(def قائمة-المصطلحات-المعربة
  {
   :ontology :أنتولوجيا

   ;; ontology handler معالجات الأنتولوجيا
   :noname :بدون-اسم
   :iri-gen :آي-آر-آي
   :prefix :بادئة
   :seealso :انظر-أيضا
   :comment :تعليق
   :versioninfo :الإصدار
   :annotation :التدوين
   :import :جلب

   ;; annotation handler معالجات التدوين
   ;; same as :super
   :label :علامة

   ;; subject property خاصية الكائن
   :domain :المجال
   :range :المدى
   :inverse :عكسي
   :characteristic :ميزة
   :subchain :متسلسل-من
   :disjoint :منفصل
   :equivalent :مساوي

   ;; class  الصنف
   :haskey :مفتاحه
   :subclass :متفرع-من

   ;; individual  المثال
   :type :نوع
   :fact :حقيقة
   :same :مثل
   :different :مختلف})

(defn العربية [f]
  (p/polyglot-trans f قائمة-المصطلحات-المعربة))

(def الأنتولوجيا
  (العربية o/ontology))

(defmacro عرف-الأنتولوجيا
  "تعريف الأنتولوجيا واعطاءها اسم معرف لها
  البيانات التالية يجب إضافتها مع التعريف:
  الاسم: وهو الاسم المعرف
  أي معاملات أخرى ذات العلاقة مثل :بادئة و :آي-آر-آي وغيره"
  [الاسم & المعاملات]
  (o/ontology-def-f الاسم المعاملات))

(def خاصية-التدوين
  (العربية o/annotation-property))

(o/defentity عرف-خاصية-التدوين
  "تعريف خاصية تدوين جديدة للأنتولوجيا الحالية
   والمزيد من التفاصيل موجودة في مدونة (تعريف الصنف) الموجودة في....."
  'tawny.arabic/خاصية-التدوين
  :أنتولوجيا)

(def خاصية-الكائن
  (العربية o/object-property))

(o/defentity عرف-خاصية-الكائن
  "تعريف خاصية كائن جديدة للأنتولوجيا الحالية"
  #'tawny.arabic/خاصية-الكائن
  :أنتولوجيا)

(def خاصية-البيانات
  (العربية o/datatype-property))

(o/defentity عرف-خاصية-البيانات
  "تعريف خاصية بيانات جديدة للأنتولوجيا الحالية"
  #'tawny.arabic/خاصية-البيانات
  :أنتولوجيا)

(def المثال
  (العربية o/individual))

(o/defentity عرف-المثال
  "Declare a new individual"
  'tawny.arabic/المثال
  :أنتولوجيا)

(def نوع-بيانات
  (العربية o/datatype))

(o/defentity عرف-نوع-بيانات
  "تعريف نوع جديد من البيانات"
  'tawny.arabic/نوع-بيانات
  :أنتولوجيا)

(def الصنف
  (العربية o/owl-class))

(o/defentity عرف-الصنف
  "تعريف صنف جديد في الأنتولوجيا الحالية"
  #'tawny.arabic/الصنف
  :أنتولوجيا)

(def بعض #'o/owl-some)
(def فقط #'o/only)
(def مع #'o/owl-and)
(def أو #'o/owl-or)
(def ليس #'o/owl-not)
(def عكس #'o/inverse)
(def علامة #'o/label)
(def تعليق #'o/owl-comment)
(def معرف-بواسطة #'o/is-defined-by)
(def انظر-أيضا #'o/see-also)
(def متوافق-عكسيا-مع #'o/backward-compatible-with)
(def الإصدار #'o/version-info)
(def مهمل #'o/deprecated)
(def أصناف-منفصلة #'o/as-disjoint)
(def على-اﻷقل #'o/at-least)
(def على-اﻷكثر #'o/at-most)
(def قيمته #'o/has-value)
(def هو #'o/is)
(def تحسين #'o/refine)
(def حدود #'o/span)
(def أكثر #'tawny.owl/owl-max)
(def أقل #'tawny.owl/owl-min)
(def أقل_أكثر #'tawny.owl/min-max)
(def أكثر_يساوي #'tawny.owl/max-inc)
(def أقل_يساوي #'tawny.owl/min-inc)
(def أقل_أكثر_يساوي #'tawny.owl/min-max-inc)
