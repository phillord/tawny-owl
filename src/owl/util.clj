
(ns owl.util)


;;
;; Utillity functions for improving syntax
;;
(defn groupify
  "Takes a list with keywords and values and turns all adjacent values into a single list"
  ;; entry point
  ([list]
     (groupify () list nil nil))
  ;; not an entry point!
  ([reduced-frame long-frame-left current-keyword current-val-list]
     (if-let [first-val (first long-frame-left)]
       ;; the unreduced frame is not yet empty
       (if (keyword? first-val)
         ;; so we have a keyword as the first val
         (if current-keyword 
           ;; but we not have any vals -- exception
           (if (not current-val-list)
             (throw (IllegalArgumentException. "Cannot have two adjacent keywords"))
             ;; and we have some existing keywords and values
             (groupify
              (cons current-keyword
                    (cons current-val-list
                          reduced-frame))
              (rest long-frame-left) first-val nil))
           ;; so we have a legal new keyword, so start a new list
           (groupify
            reduced-frame (rest long-frame-left)
            first-val nil))
         ;; we do not have a keyword
         (groupify reduced-frame (rest long-frame-left)
                   current-keyword (cons first-val current-val-list)))
       
       ;; else we have nothing left in the frame, so terminate
       (cons current-keyword
             (cons current-val-list
                   reduced-frame)))))


(defn hashify
  "Takes a list with alternating keyword values and returns a hash"
  [list]
  (apply
   hash-map (groupify list)))
