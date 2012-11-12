
(ns owl.util)


;;
;; Utillity functions for improving syntax
;;
(defn groupify
  "Takes a list with keywords and values and turns all adjacent values into a single list"
  ;; entry point
  [list]
  (loop [reduced-frame ()
         long-frame-left list
         current-keyword nil
         current-val-list nil]
     (if-let [first-val (first long-frame-left)]
       ;; the unreduced frame is not yet empty
       (if (keyword? first-val)
         ;; so we have a keyword as the first val
         (if current-keyword 
           ;; but we not have any vals -- exception
           (if (not current-val-list)
             (throw (IllegalArgumentException. "Cannot have two adjacent keywords"))
             ;; and we have some existing keywords and values
             (recur
              (cons current-keyword
                    (cons current-val-list
                          reduced-frame))
              (rest long-frame-left) first-val nil))
           ;; so we have a legal new keyword, so start a new list
           (recur
            reduced-frame (rest long-frame-left)
            first-val nil))
         ;; we do not have a keyword
         (recur reduced-frame (rest long-frame-left)
                   current-keyword (cons first-val current-val-list)))
       
       ;; else we have nothing left in the frame, so terminate
       (if (or current-keyword current-val-list)
         (cons current-keyword
               (cons current-val-list
                     reduced-frame))
         reduced-frame))))


(defn hashify
  "Takes a list with alternating keyword values and returns a hash"
  [list]
  (apply
   hash-map (groupify list)))

;; contains really doesn't do what you expect
(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defn has-keys
  "Returns true iff hash has only thekeys in hash."
  [hash thekeys]
  (every?
   (fn [x]
     (in? thekeys x))
   (keys hash)))

(defn check-keys
  "Returns the hash or throws IllegalArgumentException if has does not have
  only thekeys in hash"
  [hash thekeys]
  (when-not (has-keys hash thekeys)
    (throw
     (IllegalArgumentException.
      (format "Expected only keys %s; Got %s" thekeys (keys hash)))))
  hash)


(defmacro quote-word [& symbols]
  `(do
     (list
      ~@(map
         (fn [symbol]
           (name symbol))
         symbols))))



