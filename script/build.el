;;
;; Generate lentic documentation
;;
(require 'lentic)
(require 'commander)
(require 'load-relative)

(defun gensource-and-report (file init)
  (message "Cloning %s..."
           file)
  (let ((config
         (car
          (lentic-batch-clone-and-save-with-config
           file init))))
    (message "Cloning %s...done" file)
    (message "For %s generated %s."
             file
             (oref config :lentic-file))))

(require 'lentic-doc)
(defun gensource-gen-if-necessary (file)
  (let* ((target
          ;; switch for f-swap-ext in due course -- remove lentic-doc require!
          (lentic-f-swap-ext file "org"))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skiping %s due to lock %s" file locked)
      (if (file-newer-than-file-p file target)
          (gensource-and-report file 'lentic-clojure-org-init)
        (message "File uptodate: %s" file)))))


(defun gensource-clean-if-possible (file)
  (let* ((target
          ;; switch for f-swap-ext in due course -- remove lentic-doc require!
          (lentic-f-swap-ext file "org"))
         (locked
          (or (file-locked-p file)
              (file-locked-p target))))
    (if locked
        (message "Skipping %s due to lock %s" file locked)
      (message "Cleaning %s..." target)
      (f-delete target)
      (message "Cleaning %s...done" target))))

(defun build/gen-html ()
  (with-current-buffer
      (find-file-noselect "tawny.org")
    (let ((org-export-htmlize-generate-css 'css))
      (org-html-export-to-html))))

(defun build/gen-src ()
  (mapc
   (lambda (file)
     (gensource-gen-if-necessary
      (concat "./src/tawny/" file)))
   build-sources))

(defun build/clean-src ()
  (mapc
   (lambda (file)
     (gensource-clean-if-possible
      (concat "./src/tawny/" file)))
   build-sources))

(defvar build-sources
  '("pattern.clj" "owl.clj"))

(commander
 (command "gen-html" "Generate HTML documentation" build/gen-html)
 (command "gen-src" "Generate Org from Clojure" build/gen-src)
 (command "clean-src" "Clean Org from Clojure" build/clean-src))
