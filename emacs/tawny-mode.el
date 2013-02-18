;; The contents of this file are subject to the GPL License, Version 3.0.
;; 
;; Copyright (C) 2013, Phillip Lord, Newcastle University
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'nrepl)
(require 'clojure-mode)
(require 'easymenu)


(defun tawny-mode-check-for-nrepl-buffer ()
  (if (find-if
           (lambda (buffer)
             (lexical-let ((buffer (get-buffer buffer)))
               (equal (nrepl-project-directory-for (nrepl-current-dir))
                      (buffer-local-value 'nrepl-project-dir buffer))))
           nrepl-connection-list)
      t
    (message 
     "No nREPL buffer exists. Please use `nrepl-jack-in'")
    nil
    ))

;; select reasoner
(defun tawny-mode-select-reasoner (reasoner)
  "Selects the current reasoner"
  (interactive 
   (list 
    (completing-read 
     "Select reasoner: "
     '("hermit" "elk")
     nil t)))
  (when (tawny-mode-check-for-nrepl-buffer)
    (if (equal "" reasoner)
        (message "You must choose a reasoner")
      (tawny-mode-nrepl-reasoner-eval-string
       (format
        "(do (require 'tawny.emacs)(tawny.emacs/set-reasoner :%s))" 
        reasoner)))))

(defun tawny-mode-is-coherent ()
  (interactive)
  (when (tawny-mode-check-for-nrepl-buffer)
    (tawny-mode-nrepl-reasoner-eval-string
     (format 
      "(do (require 'tawny.emacs)(tawny.emacs/is-coherent \"%s\"))"
      (clojure-find-ns)
      ))))

(defun tawny-mode-is-consistent ()
  (interactive)
  (when (tawny-mode-check-for-nrepl-buffer)  
    (tawny-mode-nrepl-reasoner-eval-string
     (format 
      "(do (require 'tawny.emacs)(tawny.emacs/is-consistent \"%s\"))"
      (clojure-find-ns)
      ))))

(defun tawny-mode-unsatisfiable ()
  (interactive)
  (when (tawny-mode-check-for-nrepl-buffer)
    (nrepl-send-string
     (format 
      "(do (require 'tawny.emacs)(tawny.emacs/get-unsatisfiable \"%s\"))"
      (clojure-find-ns))
     (tawny-mode-unsatisfiable-response-handler (current-buffer)))))


(defvar tawny-trace-buffer (get-buffer-create "*tawny-trace*"))

(defun tawny-message (string &rest values)
  (let ((msg (apply 'format string values)))
    (save-excursion
      (set-buffer tawny-trace-buffer)
      (goto-char (point-max))
      (insert (format "%s: %s\n" (current-time-string) msg)))
    (message msg)))

(defun tawny-mode-nrepl-reasoner-eval-string (string)
  (nrepl-send-string string 
                     (tawny-mode-make-reasoner-response-handler (current-buffer))))


(defun tawny-mode-make-reasoner-response-handler (buffer)
  (nrepl-make-response-handler 
   buffer
   (lambda (buffer value)
     (tawny-message "For %s: %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Output: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Error: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Complete: %s %s" buffer value))))

(defvar tawny-mode-unsatisfiable-buffer
  (get-buffer-create "*tawny-unsatisfiable*"))

(defun tawny-mode-unsatisfiable-response-handler (buffer)
  (nrepl-make-response-handler 
   buffer
   (lambda (buffer value)
     (save-excursion
       (set-buffer tawny-mode-unsatisfiable-buffer)
       (erase-buffer)
       (message value)
       (insert (format "Unsatisfiable classes for %s:\n%s" buffer 
                       (tawny-de-escape value))))
     (display-buffer tawny-mode-unsatisfiable-buffer))
   (lambda (buffer value)
     (tawny-message "Output: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Error: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Complete: %s %s" buffer value))))


(defun tawny-doc (query)
  "Opens a window with docstring for QUERY."
  (interactive "P")
  (nrepl-read-symbol-name "Term: " 'tawny-doc-handler query))

(defun tawny-doc-handler (symbol)
  (let ((form (format "(do (require 'tawny.repl)(tawny.repl/print-doc %s))" symbol))
        (doc-buffer (nrepl-popup-buffer nrepl-doc-buffer t)))
    (nrepl-send-string form
                       (nrepl-popup-eval-out-handler doc-buffer)
                       nrepl-buffer-ns
                       (nrepl-current-tooling-session))))



(defun tawny-de-escape (string)
  (replace-regexp-in-string
   "\\\\n" "\n"
   (replace-regexp-in-string 
    "\\\"" ""
    string)))

;; nrepl-make-response-handler gives me a finer response handler. 
;; should be able to plug this into working or equivalent, to give a nice
;; output. 

;; need an "ensure-factory" thing in tawny.reasoner, so I can set the reasoner
;; everytime. 
(defvar tawy-mode-menu-map nil)

(defvar tawny-mode-map 
  (let ((map (make-sparse-keymap)))
    (easy-menu-define tawny-mode-menu-map
      map "`tawny-minor-mode' menu"
      '("Tawny"
        ["Coherent" tawny-mode-is-coherent
         :help "Check ontology in current buffer for coherency"]
        ["Consistency" tawny-mode-is-consistent
         :help "Check ontology in current buffer for consistency"]
        ["Unsatisfiable" tawny-mode-unsatisfiable
         :help "Display Unsatisfiable Classes"]
        ("Reasoner"
         ["Hermit" (tawny-mode-select-reasoner "hermit")]
         ["Elk" (tawny-mode-select-reasoner "elk")]
         )
        ["Display Documentation" tawny-doc
         :help "Display documentation for entity at point"]
        )
      )

    
    (define-key map (kbd "C-c s c") 'tawny-mode-is-coherent)
    (define-key map (kbd "C-c s v") 'tawny-mode-is-consistent)
    (define-key map (kbd "C-c s u") 'tawny-mode-unsatisfiable)
    (define-key map (kbd "C-c s d") 'tawny-doc)
    map
    ))

(define-minor-mode tawny-mode
  "Interact with an ontology in tawny-owl."
  nil " Tawny" tawny-mode-map

  )

(defvar tawny-mode-block nil)

(defun tawny-mode-maybe-enable ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless tawny-mode-block
      (when 
          (re-search-forward
           (regexp-opt 
            '("defontology" "defoproperty" "defannotationproperty") 'symbols)
           (point-max) t)
        (tawny-mode 1)))))


(provide 'tawny-mode)
