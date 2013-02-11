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
;; select reasoner
(defun tawny-mode-select-reasoner (reasoner)
  "Selects the current reasoner"
  (interactive 
   (list 
    (completing-read 
     "Select reasoner: "
     '("hermit" "elk")
     nil t)))
  (if (equal "" reasoner)
      (message "You must choose a reasoner")
    (tawny-mode-nrepl-reasoner-eval-string
     (format
      "(do (require 'tawny.emacs)(tawny.emacs/set-reasoner :%s))" 
      reasoner))))

(defun tawny-mode-is-coherent ()
  (interactive)
  (tawny-mode-nrepl-reasoner-eval-string
   (format 
    "(do (require 'tawny.emacs)(tawny.emacs/is-coherent \"%s\"))"
    (clojure-find-ns)
    )))

(defun tawny-mode-is-consistent ()
  (interactive)
  (tawny-mode-nrepl-reasoner-eval-string
   (format 
    "(do (require 'tawny.emacs)(tawny.emacs/is-consistent \"%s\"))"
    (clojure-find-ns)
    )))

(defun tawny-mode-unsatisfiable ()
  (interactive)
  (tawny-mode-nrepl-reasoner-eval-string
   (format 
    "(do (require 'tawny.emacs)(tawny.emacs/get-unsatisfiable \"%s\"))"
    (clojure-find-ns)
    )))





(defun tawny-mode-nrepl-reasoner-eval-string (string)
  (nrepl-send-string string 
   (tawny-mode-make-reasoner-response-handler (current-buffer))))

(defun tawny-mode-make-reasoner-response-handler (buffer)
  (nrepl-make-response-handler 
   buffer
   (lambda (buffer value)
     (message "Value handler: %s %s" buffer value))
   (lambda (buffer value)
     (message "stdout handler: %s %s" buffer value))
   (lambda (buffer value)
     (message "stderr handler: %s %s" buffer value))
   (lambda (buffer value)
     (message "done handler: %s %s" buffer value))))

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
        )
      )

    
    (define-key map (kbd "C-c s c") 'tawny-mode-is-coherent)
    (define-key map (kbd "C-c s v") 'tawny-mode-is-consistent)
    (define-key map (kbd "C-c s u") 'tawny-mode-unsatisfiable)
    map
    ))

(define-minor-mode tawny-mode
  "Interact with an ontology in tawny-owl."
  nil " Tawny" tawny-mode-map

  )




(provide 'tawny-mode)
