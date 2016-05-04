;;; tawny-mode.el --- Ontology Editing with Tawny-OWL -*- lexical-binding: t -*-

;;; Header:

;; This file is not part of Emacs

;; Author: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Maintainer: Phillip Lord <phillip.lord@newcastle.ac.uk>
;; Version: 1.5.0
;; Package-Requires: ((cider "0.12")(emacs "25"))

;; The contents of this file are subject to the GPL License, Version 3.0.
;;
;; Copyright (C) 2013, 2016, Phillip Lord, Newcastle University
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


;;; Commentary:
;;

(require 'cider-client)
(require 'nrepl-client)
(require 'clojure-mode)
(require 'easymenu)

;;; Code:

(defun tawny-mode-check-for-nrepl-buffer ()
  "Error if there is no NREPL buffer."
  (unless (nrepl-connection-buffer-name)
    (error
     "No nREPL buffer exists. Please use `cider-jack-in'")))

;; select reasoner
(defun tawny-mode-select-reasoner (reasoner)
  "Select the current reasoner.

REASONER is the name of the reasoner, and must be one of hermit,
elk or jfact."
  (interactive
   (list
    (completing-read
     "Select reasoner: "
     '("hermit" "elk" "jfact")
     nil t)))
  (tawny-mode-check-for-nrepl-buffer)
  (if (equal "" reasoner)
      (message "You must choose a reasoner")
    (tawny-mode-nrepl-reasoner-eval-string
     (format
      "(do (require 'tawny.emacs)(tawny.emacs/set-reasoner :%s))"
      reasoner))))

(defun tawny-mode-is-coherent ()
  "Prints a message describing coherency of current ontology."
  (interactive)
  (tawny-mode-check-for-nrepl-buffer)
  (tawny-mode-nrepl-reasoner-eval-string
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/is-coherent \"%s\"))"
    (clojure-find-ns))))

(defun tawny-mode-is-consistent ()
  "Prints a message describing consistency of current ontology."
  (interactive)
  (tawny-mode-check-for-nrepl-buffer)
  (tawny-mode-nrepl-reasoner-eval-string
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/is-consistent \"%s\"))"
    (clojure-find-ns)
    )))

(defun tawny-mode-unsatisfiable ()
  "Prints a message describing unsatisfiability of current ontology."
  (interactive)
  (tawny-mode-check-for-nrepl-buffer)
  (tawny-mode-display-classes-clear)
  (nrepl-request:eval
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/get-unsatisfiable \"%s\"))"
    (clojure-find-ns))
   (tawny-mode-class-list-response-handler
    (current-buffer)
    "Unsatisfiable classes for")
   (cider-current-connection)
   (cider-current-tooling-session)))

(defvar tawny-interaction-buffer
  (get-buffer-create "*tawny-interaction*")
  "A buffer for interaction with tawny.")

(defun tawny-message (string &rest values)
  "Add message to `tawny-interaction-buffer'.
Argument STRING is a format string.
Optional argument VALUES are the values for the format string."
  (let ((msg (apply 'format string values)))
    (save-excursion
      (set-buffer tawny-interaction-buffer)
      (goto-char (point-max))
      (insert (format "%s: %s\n" (current-time-string) msg)))
    (message msg)))

(defun tawny-mode-nrepl-reasoner-eval-string (string)
  "Eval STRING in clojure."
  (nrepl-request:eval
   string
   (tawny-mode-make-reasoner-response-handler (current-buffer))
   (cider-current-connection)
   (cider-current-tooling-session)))

(defun tawny-mode-make-reasoner-response-handler (buffer)
  "Return a messaging response handler for BUFFER."
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (tawny-message "For %s: %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Output: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Error: %s %s" buffer value))
   (lambda (buffer)
     (tawny-message "Complete: %s" buffer))))

(defvar tawny-mode-class-list-buffer
  "*tawny-classes*"
  "Name of the Class list buffer.")

(defun tawny-mode-display-classes-clear()
  "Clear the class list buffer."
  (when (buffer-live-p (get-buffer tawny-mode-class-list-buffer))
    (save-excursion
      (set-buffer tawny-mode-class-list-buffer)
      (erase-buffer))))

(defun tawny-mode-display-classes (message buffer classes)
  "Display a list of classes.

Argument MESSAGE is a message.
Argument BUFFER is the buffer.
Argument CLASSES is the list of classes."
  (save-excursion
    (set-buffer
     (get-buffer-create tawny-mode-class-list-buffer))
    (insert (format "%s %s:\n%s"
                    message
                    buffer
                    (tawny-de-escape classes))))
  (display-buffer tawny-mode-class-list-buffer))

(defun tawny-mode-class-list-response-handler (buffer message)
  "Return a response handler which display classes.
Argument BUFFER is the buffer.
Argument MESSAGE is the message."
  (nrepl-make-response-handler
   buffer
   (lambda (buffer value)
     (tawny-mode-display-classes
      message buffer value))
   (lambda (buffer value)
     (tawny-message "Output: %s %s" buffer value))
   (lambda (buffer value)
     (tawny-message "Error: %s %s" buffer value))
   (lambda (buffer)
     (tawny-message "Complete: %s" buffer))))


(defun tawny-mode-save ()
  "Save the current namespace and open the file if needed."
  (interactive)
  (tawny-mode-check-for-nrepl-buffer)
  (nrepl-request:eval
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/save-namespace-ontology \"%s\"))"
    (clojure-find-ns))
   (tawny-mode-save-handler (current-buffer))
   (cider-current-connection)
   (cider-current-tooling-session)))

(defun tawny-mode-save-handler (buffer)
  "Open the ontology BUFFER after Clojure has saved it."
  (nrepl-make-response-handler
   buffer
   (lambda (&rest value))
   (lambda (&rest stdout))
   (lambda (&rest sterr))
   (lambda (&rest done)
     (let ((o-omn
            (concat (clojure-project-dir) "o.omn")))
       (if-let ((b (find-buffer-visiting o-omn)))
           (display-buffer b)
         (with-current-buffer
             (find-file o-omn)
           (auto-revert-mode 1)
           (display-buffer (current-buffer))))))))

(defun tawny-doc (query)
  "Opens a window with docstring for QUERY."
  (interactive "P")
  (cider-read-symbol-name "Term: " 'tawny-doc-handler))

(defun tawny-doc-handler (symbol)
  "Return a documentation popup.
Argument SYMBOL is the symbol to document."
  (let ((form (format "(do (require 'tawny.repl)(tawny.repl/print-doc %s))" symbol))
        (doc-buffer (cider-popup-buffer cider-doc-buffer t)))
    (nrepl-request:eval form
                       (cider-popup-eval-out-handler doc-buffer)
                       (cider-current-connection)
                       (cider-current-tooling-session)
                       (cider-current-ns))))

(defun tawny-de-escape (string)
  "Remove escape sequences from STRING."
  (replace-regexp-in-string
   "\\\\n" "\n"
   (replace-regexp-in-string
    "\\\"" ""
    string)))

(defun tawny-mode-inferred-superclasses (class)
  "Show the inferred superclasses of CLASS."
  (interactive "P")
  (cider-read-symbol-name "Class:" 'tawny-mode--inferred-superclasses-1))

(defun tawny-mode--inferred-superclasses-1 (class)
  "Support function for `tawny-mode-inferred-superclasses'.
Argument CLASS is the class."
  (tawny-mode-check-for-nrepl-buffer)
  (tawny-mode-display-classes-clear)
  (nrepl-request:eval
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/get-inferred-superclasses \"%s\" \"%s\"))"
    (clojure-find-ns)
    (thing-at-point 'symbol t))
   (tawny-mode-class-list-response-handler
    (current-buffer)
    "Inferred superclasses for")
   (cider-current-connection)
   (cider-current-tooling-session)))

(defun tawny-mode-inferred-subclasses (class)
  "Show the inferred subclasses of CLASS."
  (interactive "P")
  (cider-read-symbol-name "Class:" 'tawny-mode--inferred-subclasses-1))

(defun tawny-mode--inferred-subclasses-1 (class)
  "Support function for `tawny-mode-inferred-subclasses'.
Argument CLASS is the class."
  (interactive)
  (tawny-mode-check-for-nrepl-buffer)
  (tawny-mode-display-classes-clear)
  (nrepl-request:eval
   (format
    "(do (require 'tawny.emacs)(tawny.emacs/get-inferred-subclasses \"%s\" \"%s\"))"
    (clojure-find-ns)
    (thing-at-point 'symbol t))
   (tawny-mode-class-list-response-handler
    (current-buffer)
    "Inferred subclasses for")
   (cider-current-connection)
   (cider-current-tooling-session)))


;; Protege section
(defvar tawny-mode-protege-entity-last nil)
(defun tawny-mode-protege-entity ()
  "Track the entity at point in Protege."
  (interactive)
  (when (equal major-mode 'clojure-mode)
    (let* ((thing
            (substring-no-properties
             (thing-at-point 'symbol)))
           (thing-split
            (split-string thing "/"))
           (thing
            (or (cadr thing-split)
                (car thing-split)))
           (form
            (format
             (concat
              "(try (require 'tawny.protege-nrepl)"
              ;; use eval we get compilation errors if tawny.protege-nrepl isn't
              ;; defined.
              "(eval '(tawny.protege-nrepl/display-maybe \"%s\" \"%s\"))"
              "(catch Exception exp :not-protege)"
              ")")
             (clojure-find-ns) thing)))
      (unless (equal thing tawny-mode-protege-entity-last)
        (setq tawny-mode-protege-entity-last thing)
        (nrepl-request:eval
         form
         (tawny-mode-nrepl-protege-display-handler (current-buffer))
         (cider-current-connection)
         (cider-current-tooling-session)
         cider-buffer-ns)))))

(defun tawny-mode-nrepl-protege-display-handler (buffer)
  "Return a response handler for Protege.

This is null, as everything happens by side effect.
Argument BUFFER is the buffer."
  (nrepl-make-response-handler
   buffer
   (lambda (&rest value))
   (lambda (&rest stdout))
   (lambda (&rest stderr))
   (lambda (&rest done))))


(defvar tawny-mode-protege-track-timer
  "Timer for protege tracking. "
  nil)

(defun tawny-mode-protege-entity-idle ()
  "Function which appears to do nothing."
  (tawny-mode-protege-entity))

(defun tawny-mode-protege-track-toggle ()
  "Toggle tracking of entities in protege."
  (interactive)
  (setq tawny-mode-protege-track-timer
        (if tawny-mode-protege-track-timer
            (progn (cancel-timer tawny-mode-protege-track-timer)
                   nil)
          (run-with-idle-timer 0.5 t 'tawny-mode-protege-entity-idle))))


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
        ["Save" tawny-mode-save
         :help "Save current ontology and open in Emacs"]
        ["Coherent" tawny-mode-is-coherent
         :help "Check ontology in current buffer for coherency"]
        ["Consistency" tawny-mode-is-consistent
         :help "Check ontology in current buffer for consistency"]
        ["Unsatisfiable" tawny-mode-unsatisfiable
         :help "Display Unsatisfiable Classes"]
        ["Inferred Superclasses" tawny-mode-inferred-superclasses
         :help "Display Inferred Superclasses"]
        ["Inferred Subclasses" tawny-mode-inferred-subclasses
         :help "Display Inferred Superclasses"]

        ("Reasoner"
         ["Hermit" (tawny-mode-select-reasoner "hermit")]
         ["Elk" (tawny-mode-select-reasoner "elk")]
         )
        ("Protege"
         ["Display Entity" tawny-mode-protege-entity
          :help "Display Entity in Protege (assumes use of protege-nrepl)"]
         ["Activate Ontology" tawny-mode-protege-ontology
          :help "Activate Ontology "
          ]
         ["Track Entity" tawny-mode-protege-track-toggle
          :help "Track the nearest entity to the cursor in Protege"
          :active t :style radio :selected tawny-mode-protege-track-timer]
         ["Do Not Track Entity" tawny-mode-protege-track-toggle
          :help "Do not track the nearest entity to the cursor in Protege"
          :active t :style radio :selected (not tawny-mode-protege-track-timer)])
        ["Display Documentation" tawny-doc
         :help "Display documentation for entity at point"]
        )
      )

    (define-key map (kbd "C-c . s") 'tawny-mode-save)
    (define-key map (kbd "C-c . p") 'tawny-mode-inferred-superclasses)
    (define-key map (kbd "C-c . b") 'tawny-mode-inferred-subclasses)
    (define-key map (kbd "C-c . c") 'tawny-mode-is-coherent)
    (define-key map (kbd "C-c . v") 'tawny-mode-is-consistent)
    (define-key map (kbd "C-c . u") 'tawny-mode-unsatisfiable)
    (define-key map (kbd "C-c . d") 'tawny-doc)
    map
    ))

;;;###autoload
(define-minor-mode tawny-mode
  "Interact with an ontology in tawny-owl."
  nil " Tawny" tawny-mode-map
  (goto-address-prog-mode tawny-mode))

(defvar tawny-mode-block nil)

(defun tawny-mode-maybe-enable ()
  "Enable tawny based on heuristics."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (unless tawny-mode-block
      (when
          (re-search-forward
           (regexp-opt
            '("defontology" "defoproperty" "defannotationproperty" "tawny.owl") 'symbols)
           (point-max) t)
        (tawny-mode 1)))))


(provide 'tawny-mode)
;;; tawny-mode.el ends here
