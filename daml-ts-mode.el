;;; daml-ts-mode.el --- Tree-sitter major mode for Daml  -*- lexical-binding: t; -*-

;; Copyright (C) 2026

;; Author: esrh
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (haskell-mode "17.5"))
;; Version: 0.0.1
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Daml mode using tree-sitter, with eglot config for dpm/daml.
;; Major mode for editing Daml using Emacs' built-in tree-sitter support.
;;
;; The tree-sitter grammar needs to be installed. A default url is provided.
;;
;; We use `haskell-indentation-mode' for indentation.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'treesit)
(require 'haskell-indentation)

(defvar eldoc-documentation-functions)
(defvar eglot-server-programs)
(defvar markdown-code-lang-modes)
(declare-function eglot--TextDocumentPositionParams "eglot" ())
(declare-function eglot--current-server-or-lose "eglot" ())
(declare-function eglot--highlight-piggyback "eglot" (cb))
(declare-function eglot--hover-info "eglot" (contents &optional range))
(declare-function eglot-hover-eldoc-function "eglot" (callback))
(declare-function eglot-managed-p "eglot" ())
(declare-function eglot-server-capable "eglot" (&rest feats))
(declare-function jsonrpc-async-request "jsonrpc"
                  (connection method params &rest args))

(defgroup daml-ts nil
  "Major mode for editing Daml with tree-sitter."
  :group 'languages
  :prefix "daml-ts-")

(defvar daml-ts-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?- ". 12" table)
    (modify-syntax-entry ?\n ">" table)
    (modify-syntax-entry ?_ "w" table)
    (modify-syntax-entry ?' "w" table)
    table)
  "Syntax table for `daml-ts-mode'.")

(defconst daml-ts-mode--keywords
  '("anyclass" "as" "authority" "case" "catch" "choice" "class" "consuming"
    "controller" "data" "deriving" "do" "else" "ensure" "exception" "for"
    "forall" "hiding" "if" "implements" "import" "in" "infix" "infixl"
    "infixr" "instance" "interface" "key" "let" "maintainer" "mdo" "message"
    "module" "newtype" "nonconsuming" "observer" "of" "pattern"
    "postconsuming" "preconsuming" "qualified" "rec" "requires" "signatory"
    "stock" "template" "then" "try" "type" "via" "viewtype" "where" "with")
  "Daml keywords used for tree-sitter font-lock.")

(defconst daml-ts-mode--eglot-commands
  '(("dpm" "damlc" "multi-ide")
    ("daml" "damlc" "multi-ide"))
  "Daml language-server commands, in preference order.")

(defun daml-ts-mode--eglot-candidate (command)
  "Return an Eglot selection candidate for COMMAND, if available.

The return value is (LABEL . CONTACT).  LABEL is shown to users; CONTACT is the
command line Eglot should run, with the executable resolved to an absolute path."
  (when-let* ((program (executable-find (car command))))
    (cons (string-join command " ")
          (cons program (cdr command)))))

(defun daml-ts-mode--eglot-candidates ()
  (delq nil (mapcar #'daml-ts-mode--eglot-candidate
                    daml-ts-mode--eglot-commands)))

(defun daml-ts-mode--eglot-command-list ()
  (mapconcat (lambda (command) (string-join command " "))
             daml-ts-mode--eglot-commands ", "))

(defun daml-ts-mode--eglot-contact (interactive _project)
  "Return an Eglot contact for the Daml language server.

Unlike `eglot-alternatives', this distinguishes commands that share the same
executable but use different arguments."
  (unless (and interactive current-prefix-arg)
    (let ((candidates (daml-ts-mode--eglot-candidates)))
      (cond
       ((null candidates)
        (if interactive
            nil
          (error "No Daml language server executable found; tried: %s"
                 (daml-ts-mode--eglot-command-list))))
       ((and interactive (cdr candidates))
        (cdr (assoc (completing-read
                     "[eglot] Daml language server command: "
                     (mapcar #'car candidates)
                     nil t nil nil (caar candidates))
                    candidates)))
       (t
        (cdar candidates))))))

(defconst daml-ts-mode--treesit-language-source
  '(daml "https://github.com/Artifex1/tree-sitter-daml")
  "Default tree-sitter grammar recipe for Daml.")

(unless (assoc 'daml treesit-language-source-alist)
  (add-to-list 'treesit-language-source-alist
               daml-ts-mode--treesit-language-source))

(defun daml-ts-mode--setup-markdown-code-blocks ()
  "Teach `markdown-mode' to fontify ```daml blocks with `daml-ts-mode'."
  (unless (assoc "daml" markdown-code-lang-modes)
    (add-to-list 'markdown-code-lang-modes '("daml" . daml-ts-mode))))

(defun daml-ts-mode--first-hover-markup-value (contents)
  "Return the first markup value string from LSP hover CONTENTS."
  (cond
   ((stringp contents)
    contents)
   ((vectorp contents)
    (catch 'value
      (seq-doseq (content contents)
        (when-let* ((value (daml-ts-mode--first-hover-markup-value content)))
          (throw 'value value)))))
   ((and (consp contents)
         (stringp (plist-get contents :value)))
    (plist-get contents :value))))

(defun daml-ts-mode--first-fenced-code-block (markdown)
  "Return the first fenced code block body from MARKDOWN."
  (let ((lines (split-string markdown "\n"))
        in-block
        block-lines)
    (catch 'block
      (dolist (line lines)
        (let ((trimmed (string-trim line)))
          (cond
           ((and in-block (string-prefix-p "```" trimmed))
            (throw 'block (string-trim (string-join (nreverse block-lines) "\n"))))
           (in-block
            (push line block-lines))
           ((string-prefix-p "```" trimmed)
            (setq in-block t)))))
      nil)))

(defun daml-ts-mode--fontify-snippet (string)
  "Return STRING fontified with `daml-ts-mode' when possible."
  (if (not (treesit-ready-p 'daml))
      string
    (condition-case nil
        (with-temp-buffer
          (insert string)
          (delay-mode-hooks (daml-ts-mode))
          (font-lock-ensure)
          (buffer-string))
      (error string))))

(defun daml-ts-mode--compact-hover-echo (contents)
  "Return a compact echo-area string for Daml hover CONTENTS."
  (when-let* ((value (daml-ts-mode--first-hover-markup-value contents))
              (code (daml-ts-mode--first-fenced-code-block value)))
    (let ((lines (split-string code "\n" t "[ \t\r]+")))
      (unless (null lines)
        (daml-ts-mode--fontify-snippet (string-join lines " "))))))

(defun daml-ts-mode--eglot-hover-eldoc-function (callback)
  "Daml-aware replacement for `eglot-hover-eldoc-function'.

The Daml language server returns hover signatures as multiline markdown code
blocks.  Eglot's default `:echo' value truncates those blocks at the first
newline, so the echo area shows only the identifier.  This function keeps the
full formatted hover text for documentation buffers, but supplies Eldoc with a
single-line signature for echo-area display."
  (when (eglot-server-capable :hoverProvider)
    (let ((buf (current-buffer)))
      (jsonrpc-async-request
       (eglot--current-server-or-lose)
       :textDocument/hover (eglot--TextDocumentPositionParams)
       :success-fn
       (lambda (hover)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (let* ((contents (plist-get hover :contents))
                    (range (plist-get hover :range))
                    (info (unless (seq-empty-p contents)
                            (eglot--hover-info contents range)))
                    (echo (daml-ts-mode--compact-hover-echo contents)))
               (funcall callback info
                        :echo (or (and (not (string-empty-p (or echo "")))
                                       echo)
                                  (and info (string-match "\n" info))))))))
       :deferred :textDocument/hover))
    ;; Highlight via the old piggyback helper on eglot <= 1.17.x; newer eglot has the standalone `eglot-highlight-eldoc-function'.
    (when (fboundp 'eglot--highlight-piggyback)
      (eglot--highlight-piggyback callback))
    t))

(defun daml-ts-mode--setup-eglot-eldoc ()
  "Use Daml-specific hover formatting in Eglot-managed Daml buffers."
  (when (derived-mode-p 'daml-ts-mode)
    (if (eglot-managed-p)
        (progn
          (remove-hook 'eldoc-documentation-functions
                       #'eglot-hover-eldoc-function t)
          (add-hook 'eldoc-documentation-functions
                    #'daml-ts-mode--eglot-hover-eldoc-function -10 t))
      (remove-hook 'eldoc-documentation-functions
                   #'daml-ts-mode--eglot-hover-eldoc-function t))))

(defun daml-ts-mode--font-lock-settings ()
  "Return tree-sitter font-lock settings for Daml."
  (treesit-font-lock-rules
   :language 'daml
   :feature 'comment
   '((comment) @font-lock-comment-face
     (haddock) @font-lock-doc-face)

   :language 'daml
   :feature 'string
   '((string) @font-lock-string-face
     (char) @font-lock-string-face)

   :language 'daml
   :feature 'constant
   '((integer) @font-lock-number-face
     (float) @font-lock-number-face
     (unit) @font-lock-constant-face)

   :language 'daml
   :feature 'keyword
   `([,@daml-ts-mode--keywords] @font-lock-keyword-face)

   :language 'daml
   :feature 'operator
   '([(operator)
      (constructor_operator)
      (all_names)
      (wildcard)
      "."
      ".."
      "="
      "|"
      ":"
      "=>"
      "->"
      "<-"
      "\\"
      "`"
      "@"]
     @font-lock-operator-face)

   :language 'daml
   :feature 'definition
   '((decl/function name: (variable) @font-lock-function-name-face)
     (decl/bind name: (variable) @font-lock-function-name-face)
     (decl/signed_definition name: (variable) @font-lock-function-name-face)
     (decl/signature names: (binding_list (variable) @font-lock-function-name-face)))

   :language 'daml
   :feature 'function
   '((apply
      [(expression/variable) (expression/qualified (variable))]
      @font-lock-function-call-face))

   :language 'daml
   :feature 'type
   '((name) @font-lock-type-face
     (type/star) @font-lock-type-face
     (constructor) @font-lock-type-face)

   :language 'daml
   :feature 'variable
   '((field_name (variable) @font-lock-property-use-face)
     (pattern/variable) @font-lock-variable-name-face
     (variable) @font-lock-variable-use-face)))

(defun daml-ts-mode--setup-indent ()
  "Set up indentation for `daml-ts-mode'."
  (setq-local haskell-literate nil)
  (haskell-indentation-mode 1))

;;;###autoload
(define-derived-mode daml-ts-mode prog-mode "Daml[ts]"
  "Major mode for editing Daml using tree-sitter."
  :syntax-table daml-ts-mode-syntax-table
  (unless (treesit-ready-p 'daml)
    (error "Tree-sitter grammar for Daml is not available; install it with `treesit-install-language-grammar'"))
  (treesit-parser-create 'daml)
  (setq-local treesit-font-lock-settings (daml-ts-mode--font-lock-settings))
  (setq-local treesit-font-lock-feature-list
              '((comment string)
                (keyword type constant)
                (definition operator)
                (function variable)))
  (setq-local comment-start "--")
  (setq-local comment-start-skip "\\(?:--+\\|{-\\)\\s-*")
  (setq-local comment-end "")
  (setq-local indent-tabs-mode nil)
  (daml-ts-mode--setup-indent)
  (treesit-major-mode-setup))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.daml\\'" . daml-ts-mode))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((daml-ts-mode :language-id "daml")
                 . daml-ts-mode--eglot-contact))
  (add-hook 'eglot-managed-mode-hook #'daml-ts-mode--setup-eglot-eldoc))

(with-eval-after-load 'markdown-mode
  (daml-ts-mode--setup-markdown-code-blocks))

(provide 'daml-ts-mode)

;;; daml-ts-mode.el ends here
