;;; mixed-html-mode.el --- Major mode for HTML with inline CSS and JS  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Jeff Kaufman
;; Author: Jeff Kaufman
;; Version: 0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: languages, html, css, javascript

;;; Commentary:

;; A major mode for editing HTML files with inline CSS and JS.
;; It correctly identifies <style> and <script> regions and applies
;; language-appropriate syntax highlighting within each region.
;;
;; Region boundaries are determined the same way a browser would:
;; a literal </script> or </style> ends the region even if it appears
;; inside a JS string.

;;; Code:


;;; Faces

(defface mixed-html-tag-face
  '((t :inherit font-lock-function-name-face))
  "Face for HTML tag names."
  :group 'mixed-html)

(defface mixed-html-attr-face
  '((t :inherit font-lock-variable-name-face))
  "Face for HTML attribute names."
  :group 'mixed-html)

(defface mixed-html-attr-value-face
  '((t :inherit font-lock-string-face))
  "Face for HTML attribute values."
  :group 'mixed-html)

(defface mixed-html-comment-face
  '((t :inherit font-lock-comment-face))
  "Face for comments in all languages."
  :group 'mixed-html)

(defface mixed-html-string-face
  '((t :inherit font-lock-string-face))
  "Face for string literals."
  :group 'mixed-html)

(defface mixed-html-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'mixed-html)

(defface mixed-html-builtin-face
  '((t :inherit font-lock-builtin-face))
  "Face for built-in names."
  :group 'mixed-html)

(defface mixed-html-constant-face
  '((t :inherit font-lock-constant-face))
  "Face for constants."
  :group 'mixed-html)

(defface mixed-html-css-property-face
  '((t :inherit font-lock-variable-name-face))
  "Face for CSS property names."
  :group 'mixed-html)

(defface mixed-html-css-selector-face
  '((t :inherit font-lock-function-name-face))
  "Face for CSS selectors."
  :group 'mixed-html)

(defface mixed-html-css-value-face
  '((t :inherit font-lock-string-face))
  "Face for CSS values."
  :group 'mixed-html)

;;; Region detection

(defun mixed-html--find-regions ()
  "Find all regions in the buffer.
Returns a list of (TYPE START END) where TYPE is `html', `css', or `js'.
START and END are buffer positions.  Regions cover the entire buffer
with no gaps."
  (save-excursion
    (goto-char (point-min))
    (let ((regions '())
          (html-start (point-min)))
      (while (re-search-forward "<\\(style\\|script\\)\\(?:\\s-[^>]*\\)?>" nil t)
        (let* ((tag-name (downcase (match-string 1)))
               (region-type (if (string= tag-name "style") 'css 'js))
               (content-start (point))
               (close-re (concat "</" tag-name ">")))
          ;; Record HTML region up to and including the opening tag
          (when (< html-start (match-beginning 0))
            (push (list 'html html-start (match-beginning 0)) regions))
          ;; Push the opening tag itself as HTML
          (push (list 'html (match-beginning 0) content-start) regions)
          ;; Find the closing tag — browser-style: literal </script> or
          ;; </style> always closes, even inside strings
          (if (re-search-forward close-re nil t)
              (progn
                (push (list region-type content-start (match-beginning 0)) regions)
                ;; The closing tag is HTML
                (push (list 'html (match-beginning 0) (point)) regions)
                (setq html-start (point)))
            ;; No closing tag found — rest of file is this region
            (push (list region-type content-start (point-max)) regions)
            (setq html-start (point-max)))))
      ;; Trailing HTML
      (when (< html-start (point-max))
        (push (list 'html html-start (point-max)) regions))
      (nreverse regions))))

;;; Keyword patterns

(defconst mixed-html--js-keywords
  '("async" "await" "break" "case" "catch" "class" "const"
    "continue" "debugger" "default" "delete" "do" "else"
    "export" "extends" "finally" "for" "function" "if"
    "import" "in" "instanceof" "let" "new" "of" "return"
    "static" "super" "switch" "throw" "try" "typeof"
    "var" "void" "while" "with" "yield")
  "JavaScript keywords.")

(defconst mixed-html--js-constants
  '("true" "false" "null" "undefined" "NaN" "Infinity" "this")
  "JavaScript constants.")

(defconst mixed-html--js-builtins
  '("Array" "Boolean" "console" "Date" "document" "Error"
    "Function" "JSON" "Math" "Number" "Object" "Promise"
    "RegExp" "String" "Symbol" "window" "navigator"
    "parseInt" "parseFloat" "setTimeout" "setInterval"
    "clearTimeout" "clearInterval" "fetch"
    "Map" "Set" "WeakMap" "WeakSet" "Proxy" "Reflect"
    "AudioContext" "AudioWorklet" "AudioWorkletNode"
    "MediaStreamAudioSourceNode" "XMLHttpRequest")
  "JavaScript built-in objects and functions.")

(defconst mixed-html--css-at-keywords
  '("@media" "@import" "@font-face" "@keyframes" "@supports"
    "@charset" "@namespace" "@page" "@viewport")
  "CSS at-rules.")

;;; Fontification engine

(defun mixed-html--fontify-string (start end quote-char)
  "Fontify a JS string from START to END delimited by QUOTE-CHAR.
START should be on the opening quote character itself.
Handles escape sequences.  Returns the position after the closing quote,
or END if unterminated."
  (let ((pos (1+ start))  ; skip opening quote
        (limit end))
    (while (and (< pos limit)
                (not (eq (char-after pos) quote-char)))
      (if (eq (char-after pos) ?\\)
          (setq pos (min (+ pos 2) limit))
        (setq pos (1+ pos))))
    (when (and (< pos limit) (eq (char-after pos) quote-char))
      (setq pos (1+ pos)))
    (put-text-property start pos 'face 'mixed-html-string-face)
    pos))

(defun mixed-html--fontify-js-template (start end)
  "Fontify a JS template literal from START to END.
START points to the opening backtick.  Handles ${} expressions
by recursively fontifying them as JS.  Returns position after
the closing backtick, or END if unterminated."
  (let ((pos (1+ start))
        (limit end))
    ;; Mark opening backtick as string
    (put-text-property start (1+ start) 'face 'mixed-html-string-face)
    (while (and (< pos limit)
                (not (eq (char-after pos) ?`)))
      (cond
       ;; Escape sequence
       ((eq (char-after pos) ?\\)
        (let ((esc-end (min (+ pos 2) limit)))
          (put-text-property pos esc-end 'face 'mixed-html-string-face)
          (setq pos esc-end)))
       ;; Template expression ${...}
       ((and (eq (char-after pos) ?$)
             (< (1+ pos) limit)
             (eq (char-after (1+ pos)) ?{))
        (put-text-property pos (+ pos 2) 'face 'mixed-html-string-face)
        (setq pos (+ pos 2))
        ;; Find matching close brace, handling nesting
        (let ((depth 1))
          (while (and (< pos limit) (> depth 0))
            (cond
             ((eq (char-after pos) ?{) (setq depth (1+ depth)) (setq pos (1+ pos)))
             ((eq (char-after pos) ?})
              (setq depth (1- depth))
              (if (= depth 0)
                  (progn
                    (put-text-property pos (1+ pos) 'face 'mixed-html-string-face)
                    (setq pos (1+ pos)))
                (setq pos (1+ pos))))
             (t
              ;; Fontify the expression content as JS
              (setq pos (mixed-html--fontify-js-token pos limit)))))))
       ;; Normal template character
       (t
        (put-text-property pos (1+ pos) 'face 'mixed-html-string-face)
        (setq pos (1+ pos)))))
    ;; Mark closing backtick
    (when (and (< pos limit) (eq (char-after pos) ?`))
      (put-text-property pos (1+ pos) 'face 'mixed-html-string-face)
      (setq pos (1+ pos)))
    pos))

(defun mixed-html--fontify-js-line-comment (start end)
  "Fontify a // comment from START to END.  Returns position after comment."
  (let ((eol (or (save-excursion
                    (goto-char start)
                    (end-of-line)
                    (point))
                  end)))
    (setq eol (min eol end))
    (put-text-property start eol 'face 'mixed-html-comment-face)
    eol))

(defun mixed-html--fontify-block-comment (start end)
  "Fontify a /* */ comment from START to END.  Returns position after comment."
  (let ((close (save-excursion
                 (goto-char (+ start 2))
                 (if (search-forward "*/" end t)
                     (point)
                   end))))
    (put-text-property start close 'face 'mixed-html-comment-face)
    close))

(defun mixed-html--fontify-js-regexp (start end)
  "Fontify a JS regex literal from START to END.
START is on the opening /.  Returns position after the closing / and flags."
  (let ((pos (1+ start))
        (limit end))
    (while (and (< pos limit)
                (not (eq (char-after pos) ?/))
                (not (eq (char-after pos) ?\n)))
      (cond
       ((eq (char-after pos) ?\\)
        (setq pos (min (+ pos 2) limit)))
       ((eq (char-after pos) ?\[)
        ;; Skip character class
        (setq pos (1+ pos))
        (while (and (< pos limit)
                    (not (eq (char-after pos) ?\]))
                    (not (eq (char-after pos) ?\n)))
          (if (eq (char-after pos) ?\\)
              (setq pos (min (+ pos 2) limit))
            (setq pos (1+ pos))))
        (when (and (< pos limit) (eq (char-after pos) ?\]))
          (setq pos (1+ pos))))
       (t
        (setq pos (1+ pos)))))
    (when (and (< pos limit) (eq (char-after pos) ?/))
      (setq pos (1+ pos))
      ;; Skip flags
      (while (and (< pos limit)
                  (let ((c (char-after pos)))
                    (or (and (>= c ?a) (<= c ?z))
                        (and (>= c ?A) (<= c ?Z)))))
        (setq pos (1+ pos))))
    (put-text-property start pos 'face 'mixed-html-string-face)
    pos))

(defun mixed-html--js-slash-is-regex-p (pos)
  "Return non-nil if a / at POS should be treated as a regex delimiter.
Uses the preceding non-whitespace character to decide: / is division
after identifiers, numbers, `)' and `]'; it is a regex opener after
operators, keywords, `(', `[', `{', `,', `;', `!', or at start of code."
  (save-excursion
    (goto-char pos)
    (skip-chars-backward " \t\n\r")
    (if (bobp)
        t
      (let ((c (char-before)))
        (not (or (and (>= c ?a) (<= c ?z))
                 (and (>= c ?A) (<= c ?Z))
                 (and (>= c ?0) (<= c ?9))
                 (eq c ?_) (eq c ?$)
                 (eq c ?\)) (eq c ?\])))))))

(defun mixed-html--fontify-js-token (pos end)
  "Fontify one JS token at POS, up to END.  Return new position."
  (cond
   ;; Whitespace
   ((and (< pos end)
         (memq (char-after pos) '(?\s ?\t ?\n ?\r)))
    (1+ pos))
   ;; Line comment
   ((and (< (1+ pos) end)
         (eq (char-after pos) ?/)
         (eq (char-after (1+ pos)) ?/))
    (mixed-html--fontify-js-line-comment pos end))
   ;; Block comment
   ((and (< (1+ pos) end)
         (eq (char-after pos) ?/)
         (eq (char-after (1+ pos)) ?*))
    (mixed-html--fontify-block-comment pos end))
   ;; Template literal
   ((and (< pos end) (eq (char-after pos) ?`))
    (mixed-html--fontify-js-template pos end))
   ;; String
   ((and (< pos end) (memq (char-after pos) '(?\" ?\')))
    (mixed-html--fontify-string pos end (char-after pos)))
   ;; Identifier or keyword
   ((and (< pos end)
         (let ((c (char-after pos)))
           (or (and (>= c ?a) (<= c ?z))
               (and (>= c ?A) (<= c ?Z))
               (eq c ?_) (eq c ?$))))
    (let ((word-start pos))
      (while (and (< pos end)
                  (let ((c (char-after pos)))
                    (or (and (>= c ?a) (<= c ?z))
                        (and (>= c ?A) (<= c ?Z))
                        (and (>= c ?0) (<= c ?9))
                        (eq c ?_) (eq c ?$))))
        (setq pos (1+ pos)))
      (let ((word (buffer-substring-no-properties word-start pos)))
        (cond
         ((member word mixed-html--js-keywords)
          (put-text-property word-start pos 'face 'mixed-html-keyword-face))
         ((member word mixed-html--js-constants)
          (put-text-property word-start pos 'face 'mixed-html-constant-face))
         ((member word mixed-html--js-builtins)
          (put-text-property word-start pos 'face 'mixed-html-builtin-face))))
      pos))
   ;; Number
   ((and (< pos end)
         (let ((c (char-after pos)))
           (and (>= c ?0) (<= c ?9))))
    (let ((num-start pos)
          (is-hex nil))
      ;; Check for 0x/0X hex prefix
      (when (and (eq (char-after pos) ?0)
                 (< (1+ pos) end)
                 (let ((c2 (char-after (1+ pos))))
                   (or (eq c2 ?x) (eq c2 ?X))))
        (setq is-hex t)
        (setq pos (+ pos 2)))
      (while (and (< pos end)
                  (let ((c (char-after pos)))
                    (or (and (>= c ?0) (<= c ?9))
                        (and is-hex (>= c ?a) (<= c ?f))
                        (and is-hex (>= c ?A) (<= c ?F))
                        (and (not is-hex) (eq c ?.))
                        (and (not is-hex) (or (eq c ?e) (eq c ?E))))))
        (setq pos (1+ pos)))
      (put-text-property num-start pos 'face 'mixed-html-constant-face)
      pos))
   ;; Regex literal — a / that isn't division
   ((and (< pos end)
         (eq (char-after pos) ?/)
         (mixed-html--js-slash-is-regex-p pos))
    (mixed-html--fontify-js-regexp pos end))
   ;; Everything else: punctuation, operators, etc.
   ((< pos end) (1+ pos))
   (t pos)))

(defun mixed-html--fontify-js-region (start end)
  "Fontify JavaScript between START and END."
  (let ((pos start))
    (while (< pos end)
      (setq pos (mixed-html--fontify-js-token pos end)))))

;;; CSS fontification

(defun mixed-html--fontify-css-region (start end)
  "Fontify CSS between START and END."
  (let ((pos start))
    (while (< pos end)
      (setq pos (mixed-html--fontify-css-token pos end start)))))

(defun mixed-html--fontify-css-token (pos end &optional region-start)
  "Fontify one CSS token at POS, up to END.  Return new position.
REGION-START bounds backward searches for brace context."
  (cond
   ;; Whitespace
   ((and (< pos end)
         (memq (char-after pos) '(?\s ?\t ?\n ?\r)))
    (1+ pos))
   ;; Block comment
   ((and (< (1+ pos) end)
         (eq (char-after pos) ?/)
         (eq (char-after (1+ pos)) ?*))
    (mixed-html--fontify-block-comment pos end))
   ;; String
   ((and (< pos end) (memq (char-after pos) '(?\" ?\')))
    (mixed-html--fontify-string pos end (char-after pos)))
   ;; At-rule
   ((and (< pos end) (eq (char-after pos) ?@))
    (let ((at-start pos))
      (setq pos (1+ pos))
      (while (and (< pos end)
                  (let ((c (char-after pos)))
                    (or (and (>= c ?a) (<= c ?z))
                        (and (>= c ?A) (<= c ?Z))
                        (eq c ?-))))
        (setq pos (1+ pos)))
      (put-text-property at-start pos 'face 'mixed-html-keyword-face)
      pos))
   ;; Selector or property context — we need to figure out if we're
   ;; inside a rule block or not.
   ((and (< pos end)
         (let ((c (char-after pos)))
           (or (and (>= c ?a) (<= c ?z))
               (and (>= c ?A) (<= c ?Z))
               (eq c ?_) (eq c ?-)
               (eq c ?.) (eq c ?#)
               (eq c ?:) (eq c ?*)
               (eq c ?\[))))
    (mixed-html--fontify-css-selector-or-property pos end region-start))
   ;; Open/close braces, semicolons, etc.
   ((< pos end) (1+ pos))
   (t pos)))

(defun mixed-html--css-in-rule-block-p (pos &optional bound)
  "Return non-nil if POS is inside a CSS rule block (between { and }).
BOUND, if non-nil, limits how far back the search goes."
  (let ((depth 0)
        (found nil))
    (save-excursion
      (goto-char pos)
      (while (and (not found)
                  (re-search-backward "[{}]" bound t))
        (cond
         ((eq (char-after) ?}) (setq depth (1+ depth)))
         ((eq (char-after) ?{)
          (if (> depth 0)
              (setq depth (1- depth))
            (setq found t))))))
    found))

(defun mixed-html--fontify-css-selector-or-property (pos end &optional region-start)
  "Fontify a CSS selector or property starting at POS up to END.
REGION-START bounds backward searches for brace context."
  (if (mixed-html--css-in-rule-block-p pos region-start)
      ;; Inside a rule block: this is a property name
      (let ((prop-start pos))
        (while (and (< pos end)
                    (not (memq (char-after pos) '(?: ?\; ?} ?\n))))
          (setq pos (1+ pos)))
        ;; Trim trailing whitespace from property name
        (let ((prop-end pos))
          (save-excursion
            (goto-char prop-end)
            (skip-chars-backward " \t" prop-start)
            (setq prop-end (point)))
          (when (> prop-end prop-start)
            (put-text-property prop-start prop-end 'face 'mixed-html-css-property-face)))
        ;; If we stopped at a colon, fontify the value
        (when (and (< pos end) (eq (char-after pos) ?:))
          (setq pos (1+ pos))  ; skip colon
          (let ((val-start pos))
            ;; Find end of value
            (while (and (< pos end)
                        (not (memq (char-after pos) '(?\; ?}))))
              (cond
               ;; Handle strings in values
               ((memq (char-after pos) '(?\" ?\'))
                (setq pos (mixed-html--fontify-string pos end (char-after pos))))
               ;; Handle comments in values
               ((and (< (1+ pos) end)
                     (eq (char-after pos) ?/)
                     (eq (char-after (1+ pos)) ?*))
                (setq pos (mixed-html--fontify-block-comment pos end)))
               (t (setq pos (1+ pos)))))
            ;; Fontify value portion (excluding parts already fontified)
            (mixed-html--fontify-css-value val-start pos)))
        pos)
    ;; Outside a rule block: this is a selector
    (let ((sel-start pos))
      (while (and (< pos end)
                  (not (memq (char-after pos) '(?{ ?\;))))
        ;; Handle comments in selectors
        (if (and (< (1+ pos) end)
                 (eq (char-after pos) ?/)
                 (eq (char-after (1+ pos)) ?*))
            (setq pos (mixed-html--fontify-block-comment pos end))
          (setq pos (1+ pos))))
      (let ((sel-end pos))
        (save-excursion
          (goto-char sel-end)
          (skip-chars-backward " \t\n" sel-start)
          (setq sel-end (point)))
        (when (> sel-end sel-start)
          (put-text-property sel-start sel-end 'face 'mixed-html-css-selector-face)))
      pos)))

(defun mixed-html--fontify-css-value (start end)
  "Apply value face to unfontified portions between START and END."
  (let ((pos start))
    (while (< pos end)
      (if (get-text-property pos 'face)
          ;; Skip already-fontified region
          (setq pos (next-single-property-change pos 'face nil end))
        ;; Find extent of unfontified region
        (let ((unfont-start pos))
          (setq pos (next-single-property-change pos 'face nil end))
          ;; Only apply to non-whitespace-only portions
          (let ((trimmed-start unfont-start)
                (trimmed-end pos))
            (save-excursion
              (goto-char trimmed-start)
              (skip-chars-forward " \t\n" trimmed-end)
              (setq trimmed-start (point)))
            (when (> trimmed-end trimmed-start)
              (put-text-property trimmed-start trimmed-end
                                 'face 'mixed-html-css-value-face))))))))

;;; HTML fontification

(defun mixed-html--fontify-html-region (start end)
  "Fontify HTML between START and END."
  (let ((pos start))
    (while (< pos end)
      (cond
       ;; HTML comment
       ((and (< (+ pos 3) end)
             (eq (char-after pos) ?<)
             (eq (char-after (1+ pos)) ?!)
             (eq (char-after (+ pos 2)) ?-)
             (eq (char-after (+ pos 3)) ?-))
        (let ((comment-end (save-excursion
                             (goto-char (+ pos 4))
                             (if (search-forward "-->" end t)
                                 (point)
                               end))))
          (put-text-property pos comment-end 'face 'mixed-html-comment-face)
          (setq pos comment-end)))
       ;; DOCTYPE / processing instruction
       ((and (< (1+ pos) end)
             (eq (char-after pos) ?<)
             (eq (char-after (1+ pos)) ?!))
        (let ((tag-end (save-excursion
                         (goto-char pos)
                         (if (search-forward ">" end t)
                             (point)
                           end))))
          (put-text-property pos tag-end 'face 'mixed-html-keyword-face)
          (setq pos tag-end)))
       ;; HTML tag
       ((and (< pos end) (eq (char-after pos) ?<))
        (setq pos (mixed-html--fontify-html-tag pos end)))
       ;; Entity reference
       ((and (< pos end) (eq (char-after pos) ?&))
        (let ((ent-end (save-excursion
                         (goto-char pos)
                         (if (re-search-forward ";\\|[ \t\n]" end t)
                             (if (eq (char-before) ?\;)
                                 (point)
                               pos)
                           pos))))
          (if (> ent-end pos)
              (progn
                (put-text-property pos ent-end 'face 'mixed-html-constant-face)
                (setq pos ent-end))
            (setq pos (1+ pos)))))
       (t (setq pos (1+ pos)))))))

(defun mixed-html--fontify-html-tag (pos end)
  "Fontify an HTML tag starting at POS, up to END.  Return new position."
  (let ((tag-start pos))
    (setq pos (1+ pos))  ; skip <
    ;; Skip optional /
    (when (and (< pos end) (eq (char-after pos) ?/))
      (setq pos (1+ pos)))
    ;; Tag name
    (while (and (< pos end)
                (let ((c (char-after pos)))
                  (or (and (>= c ?a) (<= c ?z))
                      (and (>= c ?A) (<= c ?Z))
                      (and (>= c ?0) (<= c ?9))
                      (eq c ?-) (eq c ?:))))
      (setq pos (1+ pos)))
    (put-text-property tag-start pos 'face 'mixed-html-tag-face)
    ;; Attributes
    (let ((done nil))
      (while (and (< pos end) (not done)
                  (not (eq (char-after pos) ?>)))
        (cond
         ;; Whitespace
         ((memq (char-after pos) '(?\s ?\t ?\n ?\r))
          (setq pos (1+ pos)))
         ;; Self-closing />
         ((and (eq (char-after pos) ?/)
               (< (1+ pos) end)
               (eq (char-after (1+ pos)) ?>))
          (put-text-property pos (+ pos 2) 'face 'mixed-html-tag-face)
          (setq pos (+ pos 2))
          (setq done t))
       ;; Attribute name
       ((let ((c (char-after pos)))
          (or (and (>= c ?a) (<= c ?z))
              (and (>= c ?A) (<= c ?Z))
              (eq c ?_) (eq c ?:) (eq c ?-)))
        (let ((attr-start pos))
          (while (and (< pos end)
                      (let ((c (char-after pos)))
                        (or (and (>= c ?a) (<= c ?z))
                            (and (>= c ?A) (<= c ?Z))
                            (and (>= c ?0) (<= c ?9))
                            (eq c ?_) (eq c ?:) (eq c ?-))))
            (setq pos (1+ pos)))
          (put-text-property attr-start pos 'face 'mixed-html-attr-face))
        ;; Skip whitespace
        (while (and (< pos end) (memq (char-after pos) '(?\s ?\t ?\n ?\r)))
          (setq pos (1+ pos)))
        ;; = and value
        (when (and (< pos end) (eq (char-after pos) ?=))
          (setq pos (1+ pos))
          ;; Skip whitespace
          (while (and (< pos end) (memq (char-after pos) '(?\s ?\t ?\n ?\r)))
            (setq pos (1+ pos)))
          ;; Attribute value
          (cond
           ;; Quoted value
           ((and (< pos end) (memq (char-after pos) '(?\" ?\')))
            (let ((q (char-after pos)))
              (setq pos (1+ pos))
              (let ((val-start pos))
                (while (and (< pos end) (not (eq (char-after pos) q)))
                  (setq pos (1+ pos)))
                (put-text-property (1- val-start) (if (and (< pos end)
                                                           (eq (char-after pos) q))
                                                      (1+ pos)
                                                    pos)
                                   'face 'mixed-html-attr-value-face)
                (when (and (< pos end) (eq (char-after pos) q))
                  (setq pos (1+ pos))))))
           ;; Unquoted value
           ((and (< pos end) (not (memq (char-after pos) '(?> ?\s ?\t ?\n ?\r))))
            (let ((val-start pos))
              (while (and (< pos end)
                          (not (memq (char-after pos) '(?> ?\s ?\t ?\n ?\r))))
                (setq pos (1+ pos)))
              (put-text-property val-start pos 'face 'mixed-html-attr-value-face))))))
       (t (setq pos (1+ pos)))))
      ;; Skip closing > (unless we already handled />)
      (when (and (not done) (< pos end) (eq (char-after pos) ?>))
        (put-text-property pos (1+ pos) 'face 'mixed-html-tag-face)
        (setq pos (1+ pos))))
    pos))

;;; Change tracking

(defvar-local mixed-html--last-regions nil
  "Cached region list from the last fontification pass.")

(defvar-local mixed-html--region-timer nil
  "Idle timer for deferred region recomputation.")

(defvar-local mixed-html--regions-dirty nil
  "Non-nil when regions need recomputation.")

;;; Main fontification function

(defun mixed-html-fontify (start end)
  "Fontify the region between START and END.
Returns the actual bounds fontified as (ACTUAL-START . ACTUAL-END),
which may be wider than requested since we must always fontify from
the beginning of each region to maintain correct parser state.
Aborts early if user input arrives, letting jit-lock retry later."
  ;; In batch mode (tests), recompute stale regions synchronously so
  ;; font-lock-ensure gets correct results.  In interactive mode, use
  ;; stale regions — the idle timer will fix them and trigger
  ;; refontification, avoiding any synchronous full-buffer scan.
  (when (and mixed-html--regions-dirty noninteractive)
    (mixed-html--recompute-regions))
  (let ((result (while-no-input
                  (let ((regions (or mixed-html--last-regions
                                    (mixed-html--find-regions)))
                        (actual-start start)
                        (actual-end end))
                    (dolist (region regions)
                      (let ((type (nth 0 region))
                            (rstart (nth 1 region))
                            (rend (nth 2 region)))
                        ;; Only fontify regions that overlap [start, end]
                        (when (and (< rstart end) (> rend start))
                          ;; Always start from the beginning of the region so
                          ;; the parser has correct state (e.g. knows whether
                          ;; we're inside a string or comment).  Clip the end
                          ;; to the requested range.
                          (let ((fstart rstart)
                                (fend (min rend end)))
                            (setq actual-start (min actual-start fstart))
                            (cond
                             ((eq type 'html) (mixed-html--fontify-html-region fstart fend))
                             ((eq type 'css) (mixed-html--fontify-css-region fstart fend))
                             ((eq type 'js) (mixed-html--fontify-js-region fstart fend)))))))
                    (cons actual-start actual-end)))))
    ;; while-no-input returns t if interrupted
    (if (and result (not (eq result t)))
        result
      ;; Interrupted -- return original bounds so jit-lock retries
      (cons start end))))

(defun mixed-html-fontify-region (start end &optional _loudly)
  "Fontify function for `font-lock-fontify-region-function'.
Fontifies from START to END."
  (let ((bounds (mixed-html-fontify start end)))
    `(jit-lock-bounds ,(car bounds) . ,(cdr bounds))))

;;; Syntax table

(defvar mixed-html-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; Treat < and > as punctuation rather than paired delimiters
    ;; to avoid confusing subsequent parsing
    (modify-syntax-entry ?< "." table)
    (modify-syntax-entry ?> "." table)
    (modify-syntax-entry ?& "." table)
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\' "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    table)
  "Syntax table for `mixed-html-mode'.")

(defun mixed-html--recompute-regions ()
  "Recompute regions on idle.  Called from a timer or before fontification."
  (when mixed-html--regions-dirty
    (setq mixed-html--regions-dirty nil)
    (let ((old-regions mixed-html--last-regions)
          (new-regions (mixed-html--find-regions)))
      (setq mixed-html--last-regions new-regions)
      (unless (equal new-regions old-regions)
        ;; Regions changed — mark whole buffer for refontification.
        ;; jit-lock will pick up visible parts on next redisplay.
        (jit-lock-refontify (point-min) (point-max))))))

(defun mixed-html--after-change (_beg _end _old-len)
  "After-change function: schedule deferred region recomputation.
Does not scan the buffer synchronously — instead sets a dirty flag
and schedules an idle timer to recompute regions."
  (setq mixed-html--regions-dirty t)
  (when mixed-html--region-timer
    (cancel-timer mixed-html--region-timer))
  (let ((buf (current-buffer)))
    (setq mixed-html--region-timer
          (run-with-idle-timer
           0.3 nil
           (lambda ()
             (when (buffer-live-p buf)
               (with-current-buffer buf
                 (mixed-html--recompute-regions))))))))

(defun mixed-html--cleanup ()
  "Cancel pending timers when buffer is killed."
  (when mixed-html--region-timer
    (cancel-timer mixed-html--region-timer)
    (setq mixed-html--region-timer nil)))

;;; Mode definition

(defvar mixed-html-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for `mixed-html-mode'.")

;;;###autoload
(define-derived-mode mixed-html-mode fundamental-mode "MixedHTML"
  "Major mode for editing HTML with inline CSS and JavaScript.

Provides syntax highlighting that correctly handles <style> and <script>
regions, applying CSS and JavaScript highlighting within those regions
and HTML highlighting elsewhere.

Region boundaries are detected the way a browser would: a literal
</script> or </style> always ends the region, even inside a string."
  :syntax-table mixed-html-mode-syntax-table
  ;; Disable default font-lock keyword processing
  (setq font-lock-defaults '(nil t))
  ;; Use jit-lock for incremental fontification
  (setq font-lock-fontify-region-function #'mixed-html-fontify-region)
  (setq font-lock-unfontify-region-function #'font-lock-default-unfontify-region)
  ;; Enable font-lock
  (font-lock-mode 1)
  ;; Use jit-lock for lazy fontification
  (jit-lock-mode t)
  (jit-lock-register #'mixed-html-fontify)
  ;; Defer fontification so it runs from a timer, not during redisplay.
  ;; This ensures while-no-input can actually interrupt it on keypress.
  (setq-local jit-lock-defer-time 0.05)
  ;; Enable stealth (background) fontification of non-visible regions
  (setq-local jit-lock-stealth-time 1)
  (setq-local jit-lock-stealth-nice 0.1)
  (setq-local jit-lock-chunk-size 1000)
  ;; Track changes that affect region boundaries
  (setq mixed-html--last-regions (mixed-html--find-regions))
  (add-hook 'after-change-functions #'mixed-html--after-change nil t)
  (add-hook 'kill-buffer-hook #'mixed-html--cleanup nil t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.html?\\'" . mixed-html-mode))

(provide 'mixed-html-mode)
;;; mixed-html-mode.el ends here
