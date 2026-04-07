;;; mixed-html-mode-tests.el --- Tests for mixed-html-mode  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --batch -l mixed-html-mode.el -l mixed-html-mode-tests.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

(defun test-face-at (text pos)
  "Fontify TEXT in mixed-html-mode and return the face at POS."
  (with-temp-buffer
    (insert text)
    (mixed-html-mode)
    (font-lock-ensure)
    (get-text-property pos 'face)))

(defun test-face-at-search (text search-string &optional occurrence)
  "Fontify TEXT, search for SEARCH-STRING, return face at start of match.
OCCURRENCE defaults to 1 (first match)."
  (with-temp-buffer
    (insert text)
    (mixed-html-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (dotimes (_ (or occurrence 1))
      (search-forward search-string))
    (get-text-property (match-beginning 0) 'face)))

(defun test-face-at-file-search (file search-string &optional occurrence)
  "Load FILE, fontify it, search for SEARCH-STRING, return face at start of match."
  (with-temp-buffer
    (insert-file-contents file)
    (mixed-html-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (dotimes (_ (or occurrence 1))
      (search-forward search-string))
    (get-text-property (match-beginning 0) 'face)))

;;; Region detection tests

(ert-deftest mhm-test-regions-simple ()
  "Basic region detection with one style and one script block."
  (with-temp-buffer
    (insert "<html><style>body{}</style><script>var x;</script></html>")
    (let ((regions (mixed-html--find-regions)))
      ;; Should have html, html(style tag), css, html(close style), html, html(script tag), js, html(close script), html
      (should (cl-some (lambda (r) (eq (nth 0 r) 'css)) regions))
      (should (cl-some (lambda (r) (eq (nth 0 r) 'js)) regions)))))

(ert-deftest mhm-test-regions-multiple-style ()
  "Multiple style blocks should each get their own CSS region."
  (with-temp-buffer
    (insert "<style>a{}</style><style>b{}</style>")
    (let* ((regions (mixed-html--find-regions))
           (css-regions (cl-remove-if-not (lambda (r) (eq (nth 0 r) 'css)) regions)))
      (should (= (length css-regions) 2)))))

(ert-deftest mhm-test-regions-script-with-attrs ()
  "Script tag with attributes should still be detected."
  (with-temp-buffer
    (insert "<script async src=\"foo.js\"></script><script>var x;</script>")
    (let* ((regions (mixed-html--find-regions))
           (js-regions (cl-remove-if-not (lambda (r) (eq (nth 0 r) 'js)) regions)))
      (should (= (length js-regions) 2)))))

;;; HTML fontification tests

(ert-deftest mhm-test-html-tag-name ()
  "HTML tag names should get tag face."
  (should (eq (test-face-at-search "<body>" "body")
              'mixed-html-tag-face)))

(ert-deftest mhm-test-html-closing-tag ()
  "Closing tag names should get tag face."
  (should (eq (test-face-at-search "</div>" "div")
              'mixed-html-tag-face)))

(ert-deftest mhm-test-html-attribute-name ()
  "Attribute names should get attr face."
  (should (eq (test-face-at-search "<div class=\"foo\">" "class")
              'mixed-html-attr-face)))

(ert-deftest mhm-test-html-attribute-value ()
  "Quoted attribute values should get attr-value face."
  (should (eq (test-face-at-search "<div class=\"foo\">" "\"foo\"")
              'mixed-html-attr-value-face)))

(ert-deftest mhm-test-html-comment ()
  "HTML comments should get comment face."
  (should (eq (test-face-at-search "<!-- comment -->" "comment")
              'mixed-html-comment-face)))

(ert-deftest mhm-test-html-doctype ()
  "DOCTYPE should get keyword face."
  (should (eq (test-face-at-search "<!DOCTYPE html><body>" "DOCTYPE")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-html-self-closing ()
  "Self-closing tags should be handled without breaking subsequent highlighting."
  (should (eq (test-face-at-search "<meta name=\"viewport\" /><div class=\"x\">" "class")
              'mixed-html-attr-face)))

(ert-deftest mhm-test-html-entity ()
  "Entity references should get constant face."
  (should (eq (test-face-at-search "<p>&amp; stuff</p>" "&amp;")
              'mixed-html-constant-face)))

;;; CSS fontification tests

(ert-deftest mhm-test-css-selector ()
  "CSS selectors should get selector face."
  (should (eq (test-face-at-search "<style>.warning { color: red; }</style>" ".warning")
              'mixed-html-css-selector-face)))

(ert-deftest mhm-test-css-property ()
  "CSS property names should get property face."
  (should (eq (test-face-at-search "<style>.warning { color: red; }</style>" "color")
              'mixed-html-css-property-face)))

(ert-deftest mhm-test-css-value ()
  "CSS values should get value face."
  (should (eq (test-face-at-search "<style>body { color: red; }</style>" "red")
              'mixed-html-css-value-face)))

(ert-deftest mhm-test-css-at-rule ()
  "CSS at-rules should get keyword face."
  (should (eq (test-face-at-search "<style>@media screen { body {} }</style>" "@media")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-css-comment ()
  "CSS comments should get comment face."
  (should (eq (test-face-at-search "<style>/* a comment */</style>" "a comment")
              'mixed-html-comment-face)))

(ert-deftest mhm-test-css-string ()
  "CSS string values should get string face."
  (should (eq (test-face-at-search "<style>body { background: url(\"foo.png\"); }</style>" "\"foo.png\"")
              'mixed-html-string-face)))

(ert-deftest mhm-test-css-id-selector ()
  "CSS ID selectors should get selector face."
  (should (eq (test-face-at-search "<style>#main { display: block; }</style>" "#main")
              'mixed-html-css-selector-face)))

;;; JavaScript fontification tests

(ert-deftest mhm-test-js-keyword-function ()
  "JS 'function' keyword should get keyword face."
  (should (eq (test-face-at-search "<script>function foo() {}</script>" "function")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-keyword-var ()
  "JS 'var' keyword should get keyword face."
  (should (eq (test-face-at-search "<script>var x = 1;</script>" "var")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-keyword-const ()
  "JS 'const' keyword should get keyword face."
  (should (eq (test-face-at-search "<script>const x = 1;</script>" "const")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-keyword-if ()
  "JS 'if' keyword should get keyword face."
  (should (eq (test-face-at-search "<script>if (true) {}</script>" "if")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-keyword-return ()
  "JS 'return' keyword should get keyword face."
  (should (eq (test-face-at-search "<script>function f() { return 1; }</script>" "return")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-constant-true ()
  "JS 'true' should get constant face."
  (should (eq (test-face-at-search "<script>var x = true;</script>" "true")
              'mixed-html-constant-face)))

(ert-deftest mhm-test-js-constant-null ()
  "JS 'null' should get constant face."
  (should (eq (test-face-at-search "<script>var x = null;</script>" "null")
              'mixed-html-constant-face)))

(ert-deftest mhm-test-js-constant-this ()
  "JS 'this' should get constant face."
  (should (eq (test-face-at-search "<script>this.foo();</script>" "this")
              'mixed-html-constant-face)))

(ert-deftest mhm-test-js-builtin-document ()
  "JS 'document' should get builtin face."
  (should (eq (test-face-at-search "<script>document.getElementById('x');</script>" "document")
              'mixed-html-builtin-face)))

(ert-deftest mhm-test-js-builtin-console ()
  "JS 'console' should get builtin face."
  (should (eq (test-face-at-search "<script>console.log('hi');</script>" "console")
              'mixed-html-builtin-face)))

(ert-deftest mhm-test-js-double-string ()
  "JS double-quoted strings should get string face."
  (should (eq (test-face-at-search "<script>var x = \"hello\";</script>" "hello")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-single-string ()
  "JS single-quoted strings should get string face."
  (should (eq (test-face-at-search "<script>var x = 'hello';</script>" "hello")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-string-with-escape ()
  "JS strings with escapes should stay as string face."
  (should (eq (test-face-at-search "<script>var x = \"he\\\"llo\";</script>" "llo")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-template-literal ()
  "JS template literals should get string face."
  (should (eq (test-face-at-search "<script>var x = `hello`;</script>" "hello")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-template-with-expr ()
  "JS template literal with ${} — the text part should be string."
  (should (eq (test-face-at-search "<script>var x = `hello ${name}`;</script>" "hello")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-template-expr-content ()
  "JS template literal ${} — a keyword inside should get keyword face."
  (should (eq (test-face-at-search
               "<script>`${typeof x}`;</script>" "typeof")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-line-comment ()
  "JS line comments should get comment face."
  (should (eq (test-face-at-search "<script>// a comment\nvar x;</script>" "a comment")
              'mixed-html-comment-face)))

(ert-deftest mhm-test-js-line-comment-does-not-leak ()
  "Code after a JS line comment should not be comment."
  (should (eq (test-face-at-search "<script>// a comment\nvar x;</script>" "var")
              'mixed-html-keyword-face)))

(ert-deftest mhm-test-js-block-comment ()
  "JS block comments should get comment face."
  (should (eq (test-face-at-search "<script>/* block comment */var x;</script>" "block comment")
              'mixed-html-comment-face)))

(ert-deftest mhm-test-js-number ()
  "JS numbers should get constant face."
  (should (eq (test-face-at-search "<script>var x = 42;</script>" "42")
              'mixed-html-constant-face)))

;;; Regex literal tests

(ert-deftest mhm-test-js-regex-basic ()
  "A basic regex literal should get string face."
  (should (eq (test-face-at-search "<script>var re = /foo/g;</script>" "/foo/")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-regex-with-single-quote ()
  "A regex containing a single quote should not break string parsing."
  (let ((text "<script>
x.replace(/href='/g, \"target=_blank\");
var y = 1;
</script>"))
    ;; The single quote inside the regex should be part of the regex, not open a string
    (should (eq (test-face-at-search text "/href='/")
                'mixed-html-string-face))
    ;; The double-quoted string after should be a string
    (should (eq (test-face-at-search text "target=_blank")
                'mixed-html-string-face))
    ;; var after should be a keyword, not swallowed by a broken string
    (should (eq (test-face-at-search text "var")
                'mixed-html-keyword-face))))

(ert-deftest mhm-test-js-regex-after-paren ()
  "Regex after ( should be recognized."
  (should (eq (test-face-at-search "<script>if (/test/.test(x)) {}</script>" "/test/")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-regex-after-equals ()
  "Regex after = should be recognized."
  (should (eq (test-face-at-search "<script>var re = /pattern/;</script>" "/pattern/")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-regex-after-comma ()
  "Regex after , should be recognized."
  (should (eq (test-face-at-search "<script>foo(a, /re/);</script>" "/re/")
              'mixed-html-string-face)))

(ert-deftest mhm-test-js-division-not-regex ()
  "Division operator should not be treated as regex."
  ;; After a number, / is division
  (should (not (eq (test-face-at-search "<script>var x = 10 / 2;</script>" "2")
                   'mixed-html-string-face))))

;;; Cross-region tests

(ert-deftest mhm-test-style-then-script ()
  "Style block followed by script block should each get correct highlighting."
  (let ((text "<style>.x { color: red; }</style><script>var y = 1;</script>"))
    (should (eq (test-face-at-search text "color")
                'mixed-html-css-property-face))
    (should (eq (test-face-at-search text "var")
                'mixed-html-keyword-face))))

(ert-deftest mhm-test-html-between-scripts ()
  "HTML between two script blocks should get HTML highlighting."
  (let ((text "<script>var a;</script><div class=\"x\"></div><script>var b;</script>"))
    (should (eq (test-face-at-search text "class")
                'mixed-html-attr-face))))

(ert-deftest mhm-test-multiple-style-blocks ()
  "Multiple style blocks should each be highlighted correctly."
  (let ((text "<style>.a { color: red; }</style><style>.b { color: blue; }</style>"))
    (should (eq (test-face-at-search text ".a")
                'mixed-html-css-selector-face))
    (should (eq (test-face-at-search text ".b")
                'mixed-html-css-selector-face))))

;;; Example file tests

(ert-deftest mhm-test-file-whistle-synth ()
  "whistle-synth.html should fontify without error and have correct faces."
  (should (eq (test-face-at-file-search "examples/whistle-synth.html" "border")
              'mixed-html-css-property-face))
  (should (eq (test-face-at-file-search "examples/whistle-synth.html" "function" 2)
              'mixed-html-keyword-face))
  ;; Line comment
  (should (eq (test-face-at-file-search "examples/whistle-synth.html" "Wire up")
              'mixed-html-comment-face)))

(ert-deftest mhm-test-file-metahomoviria ()
  "metahomoviria.html: template literals and const keyword."
  (should (eq (test-face-at-file-search "examples/metahomoviria.html" "const" 1)
              'mixed-html-keyword-face))
  ;; Template literal content
  (should (eq (test-face-at-file-search "examples/metahomoviria.html" "Error status code")
              'mixed-html-string-face))
  ;; CSS selector
  (should (eq (test-face-at-file-search "examples/metahomoviria.html" ".warning")
              'mixed-html-css-selector-face)))

(ert-deftest mhm-test-file-bucket-brigade ()
  "bucket-brigade.html: regex with quote should not break highlighting."
  (with-temp-buffer
    (insert-file-contents "examples/bucket-brigade.html")
    (mixed-html-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    ;; Find the regex literal /href='/g in the JS section
    (search-forward "html.replace(/href='/g")
    ;; The double-quoted string argument after the regex
    (search-forward "\"target=_blank href='recordings/\"")
    (should (eq (get-text-property (1+ (match-beginning 0)) 'face)
                'mixed-html-string-face))))

(ert-deftest mhm-test-file-try-contra ()
  "try-contra.html should fontify without error."
  (with-temp-buffer
    (insert-file-contents "examples/try-contra.html")
    (mixed-html-mode)
    (font-lock-ensure)
    ;; Just verify no error and basic faces work
    (goto-char (point-min))
    (search-forward "function")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'mixed-html-keyword-face))))

(ert-deftest mhm-test-file-try-contra-events ()
  "try-contra-events.html should fontify without error."
  (with-temp-buffer
    (insert-file-contents "examples/try-contra-events.html")
    (mixed-html-mode)
    (font-lock-ensure)
    (goto-char (point-min))
    (search-forward "@media")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'mixed-html-keyword-face))))

;;; Partial-range fontification tests (simulating jit-lock chunks)

(ert-deftest mhm-test-partial-range-mid-string ()
  "Fontifying a partial range starting inside a JS string should not
cause the rest of the buffer to be treated as string."
  (let ((text "<script>
var x = \"hello\";
var y = \"world\";
var z = 42;
</script>"))
    (with-temp-buffer
      (insert text)
      (mixed-html-mode)
      ;; Fontify just a range starting inside the first string
      (let ((mid (+ (point-min) 20)))  ; somewhere in "hello"
        (mixed-html-fontify mid (point-max)))
      ;; var z should be a keyword, not swallowed by a string
      (goto-char (point-min))
      (search-forward "var z")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'mixed-html-keyword-face)))))

(ert-deftest mhm-test-partial-range-mid-comment ()
  "Fontifying a partial range starting inside a JS comment should not
cause the rest of the buffer to lose highlighting."
  (let ((text "<script>
/* a long comment */
var x = 1;
</script>"))
    (with-temp-buffer
      (insert text)
      (mixed-html-mode)
      ;; Fontify starting inside the comment
      (let ((mid (+ (point-min) 15)))
        (mixed-html-fontify mid (point-max)))
      ;; var should still be a keyword
      (goto-char (point-min))
      (search-forward "var")
      (should (eq (get-text-property (match-beginning 0) 'face)
                  'mixed-html-keyword-face)))))

;;; Region change tests (simulating edits that change region type)

(ert-deftest mhm-test-change-style-to-script ()
  "Changing <style> to <script> should re-highlight content as JS."
  (with-temp-buffer
    (insert "<style>\nvar x = 1;\n</style>")
    (mixed-html-mode)
    (font-lock-ensure)
    ;; Initially 'var' is inside CSS, not a keyword
    (goto-char (point-min))
    (search-forward "var")
    (should-not (eq (get-text-property (match-beginning 0) 'face)
                    'mixed-html-keyword-face))
    ;; Change <style> to <script> and </style> to </script>
    (goto-char (point-min))
    (search-forward "style")
    (replace-match "script")
    (goto-char (point-min))
    (search-forward "style")
    (replace-match "script")
    (font-lock-ensure)
    ;; Now 'var' should be a JS keyword
    (goto-char (point-min))
    (search-forward "var")
    (should (eq (get-text-property (match-beginning 0) 'face)
                'mixed-html-keyword-face))))

;;; mixed-html-mode-tests.el ends here
