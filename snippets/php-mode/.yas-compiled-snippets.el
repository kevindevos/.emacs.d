;;; Compiled snippets and support files for `php-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'php-mode
		     '(("setter" "public function set$1($${1:$(downcase yas/text)}){\n       $this->${1:$(downcase yas/text)} = ${1:$(downcase yas/text)};\n}" "setter" nil nil nil "/Users/kevindevos/.emacs.d/snippets/php-mode/setter" nil nil)
		       ("getter" "public function get$1(){\n       return $this->${1:$(downcase yas/text)};\n}" "getter" nil nil nil "/Users/kevindevos/.emacs.d/snippets/php-mode/getter" nil nil)
		       ("constructor" "public function __construct(){\n   $0\n}" "constructor" nil nil nil "/Users/kevindevos/.emacs.d/snippets/php-mode/constructor" nil nil)))


;;; Do not edit! File generated at Thu Sep  5 18:45:17 2019
