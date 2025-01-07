;;;; Simple Calculater
(defpackage :calculater
  (:use :cl)
  (:export
   :lexer
   :parser
   :executer))

;;; v0: 空白区切り, +-の計算を可能にする
;;; v1: 空白区切り, */の計算を可能にする
;;; v2: 空白区切り, ()の計算を可能にする
;;; v3: 空白不要, ()の計算を可能にする

;; TODO 
;;; TODO: エラーケースを追加
;;; TODO: quitを追加

(defparameter *operands* '(#\+ #\- ))

;;; Lexer : 入力文字列を解析してトークンに変換
;; ex. "1 + 1"     => '(1 '+ 1)
;; ex. "1 + 1 - 2" => '(1 '+ 1 '- 2)
;; TODO: lexerもparserのようにreduceでまとめたいがindex管理をして逐次的に実行しているせいでreduceで利用する関数にindex情報を含める必要がある。もしやりたいなら文字列に変えてindex管理を無くしてから書き直す方が良い
(defun lexer (string)
  (let ((result (list)) (len (length string)))
    (dotimes (i len)
      (let ((char (char string i)))
	(cond
	  ;; operand
	  ((member char *operands*)
	   ;;(setq result (append result (list (intern (string char)))))
	   (push (intern (string char)) result))
	  ;; number
	  ((digit-char-p char)
	   ;; 数値として読めるものを全て読み込み数字文字列を作成して数値に変換
	   (let ((num-string (string char)))
	     (loop while (and (< (+ i 1) len) (digit-char-p (char string (+ i 1))))
		   do
		      (incf i)
		      (setq num-string (concatenate 'string num-string (string (char string i)))))
	     (push (parse-integer num-string :junk-allowed nil) result))))))
    ;; pushは先頭に要素を追加するので最後はreverseして返す
    (reverse result)))

;;; Parser : 解析されたトークンをlispで計算可能なリストに変換
;; ex. '(1 '+ 1)        => '(+ 1 1)
;; ex. '(1 '+ 1 '- 2)   => '(- (+ 1 1) 2)
;; TODO: setqが多くて可読性が低い。→ reduceを使ってまとめてみる
;; TODO: やりたいことはリストの各要素を処理して新しいリストを作りたい。ということなのでreduceが最適
(defun parser (tokens)
  (reduce (lambda (form token)
	      (cond
		((numberp token)
		 (if form
		     (append form (list token))		    
		     token))
		((member (%operand-symbol->cahr token) *operands*)
		 (list token form))))
	  tokens
	  :initial-value nil))

(defun %operand-symbol->cahr (symbol)
  (char (string symbol) 0))

;;; Executer : 計算可能なリストを実行する
;; '(+ 1 1)        => 2
;; '(- (+ 1 1) 2)  => 0
(defun executer (form)
  (eval form))

;;; Interface : ユーザーからの入力と出力を定義する
(defun read-input ()
  (format t "Enter a formula : ")
  (let ((line (read-line)))
    line))

(defun output (result)
  (format t ">> ~a~%" result))

(defun main ()
  (format t "Calculater started:)~%")
  (loop
    (let* ((input (read-input))
	   (tokens (lexer input))
	   (form (parser tokens))
	   (result (executer form)))

      (when (string= input "quit")
	(format t "Bye:)~%")
	(return))
      (output result))))
