;;;; Simple Calculater


;;; v0: 空白区切り, +-の計算を可能にする
;;; v1: 空白区切り, */の計算を可能にする
;;; v2: 空白区切り, ()の計算を可能にする
;;; v3: 空白不要, ()の計算を可能にする

;; TODO s
;;; TODO: テストを追加. ユニットテスト（個々の関数のテストを行うため）統合テスト（複数のモジュールが関連するシナリオのテスト）
;;; TODO: エラーケースを追加
;;; TODO: quitを追加



(defparameter *operands* '(#\+ #\- ))

;;; Lexer : 入力文字列を解析してトークンに変換
;; ex. "1 + 1"     => '(1 '+ 1)
;; ex. "1 + 1 - 2" => '(1 '+ 1 '- 2)
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
(defun parser (tokens)
  (let ((form '()))
    (loop for token in tokens
	  do
	     (cond
	       ((numberp token)
		(if form
		    (setq form (append form (list token)))		    
		    (setq form token)))
	       ((member (%operand-symbol->cahr token) *operands*)
		(setq form (list token form)))))
    form))

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

;;; TESTS
;;; TODO: あとでテス用の関数を作る
(equal (lexer "1+1") '(1 + 1))
(equal (lexer "1 + 1") '(1 + 1))
(equal (lexer "100+111") '(100 + 111))
(equal (lexer "100 + 111") '(100 + 111))

(equal (parser '(1 + 1)) '(+ 1 1))
(equal (parser '(1 + 1 + 1)) '(+ (+ 1 1) 1))
(equal (parser '(1 + 1 - 1)) '(- (+ 1 1) 1))

(eql (executer '(+ 1 1)) 2)
(eql (executer '(+ (+ 1 1) 1)) 3)
(eql (executer '(- (+ 1 1) 1)) 1)


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

(main)
