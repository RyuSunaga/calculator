d;;;; Simple Calculater

;;;
;;; v0: 空白区切り, +-の計算を可能にする
;;; v1: 空白区切り, */の計算を可能にする
;;; v2: 空白区切り, ()の計算を可能にする
;;; v3: 空白不要, ()の計算を可能にする


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


;;; Executer : 計算可能なリストを実行する
;; '(+ 1 1)        => 2
;; '(- (+ 1 1) 2)  => 0



;;; Interface : ユーザーからの入力と出力を定義する

;;; TESTS
;;; TODO: あとでテス用の関数を作る
(equal (lexer "1+1") '(1 + 1))
(equal (lexer "1 + 1") '(1 + 1))
(equal (lexer "100+111") '(100 + 111))
(equal (lexer "100 + 111") '(100 + 111))

(defun test-lexer (pair)
  (destructuring-bind (input output) pair
    (format t "RESULT:~a Input: ~a Output: ~a" (lexer input) input output)))

(("1+1" '(1 + 1))
 ("1 + 1" '(1 + 1))
 ("100+111" '(100 + 111))
 ("100 + 111" '(100 + 111)))



;; 以下のようにしたい
;; ((input, output))





(defun main ()
  (format t "Calculater~%")
  )

(main)




