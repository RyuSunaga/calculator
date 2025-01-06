;;;; Simple Calculater

;;;
;;; v0: 空白区切り, +-の計算を可能にする
;;; v1: 空白区切り, */の計算を可能にする
;;; v2: 空白区切り, ()の計算を可能にする
;;; v3: 空白不要, ()の計算を可能にする


;;; Lexer : 入力文字列を解析してトークンに変換
;; ex. "1 + 1"     => '(1 '+ 1)
;; ex. "1 + 1 - 2" => '(1 '+ 1 '- 2)
(defun lexer (string)
  (let ((result (list)) (len (length string)))
    (dotimes (i len)
      (let ((char (char string i)))
	(if (digit-char-p char)
	    (progn
	      (let ((num-string (string char)))
		(loop while (and (< (+ i 1) len) (digit-char-p (char string (+ i 1))))
		      do
			 (incf i)
			 (setq num-string (concatenate 'string num-string (string (char string i)))))
		(setq result (append result (list (parse-integer num-string :junk-allowed nil))))))
	    (ecase char ;; 数値文字以外の場合は空白文字またはオペランドのはず
	      (#\  (continue)) ;; 空白文字の場合は何もしない
	      (#\+ (setq result (append result (list '+)))) ;; TODO: 数値以外のオペレーターはまとめて扱えるようにしたい
	      (#\- (setq result (append result (list '-))))))))
    result))

(equal (lexer "1+1") '(1 + 1))


;;; Parser : 解析されたトークンをlispで計算可能なリストに変換
;; ex. '(1 '+ 1)        => '(+ 1 1)
;; ex. '(1 '+ 1 '- 2)   => '(- (+ 1 1) 2)


;;; Executer : 計算可能なリストを実行する
;; '(+ 1 1)        => 2
;; '(- (+ 1 1) 2)  => 0



;;; Interface : ユーザーからの入力と出力を定義する


(defun main ()
  (format t "Calculater~%")
  )

(main)
