; The directory where the type debugger binary exists.
; "" works if the default ocaml is the type debugger.
(defconst ochadai-debugger-path "")

; Warning option that you want to use
(defconst ochadai-warn-error "") ; or, e.g., "-warn-error A"

; OCaml language level.
; 4: OCaml, 3: w/o ==, !=, 2: w/o side effects, 1: w/o 1st class functions
(defconst ochadai-language-level "OCAML_LANGUAGE_LEVEL=4")

; ログをとる場合は t, そうでなければ nil。
(defconst ochadai-log-interaction nil)

; ログをとる場合、ログの置き場所
(defconst ochadai-log-path "/usr/local/debugger/log/")

; シェルスクリプト copy-source の置き場所
(defconst ochadai-copy-source "/usr/local/debugger/bin/copy-source")

; お茶大版 tuareg mode
; Evaluate Buffer で毎回、ファイルを保存し、新たに読み込むようにする。
; Compile Program, Highlight Error を使えるようにする。

; Highlight specified area of the current buffer ---------------------

(defun ochadai-highlight-buffer (bgn end)
  "現在のバッファにオーバーレイをかける"
  (interactive)
  (let ((face 'highlight)
	(ol (make-overlay bgn end)))
    (overlay-put ol 'face face)
    ol))

; Obtain error location ----------------------------------------------

(defun ochadai-next-filename ()
  "カーソル位置以降にある最初のファイル名を切り出して返す"
  (interactive)
  (progn
    (re-search-forward "\"[^\"]*\"" nil t 1)
    (substring (buffer-string)
	       (match-beginning 0)
	       (- (match-end 0) 2))))

(defun ochadai-next-number ()
  "カーソル位置以降にある最初の数字を切り出して（整数として）返す"
  (interactive)
  (progn
    (re-search-forward "[0-9]+" nil t 1)
    (string-to-int (substring (buffer-string)
			      (- (match-beginning 0) 1)
			      (- (match-end 0) 1)))))

(defconst ochadai-previous-error-location-ol nil)

(defun ochadai-obtain-error-location ()
  "エラーの位置を (ファイル名, 行番号, 開始位置, 終了位置) の形で返す"
  ; 下のような行のどこかにいることを仮定している。実行後は行の最後に移動する。
  ; File "exp.ml", line 17, characters 4-22:
  (interactive)
  (progn
    (beginning-of-line)
    (let ((filename (ochadai-next-filename)))
      (re-search-forward "line" nil t 1)
      (let* ((start (point))
	     (line-number (ochadai-next-number))
	     (char-begin (ochadai-next-number))
	     (char-end (ochadai-next-number)))
	(end-of-line)
	(if ochadai-previous-error-location-ol
	  (delete-overlay ochadai-previous-error-location-ol))
	(setq ochadai-previous-error-location-ol
	      (ochadai-highlight-buffer (- start 4) (- (point) 1)))
	(list filename line-number char-begin char-end)))))

; Adjust number for multibyte ----------------------------------------

(defun ochadai-is-multibyte (p)
  "引数の位置の文字がマルチバイト文字かを判定する"
  (interactive)
  (let* ((str1 (substring (buffer-string) (- p 1) p))
	 (str2 (string-as-unibyte str1)))
    (not (= (length str1) (length str2)))))

(defun ochadai-adjust-number-for-multibyte (n)
  "列番号をマルチバイト対応の列番号にする"
  (interactive)
  (progn
    (beginning-of-line)
    (setq acc 0)
    (while (> n 0)
      (setq n (- n (if (ochadai-is-multibyte (point))
		     3 ; 2:EUC, 3:UTF-8
		     1)))
      (setq acc (+ acc 1))
      (forward-char 1))
    acc))

; Highlight error location -------------------------------------------

(defconst ochadai-previous-highlight-ol nil)

(defun ochadai-highlight-error-location-in-buffer
  					(line-number char-begin char-end)
  "現在のバッファのエラーの箇所をハイライトする"
  (interactive)
  (progn
    (goto-line line-number)
    (let ((cur (point))
	  (char-begin2 (ochadai-adjust-number-for-multibyte char-begin))
	  (char-end2 (ochadai-adjust-number-for-multibyte char-end)))
;     (if ochadai-previous-highlight-ol
;	(delete-overlay ochadai-previous-highlight-ol))
      (setq ochadai-previous-highlight-ol
	    (ochadai-highlight-buffer (+ cur char-begin2) (+ cur char-end2)))
;     (goto-line line-number)
;     (recenter) ; エラー箇所を画面の中心に配置
      )))

(defun ochadai-highlight-error-location (error-location)
  "ファイルを必要に応じて読み込んで、エラーの箇所をハイライトする"
  ; 実行後、current-buffer が filename になる。
  (interactive)
  (let ((filename (car error-location))
	(line-number (car (cdr error-location)))
	(char-begin (car (cdr (cdr error-location))))
	(char-end (car (cdr (cdr (cdr error-location))))))
    (if (string-equal (buffer-name) tuareg-interactive-buffer-name)
      (switch-to-buffer-other-window (find-file-noselect filename))
      (if (not (string-equal (buffer-name) filename))
	(switch-to-buffer (find-file-noselect filename))))
    (ochadai-highlight-error-location-in-buffer
      line-number char-begin char-end)))

; Highlight Error ----------------------------------------------------

(defun ochadai-search-error-line ()
  "（Error-enabled warnings 以外の）エラーの行に移動する。"
  (interactive)
  (progn
    (goto-char (point-max))
    (let ((result (re-search-backward "Error-enabled warnings" nil t 1)))
      (if result
	(re-search-backward "File \"[^\"]+\", line [0-9]+, characters "
			    nil t 2)
	(progn
	  (goto-char (point-max))
	  (re-search-backward "File \"[^\"]+\", line [0-9]+, characters "
			      nil t 1))))))

(defun ochadai-highlight-error ()
  "エラーの箇所をハイライトする"
  (interactive)
  (let ((lst (save-current-buffer
	       (set-buffer tuareg-interactive-buffer-name)
	       (goto-char (point-max))
	       (let ((result (ochadai-search-error-line)))
		 (if result
		   (ochadai-obtain-error-location) ; エラー情報を取得
		   result)))))
    (if ochadai-previous-highlight-ol ; 前のハイライトを削除
      (delete-overlay ochadai-previous-highlight-ol))
    (if (null lst)
      ()
      (ochadai-highlight-error-location lst))))

; Buffer functions ---------------------------------------------------

(defun ochadai-kill-buffer (name)
  "name というバッファがあれば削除する。中のプロセスも削除する。"
  (interactive)
  (if (get-buffer name)
    (progn
      (if (get-buffer-process name)
	(process-kill-without-query (get-buffer-process name) nil))
      (kill-buffer name))))

(defun ochadai-get-buffer-create (name)
  "name という空のバッファを（存在していたら破棄した上で）新しく作る"
  (interactive)
  (progn
    (ochadai-kill-buffer name)
    (get-buffer-create name)))

; ログ

(defun ochadai-save-file (current-buf log-dir)
  "current-buf とそこから #use されているファイルを保存"
  (interactive)
  (progn
    (set-buffer current-buf)
    (let ((p (point)))
      (shell-command-to-string (concat "cp " buffer-file-name " " log-dir))
      (goto-char (point-min))
      (while (re-search-forward "#use +\"\\([^\"]+\\)\"" nil t 1)
	; バッファ中で #use しているファイルを全てコピーする
	(shell-command-to-string (concat "cp " (match-string 1) " " log-dir)))
      (goto-char p)))) ; カーソルをもとの場所に戻す

(defun ochadai-take-log (&optional current-buf)
  "ログ用のディレクトリを作成し、ソースをコピーし、script 用のファイル名を返す"
  (interactive)
  (let ((log-root-dir (concat ochadai-log-path (system-name))))
    (if (not (file-directory-p log-root-dir))
        (make-directory log-root-dir))
    (shell-command-to-string (concat "chmod 777 " log-root-dir))
    (let* ((dir-name (format-time-string "%m_%d_%H_%M_%S" (current-time)))
	   (log-dir (concat log-root-dir "/" dir-name "/")))
      (make-directory log-dir) ; ログディレクトリを作成
      (shell-command-to-string (concat "chmod 777 " log-dir))
      (if current-buf ; current-buf が渡されている (evaluate buffer) なら
	(ochadai-save-file current-buf log-dir) ; それをコピー
        ; プログラム（Makefile を含む）をログディレクトリにコピー
        (shell-command-to-string (concat ochadai-copy-source " . " log-dir)))
      (shell-command-to-string (concat "chmod 666 " log-dir "*"))
      (concat log-dir "buffer.txt") ; ログファイル名
      )))

; Evaluate Buffer ----------------------------------------------------

(defun ochadai-eval-buffer ()
  "現在のバッファを保存してから tuareg-eval-buffer で OCaml に読み込む"
  ; その際、tuareg-interactive-buffer-name を kill して、新たに始める
  (interactive)
  (let ((filename buffer-file-name)
	(current-buf (current-buffer)))
    (save-buffer)
    (save-current-buffer
      (ochadai-kill-buffer tuareg-interactive-buffer-name)
      (let* ((tuareg-interactive-program
	       (concat "env " ochadai-language-level " "
		       tuareg-interactive-program))
	     (tuareg-interactive-program ; dynamic binding!!!
	       (if ochadai-log-interaction
		 (let* ((log-file (ochadai-take-log current-buf)))
		   (shell-command-to-string (concat "touch " log-file))
		   (shell-command-to-string (concat "chmod 666 " log-file))
		   (concat "script -aq -t 0 " log-file " "
			   tuareg-interactive-program))
		 tuareg-interactive-program)))
	(with-temp-buffer
	  (insert (concat "#use \"" filename "\";;\n"))
	  (tuareg-eval-buffer))
	(sleep-for 1)))
    (sleep-for 1) ; なぜ、ひとつだと待ってくれない？
    (sleep-for 1)
    (sleep-for 1)
    (ochadai-highlight-error)))

; Compile Program ----------------------------------------------------

(defun ochadai-compile-program ()
  "現在のディレクトリで make を行う"
  (interactive)
  (if (file-exists-p "Makefile")
    (progn
      (save-buffer) ; 現在のバッファを保存する
      (save-current-buffer
	(ochadai-kill-buffer tuareg-interactive-buffer-name)
	(let ((buf (if ochadai-log-interaction
		     (let* ((log-file (ochadai-take-log)))
		       (shell-command-to-string (concat "touch " log-file))
		       (shell-command-to-string (concat "chmod 666 " log-file))
		       (make-comint "ocaml-toplevel" "script" nil "-akq" "-t 0"
				    log-file "make"
			 ochadai-language-level
			 (concat "OCAMLC="
				 (replace-regexp-in-string
				   "ocaml\\\( \\\|$\\\)" "ocamlc\\1"
				   tuareg-interactive-program))))
		     (make-comint "ocaml-toplevel" "make" nil
		       ochadai-language-level
		       (concat "OCAMLC="
			       (replace-regexp-in-string
				 "ocaml\\\( \\\|$\\\)" "ocamlc\\1"
				 tuareg-interactive-program))))))
	    (display-buffer buf)))
      (sleep-for 1) ; なぜ、ひとつだと待ってくれない？
      (sleep-for 1)
      (sleep-for 1)
      (ochadai-highlight-error))
    ; Makefile が存在しなかった
    (message "プログラムをコンパイルするには Makefile が必要です。")))

; Send answers -------------------------------------------------------

(defun ochadai-send (ans)
  "ans と答える"
  (interactive)
  (save-current-buffer
    (set-buffer tuareg-interactive-buffer-name)
    (goto-char (point-max))
    (setq tuareg-interactive-last-phrase-pos-in-toplevel (point))
    (comint-send-string tuareg-interactive-buffer-name ans)
;    (let ((pos (point)))
;      (comint-send-input)
;      (save-excursion
;	(goto-char pos)
;	(insert ans)))
    )
  (sleep-for 0.3)
  (sleep-for 0.3)
  (ochadai-highlight-error))

(defun ochadai-send-yes ()
  "yes と答える"
  (interactive)
  (ochadai-send "y;;
"))

(defun ochadai-send-no ()
  "no と答える"
  (interactive)
  (ochadai-send "n;;
"))

(defun ochadai-send-quit ()
  "quit と答える"
  (interactive)
  (ochadai-send "q;;
"))

; Install Ochadai Version --------------------------------------------

(defconst ochadai-tuareg-interactive-program-modified nil)
(defconst ochadai-encoding 'utf-8) ; or euc-jp-unix

(add-hook 'tuareg-mode-hook
  (lambda ()
    ; set character set
    (set-default-coding-systems ochadai-encoding)
    (set-buffer-file-coding-system ochadai-encoding)
    (set-terminal-coding-system ochadai-encoding)
    (set-keyboard-coding-system ochadai-encoding)
    (setq default-buffer-file-coding-system ochadai-encoding)
    ; input sent to the OCaml toplevel is read-only.
    (setq tuareg-interactive-read-only-input t)
    ; ocaml command to run.  Warning is not permitted.
    (setq tuareg-interactive-program
	  (if ochadai-tuareg-interactive-program-modified
	    tuareg-interactive-program
	    (progn
	      (setq ochadai-tuareg-interactive-program-modified t)
	      (concat ochadai-debugger-path
		      tuareg-interactive-program
		      (if (string-equal ochadai-warn-error "")
			""
		        (concat " " ochadai-warn-error))))))
    ; install Evaluate Buffer and Highlight Error
    (define-key tuareg-mode-map "\C-c\C-o" 'tuareg-eval-buffer)
    (define-key tuareg-mode-map "\C-c\C-b" 'ochadai-eval-buffer)
    (define-key tuareg-mode-map "\C-c\C-m" 'ochadai-compile-program)
    (define-key tuareg-mode-map "\C-c\C-h" 'ochadai-highlight-error)
    (define-key tuareg-mode-map "\C-c\C-y" 'ochadai-send-yes)
    (define-key tuareg-mode-map "\C-c\C-n" 'ochadai-send-no)
    (define-key tuareg-mode-map "\C-c\C-q" 'ochadai-send-quit)
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Evaluate Buffer" ochadai-eval-buffer t])
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Compile Program" ochadai-compile-program t])
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Highlight Error" ochadai-highlight-error t])
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Answer Yes" ochadai-send-yes t])
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Answer No" ochadai-send-no t])
    (easy-menu-add-item nil
      '("Tuareg" "Interactive Mode")
      ["Answer Quit" ochadai-send-quit t])
    ))
