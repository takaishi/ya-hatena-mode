(require 'ya-hatena-api)
(require 'xml)
(require 'sxml)

;; はてなAPIを使用するための設定項目
(defvar yhtn:username "")
(defvar yhtn:passwd "")
(defvar *yhtn:account-info-file* "~/.yhtn:account-info.el")

(load *yhtn:account-info-file*)

;; マイナモード用の設定
(defvar ya-hatena-mode nil)
(defvar ya-hatena-mode-map nil)

(if (not (assq 'ya-hatena-mode minor-mode-alist))
    (setq minor-mode-alist
          (cons '(ya-hatena-mode "Yet Another Hatena-mode")
                minor-mode-alist)))

(defun ya-hatena-mode (&optional arg)
  "Ya-hatena-mode"
  (interactive)
  (cond
   ((< (prefix-numeric-value arg) 0)
    (setq ya-hatena-mode nil))
   (arg
    (setq ya-hatena-mode t))
   (t
    (setq ya-hatena-mode (not ya-hatena-mode))))
  (if ya-hatena-mode
    nil))

(defun ya-hatena-define-mode-map ()
  "キーマップ `ya-hatena-define-mode-map' を定義する。"
  (unless (keymapp ya-hatena-mode-map)
    (setq ya-hatena-mode-map (make-sparse-keymap))
    (setq minor-mode-map-alist
          (cons (cons 'ya-hatena-mode ya-hatena-mode-map)
                minor-mode-map-alist))))

;;------------------------------------------------
;; ユーティリティ
;;------------------------------------------------
(defvar *yhtn:d:buf-edit* "*yhtn:d:edit*")
(defvar *yhtn:d:buf-edit-draft* "*yhtn:d:edit-draft*")

(defun eval-string (str)
  (with-temp-buffer
    (insert str)
    (read (buffer-string))))

(defun to-utf8 (s)
  (if (stringp s)
      (decode-coding-string s 'utf-8)))

(defun yhtn:d:get-date-and-entryid-from-id (id)
  "\"tag:d.hatena.ne.jp,2008:diary-{はてなID}-{date}-{entry_id}\"
=> ((date . \"{date}\") (entry_id . \"{entry_id}\"))"
  (let ((l (split-string (car (reverse (split-string id ":"))) "-")))
    (list (cons 'date (nth 2 l))
          (cons 'entry_id (nth 3 l)))))

(defun yhtn:d:get-blog-member-for-anything (entries)
  "取得した記事一覧をAnything向けに加工する．"
  (mapcar '(lambda (n)
             (let* ((updated (caddr (assoc 'updated n)))
                    (title (to-utf8 (caddr (assoc 'title n))))
                    (date (cdr (assoc 'date (yhtn:d:get-date-and-entryid-from-id (caddr (assoc 'id n))))))
                    (entry_id (cdr (assoc 'entry_id (yhtn:d:get-date-and-entryid-from-id (caddr (assoc 'id n)))))))
               (decode-coding-string
                (format "%S" (list updated title date entry_id))
                'utf-8))) entries))

(defun yhtn:d:view-entry (entry)
  "エントリを閲覧する．"
  (let ((title (to-utf8 (caddr (assoc 'title entry))))
        (content (to-utf8 (caddr (if (assoc 'hatena:syntax entry)
                                     (assoc 'hatena:syntax entry)
                                   (assoc 'content entry)))))
        (buf (get-buffer-create "*yhtn:d:view*")))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (ya-hatena-mode t)
      (insert (format "*%s\n" title))
      (insert content)
      (setq buffer-read-only t))))

(defun yhtn:d:publish-draft-entry (entry-id)
  (let* ((entry (yhtn:d:get-draft-member entry-id))
         (title (to-utf8 (caddr (assoc 'title entry))))
         (content (to-utf8 (caddr (if (assoc 'hatena:syntax entry)
                                      (assoc 'hatena:syntax entry)
                                    (assoc 'content entry))))))
    (yhtn:d:put-draft-member title content entry-id nil t)))
    
(defun yhtn:d:edit-entry (entry &optional draft?)
  "エントリを編集する．"
  (let ((title (to-utf8 (caddr (assoc 'title entry))))
        (content (to-utf8 (caddr (if (assoc 'hatena:syntax entry)
                                     (assoc 'hatena:syntax entry)
                                   (assoc 'content entry)))))
        (date (assoc 'date (yhtn:d:get-date-and-entryid-from-id (nth 2 (assoc 'id entry)))))
        (id (assoc 'entry_id (yhtn:d:get-date-and-entryid-from-id (nth 2 (assoc 'id entry)))))
        (buf (get-buffer-create (if draft? *yhtn:d:buf-edit* *yhtn:d:buf-edit-draft*))))
    (with-current-buffer buf
      (ya-hatena-mode t)
      (make-local-variable 'yhtn:d:date)
      (make-local-variable 'yhtn:d:entry_id)
      (setq yhtn:d:date (cdr date))
      (setq yhtn:d:entry_id (cdr id))
      (define-key ya-hatena-mode-map "\C-cs" 'yhtn:d:save-draft)
      (define-key ya-hatena-mode-map "\C-cp" 'yhtn:d:publish-draft)
      (switch-to-buffer buf)
      (insert (format "*%s\n" title))
      (insert content))))

;;------------------------------------------------
;; 各種コマンド
;;------------------------------------------------

(defun yhtn:d:new-entry ()
  "新規エントリを作成する"
  (let ((buf (get-buffer-create "*hatena-diary*")))
    (with-current-buffer buf
      (ya-hatena-mode t)
      (switch-to-buffer buf))))

(defun yhtn:d:post-draft-collection-buffer ()
  "バッファを新規下書きとしてポストする"
  (interactive)
  (if (string= (buffer-name) "*hatena-diary*")
      (let* ((text (split-string (replace-regexp-in-string "\n" "\n\r"
                                                           (buffer-substring-no-properties (point-min) (point-max))) "\r"))
             (title (let () (string-match "\\*\\(.*\\)" (car text)) (match-string 1 (car text))))
             (body (mapconcat 'concat (cdr text) "")))
        (yhtn:d:post-draft-collection title body)
        (kill-buffer (current-buffer)))
    (message "*hatena-diary* バッファではないので終了します")))

(defun yhtn:d:post-blog-collection-buffer ()
  "バッファを新規エントリとしてポストする"
  (interactive)
  (if (string= (buffer-name) "*hatena-diary*")
      (let* ((text (split-string (replace-regexp-in-string "\n" "\n\r"
                                                           (buffer-substring-no-properties (point-min) (point-max))) "\r"))
             (title (let () (string-match "\\*\\(.*\\)" (car text)) (match-string 1 (car text))))
             (body (mapconcat 'concat (cdr text) "")))
        (yhtn:d:post-blog-collection title body)
        (kill-buffer (current-buffer)))
    (message "*hatena-diary* バッファではないので終了します")))

(defun yhtn:d:put-draft-collection-buffer (date entry_id)
  "下書きを更新する"
  (interactive)
  (if (string= (buffer-name) *yhtn:d:buf-edit*)
      (let* ((text (split-string (replace-regexp-in-string "\n" "\n\r"
                                                           (buffer-substring-no-properties (point-min) (point-max))) "\r"))
             (title (let () (string-match "\\*\\(.*\\)" (car text)) (match-string 1 (car text))))
             (body (mapconcat 'concat (cdr text) "")))
        (yhtn:d:put-draft-member title body date entry_id)
        (define-key ya-hatena-mode-map "\C-cp" 'yhtn:d:post-draft-collection-buffer)
        (kill-buffer (current-buffer)))
    (message "*hatena-diary* バッファではないので終了します")))

(defun yhtn:d:save-draft ()
  "下書きとして保存する"
  (interactive)
  (if (or (string= (buffer-name) *yhtn:d:buf-edit*)
          (string= (buffer-name) *yhtn:d:buf-edit-draft*))
      (let* ((text (split-string (replace-regexp-in-string "\n" "\n\r"
                                                           (buffer-substring-no-properties (point-min) (point-max))) "\r"))
             (title (let () (string-match "\\*\\(.*\\)" (car text)) (match-string 1 (car text))))
             (body (mapconcat 'concat (cdr text) ""))
             (id yhtn:d:entry_id))
        (yhtn:d:post-draft-collection title body id)
        ;;(define-key ya-hatena-mode-map "\C-cp" 'yhtn:d:post-blog-collection-buffer)
        (kill-buffer (current-buffer)))
    (message "*hatena-diary* バッファではないので終了します")))

(defun yhtn:d:publish-draft ()
  "現在の下書きを公開する"
  (interactive)
  (if (or (string= (buffer-name) *yhtn:d:buf-edit*)
          (string= (buffer-name) *yhtn:d:buf-edit-draft*))
      (let* ((text (split-string (replace-regexp-in-string "\n" "\n\r"
                                                           (buffer-substring-no-properties (point-min) (point-max))) "\r"))
             (title (let () (string-match "\\*\\(.*\\)" (car text)) (match-string 1 (car text))))
             (body (mapconcat 'concat (cdr text) ""))
             (id yhtn:d:entry_id))
        (yhtn:d:put-draft-member title body id nil t)
        (kill-buffer (current-buffer)))
    (message "*hatena-diary* バッファではないので終了します")))

(defun yhtn:d:quit ()
  (interactive)
  (ya-hatena-mode nil)
  (kill-buffer (current-buffer)))

;; (defun yhtn:d:get-blog-member-titles ()
;;   (mapcar '(lambda (n) (decode-coding-string
;;                         (caddr (assoc 'title n))
;;                         'utf-8))
;;           (yhtn:d:get-blog-collection)))

;; (defun yhtn:d:get-blog-member-updated ()
;;   (mapcar '(lambda (n) (decode-coding-string
;;                         (caddr (assoc 'updated n))
;;                         'utf-8)) yhtn:entries))


;;------------------------------------------------
;; Anything
;;------------------------------------------------
(defvar yhtn:menu '(("はてなダイアリー : 新しく日記を書く" . (yhtn:d:new-entry))
                    ("はてなダイアリー : 日記一覧を見る"   . (anything anything-c-source-hatena-diary-entries))
                    ("はてなダイアリー : 下書き一覧を見る" . (anything anything-c-source-hatena-draft-entries))))

(defvar yhtn:d:action '(("記事を投稿する" . (yhtn:d:post-blog-collection-buffer))
                        ("記事を下書きとして保存する" . (yhtn:d:post-draft-collection-buffer))
                        ("編集を終了する(書いた内容は保存されません)" . (yhtn:d:quit))))

(setq anything-c-source-ya-hatena-diary-action
      '((name . "操作")
        (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                             (let ((menu yhtn:d:action))
                               (mapc (lambda (m)
                                       (insert (format "%S\n" m)))
                                     menu)))))
        (candidates-in-buffer)
        (real-to-display . (lambda (c)
                             (let ((l (eval-string c)))
                               (format "%s" (car l)))))
        (action ("Do" . (lambda (c)
                          (eval (cdr (eval-string c))))))))

(setq anything-c-source-ya-hatena-menu
      '((name . "Hatena")
        (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                             (let ((menu yhtn:menu))
                               (mapc (lambda (m)
                                       (insert (format "%S\n" m)))
                                     menu)))))
        (candidates-in-buffer)
        (real-to-display . (lambda (c)
                             (let ((l (eval-string c)))
                               (format "%s" (car l)))))
        (action ("Do" . (lambda (c)
                          (eval (cdr (eval-string c))))))))

(setq anything-c-source-hatena-diary-entries
      '((name . "Hatena Diary")
        (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                             (let ((e (yhtn:d:get-blog-member-for-anything (yhtn:d:get-blog-collection))));;yhtn:entries)))
                               (while e
                                 (insert (concat (car e) "\n"))
                                 (setq e (cdr e)))))))
        (candidates-in-buffer)

        (real-to-display . (lambda (c)
                             (let ((l (eval-string c)))
                               (format "%s %s" (nth 0 l) (nth 1 l)))))
        (action ("View"   . (lambda (c) (let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (yhtn:d:view-entry (yhtn:d:get-blog-member date entry_id)))))
                ("Edit"   . (lambda (c) (let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (yhtn:d:edit-entry (yhtn:d:get-blog-member date entry_id)))))
                ("Delete" . (lambda (c) (let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (message (yhtn:d:delete-blog-member date entry_id))))))))

(setq anything-c-source-hatena-draft-entries
      '((name . "Hatena Diary")
        (init . (lambda () (with-current-buffer (anything-candidate-buffer 'global)
                             (let ((e (yhtn:d:get-blog-member-for-anything (yhtn:d:get-draft-collection))));;yhtn:entries)))
                               (while e
                                 (insert (concat (car e) "\n"))
                                 (setq e (cdr e)))))))
        (candidates-in-buffer)
        (real-to-display . (lambda (c)
                             (let ((l (eval-string c)))
                               (format "%s %s" (nth 0 l) (nth 1 l)))))
        (action ("View"    . (lambda (c) (yhtn:d:draft-action c 'yhtn:d:view-entry)))
                ("Edit"    . (lambda (c) (yhtn:d:draft-action c 'yhtn:d:edit-entry)))
                ("Delete"  . (lambda (c) (yhtn:d:draft-action c 'yhtn:d:delete-draft-member)))
                ("Publish" . (lambda (c) (yhtn:d:draft-action c 'yhtn:d:publish-draft-entry))))
        (persistent-action . (lambda (c) (yhtn:d:draft-action c 'yhtn:d:view-entry)))))

(defun yhtn:d:draft-action (c f)
  (if (equal f 'yhtn:d:delete-draft-member)
      (funcall f (nth 3 (eval-string c))))
  (if (equal f 'yhtn:d:publish-draft-entry)
      (funcall f (nth 3 (eval-string c))))
  (if (equal f 'yhtn:d:edit-entry)
      (funcall f (yhtn:d:get-draft-member (nth 3 (eval-string c))) t))
  (if (equal f 'yhtn:d:view-entry)
      (funcall f (yhtn:d:get-draft-member (nth 3 (eval-string c))))))


(defun ya-hatena ()
  (interactive)
  (anything anything-c-source-ya-hatena-menu))

(defun yhtn:d:action ()
  (interactive)
  (anything anything-c-source-ya-hatena-diary-action))

;; Key Binding
(ya-hatena-define-mode-map)
(define-key ya-hatena-mode-map "\C-cp" 'yhtn:d:post-blog-collection-buffer)
(define-key ya-hatena-mode-map "\C-cd" 'yhtn:d:post-draft-collection-buffer)
(define-key ya-hatena-mode-map "\C-cq" 'yhtn:d:quit)
(define-key ya-hatena-mode-map "\C-cm" 'yhtn:d:action)
(define-key ya-hatena-mode-map [left] 'ya-hatena-post-new-entry)


(provide 'ya-hatena-mode)
