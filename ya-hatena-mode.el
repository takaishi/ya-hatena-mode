(add-to-list 'load-path "~/code/elisp/ya-hatena-mode")
(require 'ya-hatena-api)
(require 'xml)
(require 'el-expectations)
(require 'sxml)

(expectations
 (desc "comment")
 (expect '((date . "20110427") (entry_id . "1303867061"))
   (yhtn:d:get-date-and-entryid-from-id "tag:d.hatena.ne.jp,2011:diary-r_takaishi-20110427-1303867061")))

;; はてなAPIを使用するための設定項目
(defvar yhtn:username "")
(defvar yhtn:passwd "")
(load "./account-info.el")

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

;; 投稿コマンド
;; 新規エントリを作成する
(defun yyhtn:d:new-entry ()
  (interactive)
  (let ((buf (get-buffer-create "*hatena-diary*")))
    (with-current-buffer buf
      (ya-hatena-mode)
      (switch-to-buffer buf))))

;; バッファを新規エントリとしてポストする
(defun yhtn:d:post-blog-collection-buffer ()
  (let ((title "")
        (body ((message "a"))))))
(defun yhtn:d:post-blog-collection-region () (message "a"))
;; (defun my-org-export-html (beg end)
;;   (interactive "r")
;;   (org-export-region-as-html beg end t 'string))
;; (yhtn:d:post-blog-collection "TEST" "This is a test")
;; (yhtn:d:put-blog-member "put" "hogehoaaaa" "20110427" "1303881762")


(defun yhtn:d:search-entry (key value entries)
  (let ((k key)
        (v value)
        (l entries))
    (filter '(lambda (e)
               (string= (decode-coding-string
                         (caddr (assoc k e))
                         'utf-8) v))
            l)))

;; (defmacro htn:d:define-get-entry-element-method (elem)
;;   (eval `(defun ,(intern (concat "htn:d:get-entry-" elem)) ()
;;            (mapcar '(lambda (n) (decode-coding-string
;;                                  (caddr (assoc ,(make-symbol elem) n))
;;                                  'utf-8))
;;                    (htn:d:get-entries)))))

(defun yhtn:d:get-date-and-entryid-from-id (id)
  (let ((l (split-string (car (reverse (split-string id ":"))) "-")))
    (list (cons 'date (nth 2 l))
          (cons 'entry_id (nth 3 l)))))

(defun yhtn:d:get-blog-member-titles ()
  (mapcar '(lambda (n) (decode-coding-string
                        (caddr (assoc 'title n))
                        'utf-8))
          (yhtn:d:get-blog-collection)))

(defun yhtn:d:get-blog-member-updated ()
  (mapcar '(lambda (n) (decode-coding-string
                        (caddr (assoc 'updated n))
                        'utf-8)) yhtn:entries))

(defun yhtn:d:get-blog-member-for-anything (entries)
  (mapcar '(lambda (n)
             (let* ((updated (caddr (assoc 'updated n)))
                    (title (to-utf8 (caddr (assoc 'title n))))
                    (date (cdr (assoc 'date (yhtn:d:get-date-and-entryid-from-id (caddr (assoc 'id n))))))
                    (entry_id (cdr (assoc 'entry_id (yhtn:d:get-date-and-entryid-from-id (caddr (assoc 'id n)))))))
               (decode-coding-string
                (format "%S" (list updated title date entry_id))
                ;;(format "%s|%s|%s|%s" updated title date entry_id)
                'utf-8))) entries))
;;(setq yhtn:entries (yhtn:d:get-blog-collection))
;;(yhtn:d:get-blog-member-for-anything yhtn:entries)
  (defun eval-string (str)
  (with-temp-buffer
    (insert str)
    (read (buffer-string))))


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
                ("Edit"   . (lambda (c)))
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
        (action ("View"   . (lambda (c) (let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (yhtn:d:view-entry (yhtn:d:get-draft-member date entry_id)))))
                ("Edit"   . (lambda (c)))
                ("Delete" . (lambda (c) (let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (message (yhtn:d:delete-draft-member date entry_id)))))
                ("Publish" . (lambda (c))))
        (persistent-action . (lambda (c)(let* ((entry (eval-string c))
                                               (date  (nth 2 entry))
                                               (entry_id (nth 3 entry)))
                                          (yhtn:d:view-entry (yhtn:d:get-draft-member date entry_id)))))))

;; (setq yhtn:test-entry (yhtn:d:get-blog-member "20110427" "1303867061"))
;;(anything anything-c-source-hatena-diary-entries) 
;;(anything anything-c-source-hatena-draft-entries)
;; (eval-string (car (yhtn:d:get-blog-member-for-anything yhtn:entries)))

;; (insert (format "%S" yhtn:test-entry))
;; (to-utf8 (car (cdr (cdr (assoc 'hatena:syntax yhtn:test-entry)))))
;; (yhtn:d:view-entry yhtn:test-entry)
(defun to-utf8 (s)
  (decode-coding-string s 'utf-8))

(defun yhtn:d:view-entry (entry)
  (let ((title (to-utf8 (caddr (assoc 'title entry))))
        (content (to-utf8 (caddr (if (assoc 'hatena:syntax entry)
                                     (assoc 'hatena:syntax entry)
                                   (assoc 'content entry)))))
        (buf (get-buffer-create "*yhtn:d:view*")))
    (with-current-buffer buf
      (switch-to-buffer buf)
      (insert title)
      (insert content))))

;(decode-coding-string (cadr  (cdr (assoc 'content (car yhtn:entries)))) 'utf-8)

;(encode-coding-string (assoc "content" (assoc "entry" (cdr yhtn:xml))) 'utf-8)


;(format-time-string "%Y-%m-%dT%H:%M"(org-time-string-to-time "2011-04-27T10:17:41+09:00 "))
  


;;(parse-time-string "2011-04-27T10:17:41+09:00")
(ya-hatena-define-mode-map)
(define-key ya-hatena-mode-map "\C-cp" 'ya-hatena-post-new-entry)
(define-key ya-hatena-mode-map [left] 'ya-hatena-post-new-entry)
(provide 'ya-hatena-mode)
