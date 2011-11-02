(require 'xml)

;; URL
(defun yhtn:d:blog-collection-url ()
  (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog"))

(defun yhtn:d:blog-member-url (date entry_id)
  (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog/" date "/" entry_id))

(defun yhtn:d:draft-collection-url ()
  (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/draft"))

(defun yhtn:d:draft-member-url (entry_id)
  (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/draft" "/" entry_id))

;; XML
(defun yhtn:d:data-xml (title content &optional updated)
  (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
          "<title>" title "</title>"
          "<content type=\"text/plain\">" content "</content>"
          (or updated)
          "</entry>"))

;; ユーティリティ
(defun yhtn:filter (pred ls)
  (let (a)
    (dolist (x ls (nreverse a))
      (and (funcall pred x) (push x a)))))

(defun yhtn:x-wsse ()
  " WSSE認証用のヘッダを生成する
→を参考にした．Emacs LispでX-WSSE認証をする - Emacs/Lisp/Drill - Emacsグループ - http://emacs.g.hatena.ne.jp/k1LoW/20081112/1226460796"
  (let ((created (format-time-string "%Y-%m-%dT%TZ" (current-time)))
        (nonce (sha1 (format-time-string "%Y-%m-%dT%T%z" (current-time))))
        (url "http://f.hatena.ne.jp/atom/feed")
        (x-wsse "")
        (x-wsse-list nil))
    (setq x-wsse (concat "UsernameToken Username=\"" yhtn:username "\", PasswordDigest=\""
                         (base64-encode-string (sha1-binary (concat nonce created yhtn:passwd))) 
                         "\", Nonce=\""  (base64-encode-string nonce) 
                         "\", Created=\"" created "\""))
    (setq x-wsse-list (cons "X-WSSE" x-wsse))
    (list x-wsse-list)))


(defun yhtn:request (url method &optional extra-headers data)
  (let* ((url-request-method method)
         (url-request-extra-headers extra-headers)
         (url-request-data (encode-coding-string (or data "") 'utf-8))
         (buf (url-retrieve-synchronously url))
    ;; (if extra-headers
    ;;     (setq url-request-extra-headers extra-headers))
    ;; (if data
    ;;     (setq url-request-data data))
         (res (with-current-buffer buf
                (let ((txt (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
                      (xml (xml-parse-region (point-min) (point-max))))
                  ;; (if (equal xml nil)
                  ;;     (car txt)
                    (cons txt xml)))))
    (kill-buffer buf)
    res))

;; (defun yhtn:request (url wsse)
;;   (setq url-request-extra-headers (list wsse))
;; ;;  (switch-to-buffer (url-retrieve-synchronously url)))
;;   (with-current-buffer (url-retrieve-synchronously url)
;;     (xml-parse-region (point-min) (point-max))))
;    (buffer-substring-no-properties (point-min) (point-max))

;; はてなダイアリー
;; 日記エントリーの操作
;; 新規日記エントリーの投稿 (ブログ コレクションURI への POST)
(defun yhtn:d:post-blog-collection (title content &optional updated)
  (let ((url (yhtn:d:blog-collection-url))
        (method "POST")
        (wsse (yhtn:x-wsse))
        (data (yhtn:d:data-xml title content updated)))
    (message (caar (yhtn:request url method wsse data)))))

;; 日記エントリーの一覧の取得 (ブログ コレクションURI への GET)
(defun yhtn:d:get-blog-collection ()
  (let ((url (yhtn:d:blog-collection-url))
        (method "GET")
        (wsse (yhtn:x-wsse)))
    (yhtn:filter '(lambda (n) (when (listp n)
                           (equal (car n) 'entry)))
            (cdr (car (cdr (yhtn:request url method wsse)))))))


;; 日記エントリーの取得 (ブログ メンバURI の GET)
(defun yhtn:d:get-blog-member (date entry_id)
  (let ((url (yhtn:d:blog-member-url date entry_id))
        (method "GET")
        (wsse (yhtn:x-wsse)))
    (cadr (yhtn:request url method wsse))))

;; 日記エントリーのタイトル及び本文の変更 (ブログ メンバURI への PUT)
(defun yhtn:d:put-blog-member (title content date entry_id &optional updated)
  (let ((url (yhtn:d:blog-member-url date entry_id))
        (method "PUT")
        (wsse (yhtn:x-wsse))
        (data (yhtn:d:data-xml title content updated)))
    (message (caar (yhtn:request url method wsse data)))))

;; 日記エントリーの削除 (ブログ メンバURI への DELETE)
(defun yhtn:d:delete-blog-member (date entry_id)
  (let ((url (yhtn:d:blog-member-url date entry_id))
        (method "DELETE")
        (wsse (yhtn:x-wsse)))
    (message (caar (yhtn:request url method wsse)))))

;; 下書きコレクションの操作
;; 新規下書きエントリーの投稿 (下書きコレクションURI への POST)
(defun yhtn:d:post-draft-collection (title content &optional updated)
  (let ((url (yhtn:d:draft-collection-url))
        (method "POST")
        (wsse (yhtn:x-wsse))
        (data (yhtn:d:data-xml title content updated)))
    (message (caar (yhtn:request url method wsse data)))))


;; 日記エントリーの一覧の取得 (ブログ コレクションURI への GET)
(defun yhtn:d:get-draft-collection ()
  (let ((url (yhtn:d:draft-collection-url))
        (method "GET")
        (wsse (yhtn:x-wsse)))
    (yhtn:filter '(lambda (n) (when (listp n)
                           (equal (car n) 'entry)))
                 (cdr (car (cdr (yhtn:request url method wsse)))))))


;; 日記エントリーの取得 (ブログ メンバURI の GET)
(defun yhtn:d:get-draft-member (entry_id)
  (let* ((url (yhtn:d:draft-member-url entry_id))
        (method "GET")
        (wsse (yhtn:x-wsse))
        (result (yhtn:request url method wsse)))
    (message (caar result))
    (cadr result)))

;; 日記エントリーのタイトル及び本文の変更 (ブログ メンバURI への PUT)
(defun yhtn:d:put-draft-member (title content entry_id &optional updated publish?)
  (let* ((url (yhtn:d:draft-member-url entry_id))
        (method "PUT")
        (wsse (yhtn:x-wsse))
        (header (if publish?
                    (list (cons "X-HATENA-PUBLISH" "1") wsse)
                  (list wsse)))
        (data (yhtn:d:data-xml title content updated)))
    (message (caar (yhtn:request url method header data)))))

;; 日記エントリーの削除 (ブログ メンバURI への DELETE)
(defun yhtn:d:delete-draft-member (entry_id)
  (let* ((url (yhtn:d:draft-member-url entry_id))
         (method "DELETE")
         (wsse (yhtn:x-wsse))
         (result (yhtn:request url method wsse)))
    (message (caar result))
    (cadr result)))


;; はてなフォトライフ
;; 新規写真の投稿
(defun yhtn:f:post-post-uri (title photo &optional updated)
  (let ((url (concat "http://f.hatena.ne.jp/atom/post"))
        (method "POST")
        (wsse (yhtn:x-wsse))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content mode=\"base64\" type=\"image/jpeg\">" photo "</content>"
                      (if updated updated)
                      "</entry>")))
    (message (caar (yhtn:request url method wsse data)))))

;; EditURI
;; 写真の参照
(defun yhtn:f:get-edit-uri (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog" "/" date "/" entry_id))
        (method "GET")
        (wsse (yhtn:x-wsse)))
    (cdr (car (cdr (yhtn:request url method wsse))))))

;; 投稿した写真のタイトルの変更
(defun yhtn:f:put-edit-uri (title content date entry_id &optional updated)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog/" date "/" entry_id))
        (method "PUT")
        (wsse (yhtn:x-wsse))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content type=\"text/plain\">" content "</content>"
                      (if updated updated)
                      "</entry>")))
    (message (caar (yhtn:request url method wsse data)))))
;;(cdr (car (cdr (yhtn:request url method wsse data))))))

;; 投稿した写真の削除
(defun yhtn:f:delete-edit-uri (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog/" date "/" entry_id))
        (method "DELETE")
        (wsse (yhtn:x-wsse)))
    (message (caar (yhtn:request url method wsse)))))

;; FeedURI
;; 最近投稿した写真の一覧の取得
(defun yhtn:f:get-feed-uri ()
  (let ((url "http://f.hatena.ne.jp/atom/feed")
        (method "GET")
        (wsse (yhtn:x-wsse)))
    (yhtn:filter '(lambda (n) (when (listp n)
                           (equal (car n) 'entry)))
            (cdr (car (cdr (yhtn:request url method wsse)))))))



(provide 'ya-hatena-api)
