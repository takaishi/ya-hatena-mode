(require 'xml)
;; ユーティリティ

;; WSSE認証用のヘッダを生成する
;; →を参考にした．Emacs LispでX-WSSE認証をする - Emacs/Lisp/Drill - Emacsグループ - http://emacs.g.hatena.ne.jp/k1LoW/20081112/1226460796
(defun yhtn:x-wsse (yhtn:username password)
  (let ((created (format-time-string "%Y-%m-%dT%TZ" (current-time)))
        (nonce (sha1 (format-time-string "%Y-%m-%dT%T%z" (current-time))))
        (url "http://f.hatena.ne.jp/atom/feed")
        (x-wsse "")
        (x-wsse-list nil))
    (setq x-wsse (concat "UsernameToken Username=\"" yhtn:username "\", PasswordDigest=\""
                         (base64-encode-string (sha1-binary (concat nonce created password))) 
                         "\", Nonce=\""  (base64-encode-string nonce) 
                         "\", Created=\"" created "\""))
    (setq x-wsse-list (cons "X-WSSE" x-wsse))
    x-wsse-list))

(defun yhtn:request (url method &optional extra-headers data)
  (let ((url-request-method method)
        (url-request-extra-headers extra-headers)
        (url-request-data data))
    ;; (if extra-headers
    ;;     (setq url-request-extra-headers extra-headers))
    ;; (if data
    ;;     (setq url-request-data data))
    (with-current-buffer (url-retrieve-synchronously url)
      (pop-to-buffer (current-buffer))
      (let ((txt (split-string (buffer-substring-no-properties (point-min) (point-max)) "\n"))
            (xml (xml-parse-region (point-min) (point-max))))
        (if (equal xml nil)
            (car txt)
          xml)))))

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
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog"))
        (method "POST")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd)))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content type=\"text/plain\">" content "</content>"
                      (if updated updated)
                      "</entry>")))
    (yhtn:request url method wsse data)))


;; 日記エントリーの一覧の取得 (ブログ コレクションURI への GET)
(defun yhtn:d:get-blog-collection ()
  (let ((url "http://d.hatena.ne.jp/r_takaishi/atom/blog")
        (method "GET")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (filter '(lambda (n) (when (listp n)
                           (equal (car n) 'entry)))
            (cdr (car (yhtn:request url method wsse))))))


;; 日記エントリーの取得 (ブログ メンバURI の GET)
(defun yhtn:d:get-blog-member (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/r_takaishi/atom/blog" "/" date "/" entry_id))
        (method "GET")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (cdr (car (yhtn:request url method wsse)))))

;; 日記エントリーのタイトル及び本文の変更 (ブログ メンバURI への PUT)
(defun yhtn:d:put-blog-member (title content date entry_id &optional updated)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog/" date "/" entry_id))
        (method "PUT")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd)))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content type=\"text/plain\">" content "</content>"
                      (if updated updated)
                      "</entry>")))
    (cdr (car (yhtn:request url method wsse data)))))

;; 日記エントリーの削除 (ブログ メンバURI への DELETE)
(defun yhtn:d:delete-blog-member (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/blog/" date "/" entry_id))
        (method "DELETE")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (yhtn:request url method wsse)))

;; 下書きコレクションの操作
;; 新規下書きエントリーの投稿 (下書きコレクションURI への POST)
(defun yhtn:d:post-draft-collection (title content &optional updated)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/draft"))
        (method "POST")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd)))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content type=\"text/plain\">" content "</content>"
                      (if updated updated)
                      "</entry>")))
    (yhtn:request url method wsse data)))


;; 日記エントリーの一覧の取得 (ブログ コレクションURI への GET)
(defun yhtn:d:get-draft-collection ()
  (let ((url "http://d.hatena.ne.jp/r_takaishi/atom/draft")
        (method "GET")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (filter '(lambda (n) (when (listp n)
                           (equal (car n) 'entry)))
            (cdr (car (yhtn:request url method wsse))))))


;; 日記エントリーの取得 (ブログ メンバURI の GET)
(defun yhtn:d:get-draft-member (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/r_takaishi/atom/draft" "/" entry_id))
        (method "GET")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (cdr (car (yhtn:request url method wsse)))))

;; 日記エントリーのタイトル及び本文の変更 (ブログ メンバURI への PUT)
(defun yhtn:d:put-draft-member (title content date entry_id &optional updated publish?)
  (let* ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/draft" "/" entry_id))
        (method "PUT")
        (wsse (yhtn:x-wsse yhtn:username yhtn:passwd))
        (header (if publish?
                    (list (cons "X-HATENA-PUBLISH" "1") wsse)
                  (list wsse)))
        (data (concat "<entry xmlns=\"http://purl.org/atom/ns#\">"
                      "<title>" title "</title>"
                      "<content type=\"text/plain\">" content "</content>"
                      (if updated updated)
                      "</entry>")))
    (cdr (car (yhtn:request url method header data)))))

;; 日記エントリーの削除 (ブログ メンバURI への DELETE)
(defun yhtn:d:delete-draft-member (date entry_id)
  (let ((url (concat "http://d.hatena.ne.jp/" yhtn:username "/atom/draft" "/" entry_id))
        (method "DELETE")
        (wsse (list (yhtn:x-wsse yhtn:username yhtn:passwd))))
    (yhtn:request url method wsse)))


(provide 'ya-hatena-api)
