(defun cadar (l)
  (car (cdr (car l))))

(defun sxml->xml (sxml)
  (defun make-attr (l ac)
    (if (equal l '())
        ac
      (make-attr (cdr l) (concat ac " " (symbol-name (car (car l)))
                                  "=\"" (cadar l) "\""))))

  (defun make-xml (x bef aft)
    (cond ((stringp x) (append bef (list x) aft))
          ((and (consp x) (symbolp (car x)))
           (let ((tag (symbol-name (car x))))
             (if (and (not (equal (cdr x) '()))
                      (consp (car (cdr x)))
                      (eq (car (car (cdr x))) '@))
                 (make-xml (cdr (cdr x))
                           (append bef (list 'BO tag (make-attr (cdr (car (cdr x))) "") 'BC))
                           (append     (list 'EO tag 'EC) aft))
               (make-xml (cdr x)
                         (append bef (list 'BO tag 'BC))
                         (append     (list 'EO tag 'EC) aft)))))
          ((consp x) (make-xml ()
                               (make-xml (car x) bef ())
                               (make-xml (cdr x) () aft)))
          (t (append bef aft))))
  (make-xml sxml () ()))


                      
(setq tags '(html body head title ))
(defun cat-normal (l s)
  (cond ((equal l '()) s)
        ((stringp (car l))
         (cat-normal (cdr l) (concat s (car l))))
        (t (cat-normal (cdr l)
                       (concat s (case (car l)
                               ((BO)    "<")
                               ((EO)    "</")
                               ((BC EC) ">\n")))))))

;; (setq data
;;   `("<!DOCTYPE etc...>\n"
;;     (html
;;      (head (title "sxml"))
;;      (body
;;       (p "hogehoge")))))

;; (cat-normal (sxml->xml data) "")

(provide 'sxml)