
;;
;; Read me:
;;
;; These codes are based on the book "On Lisp" by Paul Graham.
;; I modified original code to support wild-card pattern matching and embedded predicates.
;; The algorithm I used for wild-card matching is described in "The Elements of Artificial 
;; Intellingence".
;; This code is just a reference for study and not for practical use.
;; (Maybe bugs are in the code, Absolutely no warrenty)
;;
;; Usage: load this module and call match function.
;;
;; Example 1:
;; (match '(A *B C) '(A B B B B C))
;; ((*B B B B B)) 
;; T
;;
;; Example 2:
;; (match '(?A *B C) (A B B B B ?C))
;; ((?A . A) (?C . C) (*B B B B B))
;; T
;;
;; Example 3:
;; (match '(I AM (numberp ?AGE) YEARS OLD *X) '(I AM 30 YEARS OLD GREEK GEEK))
;; ((?AGE . 30) (*X GREEK GEEK))
;; T
;;
;; Option:
;; you have to register predicates you want to use by modifying *preds* parameter.
;; initiall you can only use numberp predicate.
;;

;; Some macros stolen from "On Lisp"
(defmacro dbind (&rest args)
  `(destructuring-bind ,@args))

(defmacro mvbind (&rest args)
  `(multiple-value-bind ,@args))

;;
;; Add more predicate function if you want to use them.
;; Predicate clause must be the list whose length is 2.
;; Match function apply predicate function against the matcher
;; and add pattern if it returns true.
(defparameter *preds* '(numberp))

;; This function is implemented by me.
;; See "The Elements of Artificial Intelligence" for algorithm description.
(defun match-* (x y &optional binds wbinds)
  (mvbind (b2 yes)
          ;; subcase-1 astarisk matches 1 element in y
          (match (cdr x) (cdr y) binds)
          (if yes
              (values (cons (cons (car x) (append wbinds (list (car y)))) b2) yes)
              (mvbind (b2 yes)
                      ;; subcase-2 matched without astarisk
                      (match (cdr x) y binds wbinds)
                      (if yes
                          (values b2 yes)
                          (mvbind (b2 yes)
                            ;; subcase-3 astarisk matched rest of y
                            (match x (cdr y) binds (append wbinds (list (car y))))
                            (values b2 yes)))))))

;; Most part of this function is stolen from "On Lisp"
(defun match (x y &optional binds wbinds)
  (cond
   ((eql x y) (values binds t))
   ((assoc x binds) (match (binding x binds) y binds))
   ((assoc y binds) (match x (binding y binds) binds))
   ((var? x) (values (cons (cons x y) binds) t))
   ((var? y) (values (cons (cons y x) binds) t))
   ;; Added to support matching using predicates
   ((pred? x y) (values (cons (cons (cadr x) y) binds) t))
   ((pred? y x) (values (cons (cons (cadr y) x) binds) t))
   ;; Added to support wild-card matching. 
   ;; Wild-cards symbols could be placed in x (right-hand side) only
   ((and (consp x) (consp y) (wild? (car x)))
    (match-* x y binds wbinds))
   (t 
    (when (and (consp x) (consp y))
          (multiple-value-bind (b2 yes)
                               (match (car x) (car y) binds)
            (and yes (match (cdr x) (cdr y) b2)))))))

(defun var? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\?)))

;; wild card pattern must be start with '*'
(defun wild? (x)
  (and (symbolp x)
       (eql (char (symbol-name x) 0) #\*)))

;; if given symbol x is a member of *preds*, apply it y.
(defun pred? (x y)
  (if (and (consp x)
           (symbolp (car x))
           (member (car x) *preds*))
           ;;(fboundp (car x)))
       (apply (car x) (list y))))

;; Rest of the codes are also stolen from "On Lisp"
(defun binding (x binds)
  (let ((b (assoc x binds)))
       (if b
           (or (binding (cdr b) binds)
               (cdr b)))))

(defvar *rules* (make-hash-table))

(defmacro <- (con &optional ant)
  `(length (push (cons (cdr ',con) ',ant)
                 (gethash (car ',con) *rules*))))


