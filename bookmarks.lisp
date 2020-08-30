;;;; bookmarks.lisp

(in-package #:bookmarks)

(defclass bookmark ()
  ((url :col-type :text
        :initarg :url
        :initform (error "You must provide a URL.")
        :accessor url)
   (description :col-type (or :null :text)
                :initarg :description
                :accesson description))
  (:metaclass mito:dao-table-class))

(defclass tag ()
  ((name :col-type :text
         :initarg :name
         :initform (error "You must provide a name for a tag.")
         :accessor tag))
  (:unique-keys name)
  (:metaclass mito:dao-table-class))

(defclass l-bookmark-tag ()
  ((bookmark :col-type bookmark :initarg :bookmark)
   (tag :col-type tag :initarg :tag))
  (:metaclass mito:dao-table-class))

(defun insert-tag (name)
  "Ensures a tag entity named NAME exists. Creates a row if it's not 
already in the database. Returns the ID corresponding to the tag."
  (handler-case (mito:insert-dao
                 (make-instance 'tag :name (string name)))
    (dbi:dbi-database-error ()
      (mito:find-dao 'tag :name (string name)))))

(defun insert-l-bookmark-tag (bookmark tag)
  (mito:insert-dao
   (make-instance 'l-bookmark-tag
                  :bookmark bookmark
                  :tag tag)))

(defun insert-bookmark (url description &optional tags)
  (let ((new-bookmark (mito:insert-dao
                       (make-instance 'bookmark :url url :description description))))
    (when tags
      (loop for name in tags
            for tag = (insert-tag name)
            do (insert-l-bookmark-tag new-bookmark tag))
      (mapcar #'insert-tag tags))))

(defun init-database ()
  (mito:connect-toplevel
   :sqlite3
   :database-name #P"/tmp/bookmarks.db"
   ; #P"~/.quicklisp/local-projects/bookmarks/bookmarks.db"
   )
  (mapcar #'mito:ensure-table-exists '(bookmark tag l-bookmark-tag)))

;;; Tests

;; (insert-tag "personal")
;; (insert-bookmark "http://stefanorodighiero.net" "My homepage"
;;                  '(personal homepage))

;;; Web server

;;; TBD
