;;;; bookmarks.lisp

(in-package #:bookmarks)

(defclass bookmark ()
  ((url :col-type :text
        :initarg :url
        :initform (error "You must provide a URL.")
        :accessor url)
   (description :col-type (or :null :text)
                :initarg :description
                :accessor description))
  (:metaclass mito:dao-table-class))

(defclass tag ()
  ((name :col-type :text
         :initarg :name
         :initform (error "You must provide a name for a tag.")
         :accessor name))
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

(defun insert-bookmark (description url &optional tags)
  (let ((new-bookmark (mito:insert-dao
                       (make-instance 'bookmark :url url :description description))))
    (when tags
      (loop for name in tags
            for tag = (insert-tag name)
            do (insert-l-bookmark-tag new-bookmark tag)))))

(defun init-database ()
  (mito:connect-toplevel
   :sqlite3
   :database-name #P"/tmp/bookmarks.db"
   ; #P"~/.quicklisp/local-projects/bookmarks/bookmarks.db"
   )
  (mapcar #'mito:ensure-table-exists '(bookmark tag l-bookmark-tag)))

(defun tags (bookmark)
  ;; TODO
  ;; select * from tag where id in (select tag_id from l_bookmark_tag where bookmark_id = 1)
  ;; select * from tag t join l_bookmark_tag l on l.tag_id = t.id where l.bookmark_id = 1
  (mito:select-dao 'tag
    (sxql:left-join :l_bookmark_tag :on (:= :l_bookmark_tag.tag_id :tag.id))
    (sxql:where (:l_bookmark_tag.bookmark_id 1))))

(defun bookmark-json (bookmark)
  (let ((tags '(foo bar
                baz rest)))
    (when bookmark
      `((url . ,(url bookmark))
        (description . ,(description bookmark))
        (tags . ,tags)))))

(defun tag-json (tag)
  (when tag
    `((name . ,(name tag)))))

;;; Tests

;; (insert-tag "personal")
;; (init-database)
;; (insert-bookmark "http://stefanorodighiero.net" "My homepage"
;;                  '(personal homepage))

;;; Web server

(defparameter *web* (make-instance '<app>))

(defvar *handler* nil)
  
(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (init-database)
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *web* args)))

(defun stop ()
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))

(defroute "/" ()
  (render-template* #P"index.tmpl"))

(defroute "/add" ()
  (render-template* #P"add.tmpl"))

(defroute ("/bookmark" :method :POST) (&key _parsed)
  (let ((bookmark (cdr (assoc "bookmark" _parsed :test #'string=))))
    (print bookmark)
    (insert-bookmark
     (second (assoc "description" bookmark))
     (second (assoc "url" bookmark))
     '(youtube data-engineering functional))))

(defroute "/bookmarks" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (setf (getf (response-headers *response*) :access-control-allow-origin) "*")
  (encode-json-to-string
   (mapcar #'bookmark-json (mito:retrieve-dao 'bookmark))))

(defroute "/tags" ()
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (setf (getf (response-headers *response*) :access-control-allow-origin) "*")  (encode-json-to-string
   (mapcar #'tag-json (mito:retrieve-dao 'tag))))

