;;;; copy-personal.lisp
;;;;
;;;; Read every record of the data file named "personal" in dfile.cfg and
;;;; write its values to the data files named "personal2" and "personal3".
;;;;
;;;; This file has no DEFPACKAGE or IN-PACKAGE form; its definitions go
;;;; into whatever package is current when it is loaded (normally
;;;; CL-USER), and it reaches the library only through DFILE-LIB:
;;;; qualified symbols.
;;;;
;;;; Values are copied by field name, so the three files may use different
;;;; formats (delimited or length-prefixed) and field orders.  A field in
;;;; an output layout that personal does not have is written empty; a
;;;; personal field that an output layout lacks is dropped.  The output
;;;; data files are replaced on each run.
;;;;
;;;; Usage, from the directory containing dfile.cfg:
;;;;   sbcl --script copy-personal.lisp
;;;; or, in a running Lisp:
;;;;   (load "copy-personal.lisp")

;; Load the library from this file's directory if it isn't loaded yet.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package "DFILE-LIB")
    (load (merge-pathnames "dfile.lisp" (or *compile-file-truename*
                                            *load-truename*)))))

(defun copy-dfile-records (source-name target-names)
  "Copy every record of the data file SOURCE-NAME to each data file in
TARGET-NAMES, all names as defined in dfile.cfg.  Returns the number of
records copied."
  (let* ((source (dfile-lib:lookup-dfile source-name))
         (source-path (probe-file (dfile-lib:dfile-data-file source)))
         (targets (mapcar #'dfile-lib:lookup-dfile target-names)))
    ;; Superseding the source would destroy it before it is read.
    (dolist (target targets)
      (when (and source-path
                 (equal source-path (probe-file (dfile-lib:dfile-data-file target))))
        (error "~A and ~A use the same data file ~A."
               (dfile-lib:dfile-name source) (dfile-lib:dfile-name target)
               source-path)))
    (unwind-protect
         (progn
           (dfile-lib:open-dfile source :direction :input)
           (dolist (target targets)
             (dfile-lib:open-dfile target :direction :output :if-exists :supersede))
           (loop for record = (dfile-lib:read-record source)
                 while record
                 do (dolist (target targets)
                      (dfile-lib:write-record target record))
                 count t))
      (dfile-lib:close-dfile source)
      (dolist (target targets)
        (dfile-lib:close-dfile target)))))

(format t "~&Copied ~D records from personal to personal2 and personal3.~%"
        (copy-dfile-records "personal" '("personal2" "personal3")))
