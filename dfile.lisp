;;;; dfile.lisp
;;;;
;;;; Read and write ASCII data files described by a configuration file
;;;; named dfile.cfg.
;;;;
;;;; dfile.cfg format: one record per line, fields separated by #\:
;;;;
;;;;   name:field-delimiter:record-separator:record-layout-file:data-file
;;;;
;;;;   name              Identifies the record; passed to LOOKUP-DFILE.
;;;;   field-delimiter   A single ASCII character, or a whole number 0-255
;;;;                     whose one-byte binary value is the delimiter.
;;;;   record-separator  Same rules as field-delimiter (10 = newline).
;;;;   record-layout     Path to a file listing one field name per line,
;;;;                     in the order the fields appear in each record.
;;;;   data-file         Path to the data file.
;;;;
;;;; Notes on the delimiter fields:
;;;;   - A value made only of digits is always read as a number, so "5"
;;;;     means byte 5.  To use the character 5 itself, write 53.
;;;;   - Because #\: separates the fields of dfile.cfg, a colon delimiter
;;;;     must be written as 58.
;;;;   - If both are empty, the data file is length-prefixed: each field
;;;;     value is preceded by one byte (0-255) holding its length, and no
;;;;     delimiters or separators are written.
;;;;
;;;; Relative paths in dfile.cfg are resolved against the directory of
;;;; dfile.cfg.  The data-file field takes the rest of the line, so it
;;;; may itself contain colons.
;;;;
;;;; Records are association lists of (field-name . value), with field
;;;; names as strings in layout order and values as strings.
;;;;
;;;; Example:
;;;;
;;;;   (let ((df (dfile:lookup-dfile "customers")))
;;;;     (dfile:open-dfile df :direction :output)
;;;;     (dfile:write-record df '(("id" . "1") ("name" . "Ada")))
;;;;     (dfile:close-dfile df)
;;;;     (dfile:open-dfile df :direction :input)
;;;;     (loop for rec = (dfile:read-record df)
;;;;           while rec
;;;;           do (print rec))
;;;;     (dfile:close-dfile df))

(defpackage #:dfile
  (:use #:common-lisp)
  (:export
   ;; configuration
   #:*config-file*
   #:lookup-dfile
   ;; the dfile object
   #:dfile
   #:dfile-p
   #:dfile-name
   #:dfile-field-delimiter
   #:dfile-record-separator
   #:dfile-layout-file
   #:dfile-data-file
   #:dfile-fields
   #:dfile-direction
   #:dfile-length-prefixed-p
   #:dfile-open-p
   ;; file operations
   #:open-dfile
   #:read-record
   #:write-record
   #:close-dfile
   #:close-all-dfiles
   #:with-dfile
   ;; helpers
   #:field-value
   ;; conditions
   #:dfile-error))

(in-package #:dfile)

;;; ------------------------------------------------------------------
;;; Globals and conditions
;;; ------------------------------------------------------------------

(defvar *config-file* "dfile.cfg"
  "Pathname designator of the configuration file read by LOOKUP-DFILE.")

(defvar *open-dfiles* '()
  "List of DFILE objects whose data files are currently open.")

(defparameter +whitespace+ '(#\Space #\Tab #\Return #\Newline))

(define-condition dfile-error (simple-error) ()
  (:documentation "Signalled for configuration, layout and data errors."))

(defun dfile-error (control &rest args)
  (error 'dfile-error :format-control control :format-arguments args))

;;; ------------------------------------------------------------------
;;; The dfile object
;;; ------------------------------------------------------------------

(defstruct (dfile (:constructor %make-dfile))
  "Data file description from dfile.cfg plus its open stream, if any.
FIELD-DELIMITER and RECORD-SEPARATOR are byte values (0-255), or NIL for
a length-prefixed file."
  (name "" :type string)
  (field-delimiter nil)
  (record-separator nil)
  (layout-file nil)
  (data-file nil)
  (fields '() :type list)
  (stream nil)
  (direction nil))

(defmethod print-object ((object dfile) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "~S~@[ ~(~A~)~]" (dfile-name object) (dfile-direction object))))

(defun dfile-length-prefixed-p (dfile)
  "True when DFILE uses length-prefixed fields instead of delimiters."
  (null (dfile-field-delimiter dfile)))

(defun dfile-open-p (dfile)
  "True when DFILE's data file is currently open."
  (not (null (dfile-stream dfile))))

;;; ------------------------------------------------------------------
;;; Parsing helpers
;;; ------------------------------------------------------------------

(defun split-string (string delimiter)
  "Split STRING at every DELIMITER character.  Empty fields are kept."
  (loop with start = 0
        for pos = (position delimiter string :start start)
        collect (subseq string start pos)
        while pos
        do (setf start (1+ pos))))

(defun join-strings (strings separator)
  (with-output-to-string (out)
    (loop for (s . rest) on strings
          do (write-string s out)
             (when rest (write-char separator out)))))

(defun parse-delimiter-spec (spec what name)
  "Convert a delimiter field from dfile.cfg into a byte value, or NIL if
SPEC is empty.  WHAT and NAME are used in error messages."
  (cond ((zerop (length spec)) nil)
        ((every #'digit-char-p spec)
         (let ((n (parse-integer spec)))
           (unless (<= 0 n 255)
             (dfile-error "~A ~S for ~S is not between 0 and 255." what spec name))
           n))
        ((= (length spec) 1)
         (let ((code (char-code (char spec 0))))
           (unless (< code 256)
             (dfile-error "~A ~S for ~S is not a one-byte character." what spec name))
           code))
        (t
         (dfile-error "~A ~S for ~S must be a single character or a whole number."
                      what spec name))))

(defun resolve-path (path base)
  "Resolve PATH relative to the directory of BASE."
  (merge-pathnames (pathname path)
                   (make-pathname :name nil :type nil :version nil
                                  :defaults (pathname base))))

(defun find-config-entry (name config-file)
  "Return the list of fields of the dfile.cfg line whose name is NAME,
or NIL if there is none."
  (with-open-file (in config-file :direction :input :if-does-not-exist nil)
    (unless in
      (dfile-error "Configuration file ~S does not exist." config-file))
    (loop for line = (read-line in nil nil)
          for line-number from 1
          while line
          do (let* ((line (string-right-trim '(#\Return) line))
                    (fields (split-string line #\:)))
               (when (string= (string-trim +whitespace+ (first fields)) name)
                 (when (< (length fields) 5)
                   (dfile-error "~A line ~D: expected 5 fields for ~S, found ~D."
                                config-file line-number name (length fields)))
                 ;; The data file path takes the remainder of the line.
                 (return (append (subseq fields 0 4)
                                 (list (join-strings (nthcdr 4 fields) #\:)))))))))

(defun read-layout-file (path)
  "Return the field names listed in the record layout file PATH."
  (with-open-file (in path :direction :input :if-does-not-exist nil)
    (unless in
      (dfile-error "Record layout file ~S does not exist." path))
    (let ((fields (loop for line = (read-line in nil nil)
                        while line
                        when (plusp (length (string-trim +whitespace+ line)))
                          collect (string-trim +whitespace+ line))))
      (when (null fields)
        (dfile-error "Record layout file ~S lists no fields." path))
      (loop for (field . rest) on fields
            when (member field rest :test #'string=)
              do (dfile-error "Record layout file ~S lists field ~S more than once."
                              path field))
      fields)))

;;; ------------------------------------------------------------------
;;; Lookup
;;; ------------------------------------------------------------------

(defun lookup-dfile (name &key (config-file *config-file*))
  "Look up NAME in CONFIG-FILE (default dfile.cfg), read its record
layout file, and return a DFILE object describing the data file.
NAME is a string designator compared case-sensitively."
  (let* ((name (string-trim +whitespace+ (string name)))
         (entry (or (find-config-entry name config-file)
                    (dfile-error "No entry named ~S in ~S." name config-file))))
    (destructuring-bind (cfg-name fd-spec rs-spec layout-spec data-spec) entry
      (declare (ignore cfg-name))
      (let ((fd (parse-delimiter-spec fd-spec "Field delimiter" name))
            (rs (parse-delimiter-spec rs-spec "Record separator" name))
            (layout-spec (string-trim +whitespace+ layout-spec))
            (data-spec (string-trim +whitespace+ data-spec)))
        (when (or (and fd (null rs)) (and rs (null fd)))
          (dfile-error "~S: field delimiter and record separator must both be ~
                        given or both be empty." name))
        (when (and fd (= fd rs))
          (dfile-error "~S: field delimiter and record separator are the same." name))
        (when (zerop (length layout-spec))
          (dfile-error "~S: no record layout file given." name))
        (when (zerop (length data-spec))
          (dfile-error "~S: no data file given." name))
        (let ((layout-file (resolve-path layout-spec config-file)))
          (%make-dfile :name name
                       :field-delimiter fd
                       :record-separator rs
                       :layout-file layout-file
                       :data-file (resolve-path data-spec config-file)
                       :fields (read-layout-file layout-file)))))))

(defun ensure-dfile (designator config-file)
  (if (dfile-p designator)
      designator
      (lookup-dfile designator :config-file config-file)))

;;; ------------------------------------------------------------------
;;; Opening and closing
;;; ------------------------------------------------------------------

(defun open-dfile (dfile &key (direction :input)
                              (if-exists :append)
                              (if-does-not-exist nil if-does-not-exist-p)
                              (config-file *config-file*))
  "Open the data file of DFILE for reading (:INPUT) or writing (:OUTPUT).
DFILE may be a DFILE object or a name to look up in CONFIG-FILE.
For :OUTPUT, IF-EXISTS defaults to :APPEND (use :SUPERSEDE to replace
the file) and a missing file is created.  Returns the DFILE object."
  (let ((dfile (ensure-dfile dfile config-file)))
    (when (dfile-open-p dfile)
      (dfile-error "~S is already open for ~(~A~)." (dfile-name dfile)
                   (dfile-direction dfile)))
    (let ((stream
            (ecase direction
              (:input
               (open (dfile-data-file dfile)
                     :direction :input
                     :element-type '(unsigned-byte 8)
                     :if-does-not-exist (if if-does-not-exist-p
                                            if-does-not-exist
                                            :error)))
              (:output
               (open (dfile-data-file dfile)
                     :direction :output
                     :element-type '(unsigned-byte 8)
                     :if-exists if-exists
                     :if-does-not-exist (if if-does-not-exist-p
                                            if-does-not-exist
                                            :create))))))
      (unless stream
        (dfile-error "Could not open data file ~S." (dfile-data-file dfile)))
      (setf (dfile-stream dfile) stream
            (dfile-direction dfile) direction)
      (push dfile *open-dfiles*)
      dfile)))

(defun close-dfile (dfile)
  "Close the data file of DFILE.  Returns T if it was open, NIL otherwise."
  (let ((stream (dfile-stream dfile)))
    (setf *open-dfiles* (remove dfile *open-dfiles*))
    (when stream
      (setf (dfile-stream dfile) nil
            (dfile-direction dfile) nil)
      (close stream)
      t)))

(defun close-all-dfiles ()
  "Close every data file opened with OPEN-DFILE that is still open."
  (dolist (dfile (copy-list *open-dfiles*))
    (close-dfile dfile))
  (values))

(defmacro with-dfile ((var dfile &rest open-args) &body body)
  "Bind VAR to DFILE (an object or a name) opened with OPEN-ARGS, run
BODY, and close the data file afterwards, even on a non-local exit."
  `(let ((,var (open-dfile ,dfile ,@open-args)))
     (unwind-protect (progn ,@body)
       (close-dfile ,var))))

(defun check-open (dfile direction)
  (unless (eq (dfile-direction dfile) direction)
    (dfile-error "~S is not open for ~(~A~)." (dfile-name dfile) direction)))

;;; ------------------------------------------------------------------
;;; Reading
;;; ------------------------------------------------------------------

(defun read-ascii-bytes (stream count dfile)
  (let ((bytes (make-array count :element-type '(unsigned-byte 8))))
    (when (< (read-sequence bytes stream) count)
      (dfile-error "~S: unexpected end of file inside a record." (dfile-name dfile)))
    (map 'string #'code-char bytes)))

(defun read-delimited-values (dfile)
  "Read one record's field values, or :EOF at end of file."
  (let ((stream (dfile-stream dfile))
        (fd (dfile-field-delimiter dfile))
        (rs (dfile-record-separator dfile))
        (field (make-string-output-stream))
        (values '())
        (started nil))
    (loop for byte = (read-byte stream nil nil)
          do (cond ((null byte)
                    (unless started
                      (return-from read-delimited-values :eof))
                    (return))
                   ((= byte rs)
                    (return))
                   ((= byte fd)
                    (push (get-output-stream-string field) values))
                   (t
                    (write-char (code-char byte) field)))
             (setf started t))
    (push (get-output-stream-string field) values)
    (nreverse values)))

(defun read-prefixed-values (dfile)
  "Read one length-prefixed record's field values, or :EOF at end of file."
  (let ((stream (dfile-stream dfile)))
    (loop for field in (dfile-fields dfile)
          for first = t then nil
          for length = (read-byte stream nil nil)
          when (null length)
            do (if first
                   (return-from read-prefixed-values :eof)
                   (dfile-error "~S: unexpected end of file before field ~S."
                                (dfile-name dfile) field))
          collect (read-ascii-bytes stream length dfile))))

(defun read-record (dfile &optional (eof-error-p nil) (eof-value nil))
  "Read the next record from DFILE and return it as an association list
of (field-name . value) in layout order.  At end of file, signal an
error if EOF-ERROR-P is true, otherwise return EOF-VALUE.
Missing trailing fields in a delimited record are returned as \"\"."
  (check-open dfile :input)
  (let ((values (if (dfile-length-prefixed-p dfile)
                    (read-prefixed-values dfile)
                    (read-delimited-values dfile)))
        (fields (dfile-fields dfile)))
    (when (eq values :eof)
      (if eof-error-p
          (error 'end-of-file :stream (dfile-stream dfile))
          (return-from read-record eof-value)))
    (when (> (length values) (length fields))
      (dfile-error "~S: record has ~D fields but the layout defines ~D."
                   (dfile-name dfile) (length values) (length fields)))
    (loop for field in fields
          for rest = values then (cdr rest)
          collect (cons field (if rest (car rest) "")))))

;;; ------------------------------------------------------------------
;;; Writing
;;; ------------------------------------------------------------------

(defun field-value (field record)
  "Return the value of FIELD in the association list RECORD.  Keys may
be strings or symbols; an exact match is preferred, otherwise names are
compared case-insensitively (so :NAME finds \"name\")."
  (let ((entry (or (assoc field record :test #'string=)
                   (assoc field record :test #'string-equal))))
    (values (cdr entry) (not (null entry)))))

(defun value-bytes (value field dfile)
  "Convert VALUE to a vector of bytes.  NIL becomes the empty string and
non-strings are written with PRINC."
  (let ((string (cond ((null value) "")
                      ((stringp value) value)
                      (t (princ-to-string value)))))
    (map '(vector (unsigned-byte 8))
         (lambda (char)
           (let ((code (char-code char)))
             (unless (< code 256)
               (dfile-error "~S: field ~S contains non-ASCII character ~S."
                            (dfile-name dfile) field char))
             code))
         string)))

(defun write-record (dfile record)
  "Write RECORD, an association list of (field-name . value), to DFILE
in layout order.  Fields absent from RECORD are written empty; keys not
in the layout are ignored.  Returns RECORD."
  (check-open dfile :output)
  (let ((stream (dfile-stream dfile))
        (fd (dfile-field-delimiter dfile))
        (rs (dfile-record-separator dfile))
        (encoded (loop for field in (dfile-fields dfile)
                       collect (cons field (value-bytes (field-value field record)
                                                        field dfile)))))
    ;; Validate the whole record before writing any of it.
    (loop for (field . bytes) in encoded
          do (if (dfile-length-prefixed-p dfile)
                 (when (> (length bytes) 255)
                   (dfile-error "~S: field ~S is ~D bytes long; the maximum is 255."
                                (dfile-name dfile) field (length bytes)))
                 (when (or (find fd bytes) (find rs bytes))
                   (dfile-error "~S: field ~S contains the field delimiter or ~
                                 record separator."
                                (dfile-name dfile) field))))
    (if (dfile-length-prefixed-p dfile)
        (loop for (nil . bytes) in encoded
              do (write-byte (length bytes) stream)
                 (write-sequence bytes stream))
        (progn
          (loop for ((nil . bytes) . rest) on encoded
                do (write-sequence bytes stream)
                   (when rest (write-byte fd stream)))
          (write-byte rs stream)))
    record))
