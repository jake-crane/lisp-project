;;;; example.lisp - load from anywhere: (load "example/example.lisp")

(load (merge-pathnames "../dfile.lisp" *load-truename*))

(let ((dfile:*config-file* (merge-pathnames "dfile.cfg" *load-truename*))
      (records '((("id" . "1") ("name" . "Ada Lovelace") ("city" . "London"))
                 (("id" . "2") ("name" . "Grace Hopper") ("city" . "New York"))
                 ((:id . 3) (:name . "John McCarthy")))))   ; city left empty
  (dolist (name '("customers-csv" "customers-tab" "customers-bin"))
    (let ((df (dfile:lookup-dfile name)))
      (format t "~&~%~A: fields ~S, delimiter ~S, separator ~S, data ~A~%"
              (dfile:dfile-name df) (dfile:dfile-fields df)
              (dfile:dfile-field-delimiter df) (dfile:dfile-record-separator df)
              (dfile:dfile-data-file df))
      ;; Write, replacing any earlier run's output.
      (dfile:open-dfile df :direction :output :if-exists :supersede)
      (dolist (record records)
        (dfile:write-record df record))
      (dfile:close-dfile df)
      ;; Read it back.
      (dfile:with-dfile (in df :direction :input)
        (loop for record = (dfile:read-record in)
              while record
              do (format t "  ~S~%" record))))))
