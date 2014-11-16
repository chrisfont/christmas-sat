;;;; chris-sat.lisp

(in-package #:chris-sat)

;;; "chris-sat" goes here. Hacks and glory await!

(defvar *people* nil)

(defstruct person
  (name      nil :type (or symbol keyword))
  (match     nil :type (or null symbol keyword))
  (available nil :type (or null list)))

(defun add-person (name available &optional (match nil))
  (let* ((sym-name (if (stringp name)
                       (intern (string-downcase name))
                       name))
         (new-person (make-person
                      :name      sym-name
                      :match     match
                      :available available)))
    (push new-person *people*)))

(defun rem-person (name)
  (loop for p in *people*
       do (when (equal name (person-name p))
            (setf *people*
              (remove p *people*)))))

(defun match-people ()
  "Loop through the available people list, selecting a person, randomly choosing an available match, then marking that match not available to the rest of the group."
  (loop for p in *people*
       do (when (null (person-match p))
            (let* ((index (random (length (person-available p))))
                   (match (nth index (person-available p))))
              (setf (person-available p) nil)
              ;; Deselect match for everyone else.
              (loop for other in *people*
                   do (when (not (null (person-available other)))
                        (setf (person-available other)
                              (remove match (person-available other)))))
              (setf (person-match p) match)))))

(defun print-matches ()
  "Simple match print."
  (loop for p in *people*
       do (format t "~A is giving ~A a gift this year!~%"
                  (person-name p)
                  (person-match p))))
