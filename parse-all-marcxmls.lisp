#!/usr/local/bin/lispscript


(use-xml-namespace  "http://www.loc.gov/MARC21/slim")


(load "xwalks.lisp")



(defmacro with-get-all (someval listofunctions &body body)
  (let ((dalist (mapcar (lambda (x) `(,x (,x ,someval))) listofunctions)))
    `(let ,dalist
       (let ((everything! (list ,@listofunctions)))
         ,@body))))


(defmacro defmarcxmlfield (thename theplain thegame &rest therest)
  (with-gensyms (tmp cc tmp2)
    `(let ((,cc (xpath-compile ,theplain)))
       (defun ,thename (adoc)
         (let ((it!
                 « (xpath adoc ,cc ,@therest :all nil :text t :compiled-p t)
                      or do (return-from ,thename nil) »))
           #? ,thegame)))))


; --------------------------------------------------------------- ;

(defmarcxmlfield leader         "leader" it!)
(defmarcxmlfield oh08       		"controlfield[@tag='008']" it!)
(defmarcxmlfield barcode    		"datafield[@tag='876']/subfield[@code='p']" it!)
(defmarcxmlfield title      		"datafield[@tag='245']/subfield[@code='a']" it!)
(defmarcxmlfield scsbid     		"controlfield[@tag='001']" it!)
(defmarcxmlfield author     		"datafield[@tag='100']/subfield[@code='a']" it!)
(defmarcxmlfield lccall1    		"datafield[@tag='050']/subfield[@code='a']" it!)
(defmarcxmlfield lccall2    		"datafield[@tag='090']/subfield[@code='a']" it!)
(defmarcxmlfield localcallnum   "datafield[@tag='852']/subfield[@code='h']" it!)
(defmarcxmlfield sharedp        "datafield[@tag='876']/subfield[@code='x']" it!)

(defmarcxmlfield lccn
  "datafield[@tag='010']/subfield[@code='a']"
  (parse-integer (~ra it! •\D• "")))

(defmarcxmlfield isbn
  "datafield[@tag='020']/subfield[@code='a']"
  (~ra it! •[^\dXx].*• ""))

(defmarcxmlfield issn
  "datafield[@tag='022']/subfield[@code='a']"
  (~ra it! •[^\dXx].*• ""))

(defmarcxmlfield oclc
  "datafield[@tag='035']/subfield[@code='a']"
  (let ((res (remove-if-not (lambda (x) (~m x •^.OCoLC.•)) it!)))
    (when res (-<> res
                   (mapcar (lambda (x) (~r x •^\D+• "")) <>)
                   (remove-if-not (lambda (x) (~m x •^\d+$•)) <>)
                   (mapcar #'parse-integer <>)
                   (remove-duplicates <> :test #'eql)
                   (delim <> :sep #\;))))
  :text t :all t)

(defun lccall (something) (aif (lccall1 something) it! (lccall2 something)))

;;; repeated :(
(defmarcxmlfield language
  "controlfield[@tag='008']"
  (subseq it! 35 38))

(defmarcxmlfield pubdate
  "controlfield[@tag='008']"
  (parse-integer (subseq it! 7 11)))

(defmarcxmlfield recordtype
  "leader"
  (-<> (subseq it! 6 7)
       (string-upcase <>)
       (find-symbol <>)
       {+record-type-xwalk+ <>}))

(defmarcxmlfield biblevel
  "leader"
  (-<> (subseq it! 7 8)
       (string-upcase <>)
       (find-symbol <>)
       {+bib-level-xwalk+ <>}))

(defmarcxmlfield pubplace
  "controlfield[@tag='008']"
  (subseq it! 15 18))


; --------------------------------------------------------------- ;


« (defvar /thedir/ Ø (cadr (cmdargs)))
    or die "no directory of xml files given" »

(defvar /allfiles/ (zsh (fn "find ~A -maxdepth 1 -type f | ack xml" /thedir/) :split t))
(defvar /intotal/ (length /allfiles/))


(ft "~A~%" (delim (list "barcode" "scsbid" "sharedp" "language"
                        "pubdate" "biblevel" "recordtype" "pubplace"
                        "oclc" "lccn" "isbn" "issn" "lccall" "localcallnum"
                        "leader" "oh08" "title" "author")))

(defparameter /doc/ nil)

(for-each-list /allfiles/
  (progress index! /intotal/)
  (info "parsing file ~A~%" value!)
  (with-time
    (setq /doc/ (xml-parse-file value!))
    (info "parsing took ~A~%" (time-for-humans time!)))
  (with-time
    (for-each-list (xpath /doc/ "/collection/record")
      (let ((barcodes (xpath value! "datafield[@tag='876']/subfield[@code='p']" :all t :text t)))
      (with-get-all value!
        (scsbid sharedp language pubdate biblevel recordtype pubplace
         oclc lccn isbn issn lccall localcallnum leader oh08 title author)
        (for-each-list barcodes
          (ft "~A~C" value! #\Tab)
          (ft "~A~%" (delim everything!))))))
    (info "finished conversion in ~A~%~%" (time-for-humans time!)))
  (gc :full t))


