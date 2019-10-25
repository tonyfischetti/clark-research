#!/usr/local/bin/lispscript




; (DECLAIM (OPTIMIZE (SPEED 3)
;                    (SAFETY 0)))


; (DEFVAR many "~/data/CUL-marc/f001.mrc")
(DEFVAR MANY "~/data/CUL-marc/Columbia-extract-20190831-001.mrc")


(DECLAIM (INLINE ASCII-BYTES->STRING))
(DEFUN ASCII-BYTES->STRING (BYTES)
  (MAP 'STRING (LAMBDA (X) (CODE-CHAR X)) BYTES))


(DECLAIM (INLINE PEEK-OCTET))
(DEFUN PEEK-OCTET (ASTREAM)
  (LET ((CURPOS (FILE-POSITION ASTREAM))
        (RET (READ-BYTE ASTREAM)))
    (FILE-POSITION ASTREAM CURPOS)
    RET))

(DECLAIM (INLINE PEEK-OCTETS))
(DEFUN PEEK-OCTETS (ASTREAM num)
  (LET ((CURPOS (FILE-POSITION ASTREAM))
        (RET    NIL))
    (setq ret (loop repeat num collect (read-byte astream)))
    (FILE-POSITION ASTREAM CURPOS)
    RET))



(DEFCONSTANT +RECORD-TYPE-XWALK+
  (LIST (CONS 'A "Language material")
        (CONS 'C "Notated music")
        (CONS 'D "Manuscript notated music")
        (CONS 'E "Cartographic material")
        (CONS 'F "Manuscript cartographic material")
        (CONS 'G "Projected medium")
        (CONS 'I "Nonmusical sound recording")
        (CONS 'J "Musical sound recording")
        (CONS 'K "Two-dimensional nonprojectable graphic")
        (CONS 'M "Computer file")
        (CONS 'O "Kit")
        (CONS 'P "Mixed materials")
        (CONS 'R "Three-dimensional artifact or naturally occurring object")
        (CONS 'T "Manuscript language material")))

(DEFCONSTANT +BIB-LEVEL-XWALK+
  (LIST (CONS 'A "Monographic component part")
        (CONS 'B "Serial component part")
        (CONS 'C "Collection")
        (CONS 'D "Subunit")
        (CONS 'I "Integrating resource")
        (CONS 'M "Monograph/Item")
        (CONS 'S "Serial")))



(DEFUN YIELD-RECORD (ASTREAM)
  (LET* ((LEADER              (MAKE-ARRAY '(24) :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
         (RECORD-LENGTH       NIL)
         (RECORD-STATUS       NIL)
         (RECORD-TYPE         NIL)
         (BIB-LEVEL           NIL)
         (WHOLE               NIL))
    (WHEN (< (READ-SEQUENCE LEADER ASTREAM) 24)
      (RETURN-FROM YIELD-RECORD NIL))
    (SETQ RECORD-LENGTH (-<> LEADER
                             (SUBSEQ <> 0 5)
                             (ASCII-BYTES->STRING <>)
                             (PARSE-INTEGER <>)))
    (SETQ WHOLE (MAKE-ARRAY `(,(- RECORD-LENGTH 24)) :ELEMENT-TYPE '(UNSIGNED-BYTE 8)))
    ;; INCOMPLETE RECORD
    ;; INCOMPLETE RECORD
    (WHEN (< (READ-SEQUENCE WHOLE ASTREAM) (- RECORD-LENGTH 24))
      (RETURN-FROM YIELD-RECORD NIL))
    (SETQ WHOLE (CONCATENATE 'VECTOR LEADER WHOLE))
    (SETQ LEADER (ASCII-BYTES->STRING LEADER))
    (SETQ RECORD-STATUS (ASCII-BYTES->STRING (SUBSEQ WHOLE 5 6)))
    (SETQ RECORD-TYPE (ASCII-BYTES->STRING (SUBSEQ WHOLE 6 7)))
    (SETQ RECORD-TYPE {+RECORD-TYPE-XWALK+ (FIND-SYMBOL (STRING-UPCASE RECORD-TYPE))})
    (SETQ BIB-LEVEL (ASCII-BYTES->STRING (SUBSEQ WHOLE 7 8)))
    (SETQ BIB-LEVEL {+BIB-LEVEL-XWALK+ (FIND-SYMBOL (STRING-UPCASE BIB-LEVEL))})
    (LIST (CONS :LEADER LEADER)
          (CONS :RECORD-LENGTH RECORD-LENGTH)
          (CONS :RECORD-STATUS RECORD-STATUS)
          (CONS :RECORD-TYPE RECORD-TYPE)
          (CONS :BIB-LEVEL BIB-LEVEL)
          ; (CONS :WHOLE (ASCII-BYTES->STRING WHOLE))
          )))



; temporary
(DECLAIM (INLINE PRETTY))
(DEFUN PRETTY (ANALIST)
  (FOR-EACH-ALIST ANALIST
    (FT "~30A: ~A~%" KEY! VALUE!)))



(WITH-A-FILE MANY :B
  (FOR-EACH-YIELD (YIELD-RECORD STREAM!)
    ; (WHEN (> INDEX! 10) (BREAK!))
    (FT "~A~C~A~%" index! #\Tab {value! :leader})
    ; (PRETTY VALUE!)
    ; (FT "~%")
    ))



