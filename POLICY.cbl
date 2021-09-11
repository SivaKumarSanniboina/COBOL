       IDENTIFICATION DIVISION.
       PROGRAM-ID. POLICY.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICYFYL ASSIGN TO
           'C:/Users/HP PC/COBOL TEST/POLICYFILE.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-POLICYFYL-STATUS.

           SELECT OUTFL ASSIGN TO
           'C:/Users/HP PC/COBOL TEST/OUTPUTFILE.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-OUTFL-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD  POLICYFYL.
        01 POLICY_REC.

           05  POLICYNO        PIC     X(08).

           05  CUSTOMERID      PIC     X(08).

           05  LOBCODE         PIC     X(01).

           05  START-DATE      PIC     X(08).

           05  END-DATE        PIC     X(08).

           05  PREMIUM         PIC     X(10).

           05  FILLER          PIC     X(60).
       FD  OUTFL.
       01  OUTFL-REC.
           05  POLICYNO        PIC     X(08).

           05  CUSTOMERID      PIC     X(08).

           05  LOBCODE         PIC     X(01).

           05  START-DATE      PIC     X(08).

           05  END-DATE        PIC     X(08).

           05  PREMIUM         PIC     X(10).

           05  FILLER          PIC     X(60).
       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-POLICYFYL-STATUS        PIC X(2).
           05  WS-OUTFL-STATUS         PIC X(2).

       88  EOF                         VALUE 'Y'.
       88  NOT-EOF                     VALUE 'N'.


       01  WS-COUNTERS.
           05 WS-POLICYFYL-RD-CNT       PIC 9(4)        VALUE ZEROES.
           05 WS-OUTFL-WT-CNT          PIC 9(4)        VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-POLICYFYL-OPEN-PARA.
           IF WS-POLICYFYL-STATUS = '00' THEN
               PERFORM 2000-POLICYFYL-READ-PARA  UNTIL EOF
                   PERFORM 2500-OUTFL-WRITE-PARA
               PERFORM 3000-POLICYFYL-CLOSE-PARA
           ELSE
               DISPLAY 'FILE CANNOT BE OPENED.'
           END-IF.

           DISPLAY 'NO. OF RECORD READ: ', WS-POLICYFYL-RD-CNT.
           DISPLAY 'NOT OF RECORDS WRITTERN:', WS-OUTFL-WT-CNT.
           STOP RUN.

       1000-POLICYFYL-OPEN-PARA.
           OPEN INPUT POLICYFYL.
           DISPLAY 'FILE STATUS FOR POLICYFYL:', WS-POLICYFYL-STATUS.

           OPEN OUTPUT OUTFL.
           DISPLAY 'FILE STATUS FOR OUTFL:', WS-OUTFL-STATUS.
       2000-POLICYFYL-READ-PARA.
           READ POLICYFYL
               AT END
                   SET EOF TO TRUE
               NOT AT END
               ADD 1 TO WS-POLICYFYL-RD-CNT
                  IF POLICYNO OF POLICY_REC='S00000003' AND LOBCODE OF
                      POLICY_REC='S'
                      THEN
                      PERFORM 3000-POLICYFYL-CLOSE-PARA
                      ELSE
                          PERFORM 2500-OUTFL-WRITE-PARA
                  END-IF
           END-READ.



       2500-OUTFL-WRITE-PARA.

           WRITE OUTFL-REC.

           ADD 1 TO WS-OUTFL-WT-CNT.



       3000-POLICYFYL-CLOSE-PARA.
           CLOSE POLICYFYL.
           CLOSE OUTFL.
