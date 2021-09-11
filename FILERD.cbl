       IDENTIFICATION DIVISION.
       PROGRAM-ID. FILERD.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT POLICYFL ASSIGN TO 'D:/CBL_DEMOS/POLICY.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-POLICYFL-STATUS.

           SELECT OUTFL ASSIGN TO 'D:/CBL_DEMOS/OUTFL.TXT'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-OUTFL-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD  POLICYFL.
       01  POLICYFL-REC.
           05 POLICYNO             PIC 9(7).
           05 CUSTOMERCODE         PIC X(5).
           05 STARTDATE.
               10 MM               PIC 9(2).
               10 DD               PIC 9(2).
               10 YYYY             PIC 9(4).
           05 PREMIUM              PIC 9(5)V99.
           05 POLICYMODE           PIC X(1).
           05 LOB                  PIC X(1).
           05 FILLER               PIC X(51).

       FD  OUTFL.
       01  OUTFL-REC.
           05 POLICYNO             PIC 9(7).
           05 CUSTOMERCODE         PIC X(5).
           05 STARTDATE.
               10 MM               PIC 9(2).
               10 DD               PIC 9(2).
               10 YYYY             PIC 9(4).
           05 PREMIUM              PIC 9(5)V99.
           05 POLICYMODE               PIC X(1).
           05 LOB                      PIC X(1).
           05 FILLER                   PIC X(51).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05  WS-POLICYFL-STATUS      PIC X(2).
           05  WS-OUTFL-STATUS         PIC X(2).

       77  WS-PFL-EOF                  PIC X(1)        VALUE  'N'.

       01  WS-COUNTERS.
           05 WS-POLICYFL-RD-CNT       PIC 9(4)        VALUE ZEROES.
           05 WS-OUTFL-WT-CNT          PIC 9(4)        VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-POLICYFL-OPEN-PARA.
           IF WS-POLICYFL-STATUS = '00' THEN
               PERFORM 2000-POLICYFL-READ-PARA  UNTIL WS-PFL-EOF = 'Y'
               PERFORM 3000-POLICYFL-CLOSE-PARA
           ELSE
               DISPLAY 'FILE CANNOT BE OPENED.'
           END-IF.

           DISPLAY 'NO. OF RECORD READ: ', WS-POLICYFL-RD-CNT.
           DISPLAY 'NOT OF RECORDS WRITTERN:', WS-OUTFL-WT-CNT.
           STOP RUN.

       1000-POLICYFL-OPEN-PARA.
           OPEN INPUT POLICYFL.
           DISPLAY 'FILE STATUS:', WS-POLICYFL-STATUS.

           OPEN OUTPUT OUTFL.
           DISPLAY 'FILE STATUS:', WS-OUTFL-STATUS.

           2000-POLICYFL-READ-PARA.
               READ POLICYFL INTO POLICYFL-REC
                   AT END
                       MOVE 'Y' TO WS-PFL-EOF
                   NOT AT END
                       ADD 1 TO WS-POLICYFL-RD-CNT
                       DISPLAY POLICYFL-REC
                       IF LOB OF POLICYFL-REC = 'A' THEN
                           PERFORM 2500-OUTFL-WRITE-PARA
                       END-IF
               END-READ.

       2500-OUTFL-WRITE-PARA.
           MOVE CORRESPONDING POLICYFL-REC TO OUTFL-REC.
           WRITE OUTFL-REC.
           ADD 1 TO WS-OUTFL-WT-CNT.

       3000-POLICYFL-CLOSE-PARA.
           CLOSE POLICYFL.
           CLOSE OUTFL.
