       IDENTIFICATION DIVISION.
       PROGRAM-ID.  EMPDETAILS.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPFY ASSIGN TO 'C:/Users/HP PC/EMPLOYEEDETAILS.txt'
           ORGANIZATION IS LINE SEQUENTIAL
           FILE STATUS WS-EMPFY-STATUS.


       DATA DIVISION.
       FILE SECTION.
       FD  EMPFY.
       01  EMP_REC.
               05    Employee_NUMBER    PIC 9(5).
               05    EMPLOYEE_NAME      PIC X(5).
               05    SALARY             PIC 9(5).
               05    DESIGNATION        PIC X(10).
               05    DEPARTMENT         PIC X(17).
               05    FILLER             PIC X(42).

       WORKING-STORAGE SECTION.
       01  WS-FILE-STATUS.
           05 WS-EMPFY-STATUS          PIC X(2).

       77  WS-EMPL-EOF                 PIC X(2).
       01  WS-COUNTER.
           05  WS-EMPFY-RD-CNT         PIC 9(4)   VALUE ZERO.
       77  TOTAL                       PIC 9(5)V99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-EMPFY-OPEN-PARA.

           IF WS-EMPFY-STATUS = '00' THEN
                PERFORM 2000-EMPFY-READ-PARA UNTIL WS-EMPL-EOF='Y'
                PERFORM 3000-EMPFY-CLOSE-PARA
           ELSE
               DISPLAY 'FILE CANNOT BE OPENED.'
           END-IF.
           DISPLAY'TOTAL:', TOTAL.
           DISPLAY 'NO.OF RECORDS READ: ',WS-EMPFY-RD-CNT.
           STOP RUN.
       1000-EMPFY-OPEN-PARA.
           OPEN INPUT EMPFY.

           DISPLAY'FILE STATUS:', WS-EMPFY-STATUS.
       2000-EMPFY-READ-PARA.
           READ EMPFY INTO EMP_REC
                AT END
                MOVE 'Y' TO WS-EMPL-EOF
                 NOT AT END
                 ADD 1 TO WS-EMPFY-RD-CNT
                 COMPUTE TOTAL = TOTAL + SALARY
                 DISPLAY EMP_REC
           END-READ.

       3000-EMPFY-CLOSE-PARA.
           CLOSE EMPFY.
