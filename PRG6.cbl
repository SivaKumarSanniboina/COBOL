       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG6.
       DATA DIVISION.
       FILE SECTION.
       WORKING-STORAGE SECTION.
       77  BASIC_PAY PIC 9(5)V99.
       77  DA        PIC 9(2)V99.
       77  HRA       PIC 9(5)V99.
       77  SALARY PIC   9(5)V99.

       PROCEDURE DIVISION.
       MAIN-PROCEDURE.
           DISPLAY 'ENETR BASIC_PAY'.
           ACCEPT BASIC_PAY.
           DISPLAY 'ENETR HRA'.
           ACCEPT HRA.
           COMPUTE DA=BASIC_PAY/2.
           COMPUTE SALARY=BASIC_PAY+DA+HRA.
           DISPLAY "SALARY:"SALARY.
           STOP RUN.
