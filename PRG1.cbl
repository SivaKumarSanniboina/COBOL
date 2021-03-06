       IDENTIFICATION DIVISION.
       PROGRAM-ID. PRG1.
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  NUM1    PIC  9(3)V99.
       77  NUM2    PIC  9(3)V99.
       77  NUM3    PIC  9(3)V99.
       77  TOTAL   PIC  9(5)V99.
       77  AVERAGE   PIC  9(5)V99.
       77  FORMATTED-TOTAL   PIC  ZZZZ9.99.
       77  FORMATTED-AVERAGE   PIC  ZZZZ9.99.
       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           MOVE 70 TO NUM1.
           MOVE 70 TO NUM2.
           MOVE 70 TO NUM3.
           COMPUTE TOTAL = NUM1 + NUM2 + NUM3.
           COMPUTE AVERAGE = TOTAL/3.

           MOVE TOTAL TO FORMATTED-TOTAL.
           DISPLAY 'TOTAL:', TOTAL.
           DISPLAY 'FORMATTED-TOTAL: ', FORMATTED-TOTAL.
           MOVE AVERAGE TO FORMATTED-AVERAGE.
           DISPLAY 'AVERAGE:', AVERAGE.
           DISPLAY 'FORMATTED-AVERAGE: ', FORMATTED-AVERAGE.
           STOP RUN.
