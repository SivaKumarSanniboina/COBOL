       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARR1.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  WS-ARRAY.
           05 ELEMENT          PIC  9(2) OCCURS 5 TIMES.

       77  IDX                 PIC  S9(4) COMP.

       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-READ-ARRAY-PARA VARYING IDX FROM 1 BY 1
                                   UNTIL IDX > 5.
           PERFORM VARYING IDX FROM 1 BY 1 UNTIL IDX > 5
               DISPLAY ELEMENT(IDX)
           END-PERFORM.
           STOP RUN.
       1000-READ-ARRAY-PARA.
           ACCEPT ELEMENT(IDX).
