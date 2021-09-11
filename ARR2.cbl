       IDENTIFICATION DIVISION.
       PROGRAM-ID. ARR2.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  PROD-ARRAY.
           05 PROD-REC OCCURS 3 TIMES ASCENDING KEY IS PRODUCT-CODE
                                               INDEXED BY I.
               10 PRODUCT-CODE    PIC  X(3).
               10 PRODUCT-NAME    PIC  X(15).
               10 UNITPRICE       PIC  9(5)V99.

       77  PRICE                  PIC  9(5)V99      VALUE ZEROES.
       77  PCODE                   PIC X(3).
       77  PNAME                   PIC X(15).

       77  PRODUCT-FOUND           PIC  X(1) VALUE 'N'.
       88  FOUND                                   VALUE 'Y'.
       88  NOT-FOUND                               VALUE 'N'.

       77  IDX                    PIC  S9(4) COMP.

       PROCEDURE DIVISION.
       0000-MAIN-PARA.
           PERFORM 1000-READ-PRODUCT-PARA VARYING IDX FROM 1 BY 1
                                   UNTIL IDX > 3.
           PERFORM 2000-COSTILIEST-PRODUCT-PARA VARYING IDX FROM 1 BY 1
                                   UNTIL IDX > 3.
           DISPLAY PCODE, PNAME, PRICE

           DISPLAY 'ENTER PRODUCT CODE TO SEARCH'.
           ACCEPT PCODE.
           PERFORM 3000-SEARCH-PRODUCT-PARA VARYING IDX FROM 1 BY 1
                                   UNTIL IDX > 3.
           IF NOT-FOUND THEN
               DISPLAY PCODE, ' DOSEST EXIST'
           END-IF.
           PERFORM 4000-SEARCH-PRODUCT-PARA.

           STOP RUN.
       1000-READ-PRODUCT-PARA.
           DISPLAY 'INPUT ELEMENT - ', IDX.
           ACCEPT PRODUCT-CODE(IDX).
           ACCEPT PRODUCT-NAME(IDX).
           ACCEPT UNITPRICE(IDX).

       2000-COSTILIEST-PRODUCT-PARA.
           IF UNITPRICE(IDX) > PRICE THEN
               MOVE UNITPRICE(IDX) TO PRICE
               MOVE PRODUCT-CODE(IDX) TO PCODE
               MOVE PRODUCT-NAME(IDX) TO PNAME
           END-IF.
       3000-SEARCH-PRODUCT-PARA.
           IF PRODUCT-CODE(IDX) = PCODE THEN
               DISPLAY 'FOUND:', PRODUCT-NAME(IDX)
               SET FOUND TO TRUE
           END-IF.
       4000-SEARCH-PRODUCT-PARA.
           SET I TO 1.
           SEARCH ALL PROD-REC
               AT END
                   SET NOT-FOUND TO TRUE
                   DISPLAY PCODE, 'DOESN;T EXIST'
               WHEN PRODUCT-CODE(I) = PCODE
                   DISPLAY PRODUCT-NAME(I), '- FOUND'
           END-SEARCH.
