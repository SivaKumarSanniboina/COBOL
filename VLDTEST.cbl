       IDENTIFICATION DIVISION.
       PROGRAM-ID. VLDTEST.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77  WS-INPUT        PIC    X(20)  VALUE 'HELLO COBOL'.
       77  AUTHOR-NAME     PIC    X(20)  VALUE 'ANAND VARMA'.
       77  FIRST-NAME      PIC    X(10).
       77  LAST-NAME       PIC    X(10).

       77  WS-INPUT-LEN    PIC    S9(4) COMP  VALUE ZERO.
       77  WS-TALLY-CNT    PIC    S9(4) COMP  VALUE ZEROES.
       77  WS-REPL-CNT    PIC    S9(4) COMP  VALUE ZEROES.

       PROCEDURE DIVISION.
       0000-MAIN-PARA.

           DISPLAY WS-INPUT.

           MOVE FUNCTION LENGTH(WS-INPUT)  TO WS-INPUT-LEN.
           DISPLAY WS-INPUT-LEN.

           DISPLAY FUNCTION LOWER-CASE(WS-INPUT).
           DISPLAY FUNCTION UPPER-CASE(WS-INPUT).

           INSPECT WS-INPUT TALLYING WS-TALLY-CNT FOR TRAILING SPACES.
           DISPLAY WS-TALLY-CNT.

           MOVE 0 TO WS-REPL-CNT.
           INSPECT WS-INPUT TALLYING WS-TALLY-CNT FOR ALL CHARACTERS
                               BEFORE INITIAL SPACE.
           DISPLAY WS-TALLY-CNT.


           INSPECT WS-INPUT REPLACING ALL 'O' BY ''.
           DISPLAY WS-INPUT.
           INSPECT WS-INPUT REPLACING CHARACTERS BY '*'.
           DISPLAY WS-INPUT.	

           UNSTRING AUTHOR-NAME DELIMITED BY SPACE
               INTO FIRST-NAME, LAST-NAME.
           DISPLAY FIRST-NAME, LAST-NAME.

           STRING FIRST-NAME(1:5), ' ', LAST-NAME INTO AUTHOR-NAME.
           DISPLAY AUTHOR-NAME.
           STOP RUN.
