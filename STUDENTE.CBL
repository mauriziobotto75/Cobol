      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. GesStudente.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
      *-----------------------
           SELECT ESTUDENTE ASSIGN TO DISK
       ----ORGANIZATION IS INDEXED
           ACCESS MODE IS RANDOM
           RECORD KEY IS CD-STUDENT
           FILE STATUS IS WS-FS.

       FD  STUDENTE.
           COPY FD_Studente.

       FILE CONTROL.

       WORKING-STORAGE SECTION.
      *-----------------------
       01  WS-REGISTRO     PIC X(25) VALUE SPACES.
       01  FILLER REDEFINES WS-REGISTRO.
           02 WS-CD-STUDENT  PIC 9(05).
           02 WS-NM-STUDENT  PIC X(25).
       77  WS-EOF            PIC X(9).
           88 EOF-OK         VALUE 0 FALSE 1.
       77  WS-FS             PIC 99.
           88 FS-OK          VALUE 0.
       77  WS-CONT-REG       PIC 99.


       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.

       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
           OPEN INPUT STUDENTE
            PERFORM UNTIL WS-EOF = "F" THEN
                    READ STUDENT INTO WS-DADOS AT END MOVE "f" TO ws-EOF
                    NOT AT END DISPLAY WS-CD-STUDENT '   ' WS-NM-STUDENT
                    END-READ
            END-PERFORM.



      * The main procedure of the program

            DISPLAY "Hello world"
            STOP RUN.
      ** add other procedures here
       END PROGRAM GesStudente.
