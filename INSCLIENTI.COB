      ******************************************************************
      * Author:
      * Date:
      * Purpose:
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       PROGRAM-ID. INSCLIENTI.
       ENVIRONMENT DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       CONFIGURATION SECTION.
      *-----------------------
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CLIENTI      ASSIGN TO DISK
                               ACCESS IS SEQUENTIAL
                               ORGANIZATION IS SEQUENTIAL.


      *-----------------------
       DATA DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       FILE SECTION.
       FD  CLIENTI
           BLOCK CONTAINS 5 RECORDS
           LABEL RECORDS ARE STANDARD
           DATA RECORD IS RECORD-CLIENTE.

       01  RECORD-CLIENTE.
           05  COD-CLI PIC 9(6) VALUE 0.
           05  RAG-CLI PIC X(30) VALUE SPACES.
           05  IND-CLI PIC X(30) VALUE SPACES.
           05  CAP-CLI PIC 9(5) VALUE 0.
           05  CIT-CLI PIC X(20) VALUE SPACES.
      *-----------------------
       WORKING-STORAGE SECTION.

       01  DATI-CLIENTE.
           05  WS-COD-CLI PIC 9(6) VALUE 0.
           05  WS-RAG-CLI PIC X(30) VALUE SPACES.
           05  WS-IND-CLI PIC X(30) VALUE SPACES.
           05  WS-CAP-CLI PIC 9(5) VALUE 0.
           05  WS-CIT-CLI PIC X(20) VALUE SPACES.

       SCREEN SECTION.

       01  SCREEN-CLI.

           05  BLANK SCREEN.
           05  LINE 2 COLUMN 30 VALUE "CARICAMENTO CLIENTI"
                                REVERSE-VIDEO.
           05  LINE 5 COLUMN 2 VALUE "CODICE CLIENTE  :".
           05  LINE 5 COLUMN 20 PIC Z(6) USING COD-CLI.
           05  LINE 7 COLUMN 2 VALUE "RAGIONE SOCIALE :".
           05  LINE 7 COLUMN 20 pic X(30) USING RAG-CLI.
           05  LINE 9 COLUMN 2 VALUE "INDIRIZZO       :".
           05  LINE 9 COLUMN 20 PIC X(30) USING IND-CLI.
           05  LINE 11 COLUMN 2  VALUE "CAP:          ".
           05  LINE 11 COLUMN 20 PIC Z(5) USING CAP-CLI.
           05  LINE 13 COLUMN 2  VALUE "CITTA'        :".
           05  LINE 13 COLUMN 20 PIC X(20) USING CIT-CLI.
      *-----------------------
       PROCEDURE DIVISION.
      *-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-*-
       MAIN-PROCEDURE.
      *INIZIO.
           OPEN OUTPUT CLIENTI.

       CARICA.
           PERFORM AZZ-DATI-CLIENTI THRU AZZ-DATI-CLIENTI-EX.
           DISPLAY SCREEN-CLI.
           ACCEPT SCREEN-CLI.
           ACCEPT (24,80) CURSOR.
           ACCEPT FUNCTION-ID FROM ESCAPE-KEY.
           IF FUNCTION-ID = 1 GO TO FINE.

           MOVE DATI-CLIENTE TO RECORD-CLIENTE.
           WRITE RECORD-CLIENTE.
      **   GO TO CARICA.
            STOP RUN.
       AZZ-DATI-CLIENTI.
           MOVE 0 TO COD-CLI CAP-CLI.
           MOVE SPACES TO RAG-CLI IND-CLI CIT-CLI.
       END PROGRAM  INSCLIENTI.
