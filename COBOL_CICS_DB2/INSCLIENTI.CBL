
CREATE TABLE CLIENTI
(
    COD_CLI DECIMAL(6,0) NOT NULL,
    RAG_CLI VARCHAR(30),
    IND_CLI VARCHAR(30),
    CAP_CLI DECIMAL(5,0),
    CIT_CLI VARCHAR(20),
    PRIMARY KEY (COD_CLI)
);
******** Mappa da creare con TSO o in ambiente Mainframe comunque                       *************************************************
01 CLIMAPI.

   05 CODCLI-I      PIC X(06).
   05 RAGCLI-I      PIC X(30).
   05 INDCLI-I      PIC X(30).
   05 CAPCLI-I      PIC X(05).
   05 CITCLI-I      PIC X(20).

01 CLIMAPO.

   05 MSGO          PIC X(40).
****************** Programma COBOL/CICS/DB2             **********************************************************************************
IDENTIFICATION DIVISION.
       PROGRAM-ID. INSCLIENTI.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       01 WS-MESSAGGIO            PIC X(40).

       01 WS-CLIENTE.
          05 WS-COD-CLI           PIC S9(6) COMP-3.
          05 WS-RAG-CLI           PIC X(30).
          05 WS-IND-CLI           PIC X(30).
          05 WS-CAP-CLI           PIC S9(5) COMP-3.
          05 WS-CIT-CLI           PIC X(20).

       COPY CLIMAP.

       PROCEDURE DIVISION.

       MAIN-LOGIC.

           EXEC CICS RECEIVE
                MAP('CLIMAP')
                MAPSET('CLIMAP')
                INTO(CLIMAPI)
           END-EXEC

           IF EIBAID = DFHPF3
              GO TO FINE-PROG
           END-IF

           MOVE FUNCTION NUMVAL(CODCLI-I)
             TO WS-COD-CLI

           MOVE RAGCLI-I
             TO WS-RAG-CLI

           MOVE INDCLI-I
             TO WS-IND-CLI

           MOVE FUNCTION NUMVAL(CAPCLI-I)
             TO WS-CAP-CLI

           MOVE CITCLI-I
             TO WS-CIT-CLI

      *-----------------------------------
      * INSERT DB2
      *-----------------------------------

           EXEC SQL
              INSERT INTO CLIENTI
              (
                COD_CLI,
                RAG_CLI,
                IND_CLI,
                CAP_CLI,
                CIT_CLI
              )
              VALUES
              (
                :WS-COD-CLI,
                :WS-RAG-CLI,
                :WS-IND-CLI,
                :WS-CAP-CLI,
                :WS-CIT-CLI
              )
           END-EXEC

           IF SQLCODE = 0
              MOVE 'CLIENTE INSERITO' TO MSGO
           ELSE
              MOVE 'ERRORE INSERIMENTO DB2'
                 TO MSGO
           END-IF

           EXEC CICS SEND
                MAP('CLIMAP')
                MAPSET('CLIMAP')
                FROM(CLIMAPO)
                ERASE
           END-EXEC

           EXEC CICS RETURN
                TRANSID('ICLI')
           END-EXEC.

       FINE-PROG.

           EXEC CICS RETURN
           END-EXEC.
