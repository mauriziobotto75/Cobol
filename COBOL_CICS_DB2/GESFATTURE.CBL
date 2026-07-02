TABELLE DB2:
CREATE TABLE CLIENTI
(
    COD_CLI    DECIMAL(5,0)    NOT NULL,
    INDIRIZZO  CHAR(20),
    TELEFONO   CHAR(10),
    CITTA      CHAR(10),
    NOMINATIVO CHAR(25),
    PRIMARY KEY (COD_CLI)
);
CREATE TABLE FATTURE
(
    COD_CLI    DECIMAL(5,0)    NOT NULL,
    NUM_FAT    CHAR(8)         NOT NULL,
    IMPONIBILE DECIMAL(15,2),
    IVA        DECIMAL(15,2),
    TOT_FATT   DECIMAL(15,2),
    PRIMARY KEY (COD_CLI,NUM_FAT)
);
CREATE TABLE SITCLIEN
(
    COD_CLI        DECIMAL(5,0),
    INDIRIZZO      CHAR(20),
    TELEFONO       CHAR(10),
    NUM_FATTURE    DECIMAL(8,0),
    FATT_DA        CHAR(8),
    FATT_A         CHAR(8),
    TOT_IMPONIBILE DECIMAL(15,2),
    TOT_IVA        DECIMAL(15,2),
    TOT_FATTURA    DECIMAL(15,2)
);
IDENTIFICATION DIVISION.
       PROGRAM-ID. GESFATTTURE.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       01 WS-FINE-CLI          PIC X VALUE 'N'.
          88 EOF-CLI VALUE 'Y'.

       01 WS-FINE-FAT          PIC X VALUE 'N'.
          88 EOF-FAT VALUE 'Y'.

       **************************************************
       * CLIENTE
       **************************************************
       01 WS-CLIENTE.
          05 WS-COD-CLI        PIC S9(5) COMP-3.
          05 WS-INDIRIZZO      PIC X(20).
          05 WS-TELEFONO       PIC X(10).
          05 WS-CITTA          PIC X(10).
          05 WS-NOMINATIVO     PIC X(25).

       **************************************************
       * FATTURA
       **************************************************
       01 WS-FATTURA.
          05 WS-FAT-CODCLI     PIC S9(5) COMP-3.
          05 WS-FAT-NUMERO     PIC X(8).
          05 WS-IMPONIBILE     PIC S9(13)V99 COMP-3.
          05 WS-IVA            PIC S9(13)V99 COMP-3.
          05 WS-TOTALE         PIC S9(13)V99 COMP-3.

       **************************************************
       * TOTALI
       **************************************************
       01 WS-TOTALI.
          05 WS-TOT-IMPONIBILE PIC S9(13)V99 COMP-3.
          05 WS-TOT-IVA        PIC S9(13)V99 COMP-3.
          05 WS-TOT-FATTURA    PIC S9(13)V99 COMP-3.
          05 WS-NUM-FATTURE    PIC 9(8).
          05 WS-FATT-DA        PIC X(8).
          05 WS-FATT-A         PIC X(8).

       ********************************************************
       * CURSORE CLIENTI
       ********************************************************

       EXEC SQL
          DECLARE C-CLIENTI CURSOR FOR
             SELECT COD_CLI,
                    INDIRIZZO,
                    TELEFONO,
                    CITTA,
                    NOMINATIVO
               FROM CLIENTI
              ORDER BY COD_CLI
       END-EXEC.

       ********************************************************
       * CURSORE FATTURE
       ********************************************************

       EXEC SQL
          DECLARE C-FATTURE CURSOR FOR
             SELECT NUM_FAT,
                    IMPONIBILE,
                    IVA,
                    TOT_FATT
               FROM FATTURE
              WHERE COD_CLI = :WS-COD-CLI
              ORDER BY NUM_FAT
       END-EXEC.

       PROCEDURE DIVISION.

       MAIN-LOGIC.

           EXEC SQL
              OPEN C-CLIENTI
           END-EXEC.

           PERFORM LETTI-CLIENTE

           PERFORM UNTIL EOF-CLI

              MOVE ZERO TO WS-TOT-IMPONIBILE
              MOVE ZERO TO WS-TOT-IVA
              MOVE ZERO TO WS-TOT-FATTURA
              MOVE ZERO TO WS-NUM-FATTURE

              EXEC SQL
                 OPEN C-FATTURE
              END-EXEC

              PERFORM LETTI-FATTURA

              PERFORM UNTIL EOF-FAT

                 ADD WS-IMPONIBILE
                    TO WS-TOT-IMPONIBILE

                 ADD WS-IVA
                    TO WS-TOT-IVA

                 ADD WS-TOTALE
                    TO WS-TOT-FATTURA

                 ADD 1
                    TO WS-NUM-FATTURE

                 IF WS-NUM-FATTURE = 1
                    MOVE WS-FAT-NUMERO
                      TO WS-FATT-DA
                 END-IF

                 MOVE WS-FAT-NUMERO
                   TO WS-FATT-A

                 PERFORM LETTI-FATTURA

              END-PERFORM

              EXEC SQL
                 CLOSE C-FATTURE
              END-EXEC

              EXEC SQL
                 INSERT INTO SITCLIEN
                 (
                   COD_CLI,
                   INDIRIZZO,
                   TELEFONO,
                   NUM_FATTURE,
                   FATT_DA,
                   FATT_A,
                   TOT_IMPONIBILE,
                   TOT_IVA,
                   TOT_FATTURA
                 )
                 VALUES
                 (
                   :WS-COD-CLI,
                   :WS-INDIRIZZO,
                   :WS-TELEFONO,
                   :WS-NUM-FATTURE,
                   :WS-FATT-DA,
                   :WS-FATT-A,
                   :WS-TOT-IMPONIBILE,
                   :WS-TOT-IVA,
                   :WS-TOT-FATTURA
                 )
              END-EXEC

              PERFORM LETTI-CLIENTE

           END-PERFORM

           EXEC SQL
              CLOSE C-CLIENTI
           END-EXEC

           EXEC CICS RETURN
           END-EXEC.

       LETTI-CLIENTE.

           EXEC SQL
              FETCH C-CLIENTI
                 INTO :WS-COD-CLI,
                      :WS-INDIRIZZO,
                      :WS-TELEFONO,
                      :WS-CITTA,
                      :WS-NOMINATIVO
           END-EXEC

           IF SQLCODE = 100
              SET EOF-CLI TO TRUE
           END-IF.

       LETTI-FATTURA.

           EXEC SQL
              FETCH C-FATTURE
                 INTO :WS-FAT-NUMERO,
                      :WS-IMPONIBILE,
                      :WS-IVA,
                      :WS-TOTALE
           END-EXEC

           IF SQLCODE = 100
              SET EOF-FAT TO TRUE
           END-IF.
