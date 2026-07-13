 

                  IDENTIFICATION DIVISION.
       PROGRAM-ID. TESTPROD.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC.

       COPY DFHAID.

       COPY PRDMAPI.
       COPY PRDMAPO.

       01 WS-CODICE.
          05 WS-CODICE-PROD      PIC X(5).

       01 WS-DESCRIZIONE         PIC X(30).
       01 WS-PREZZO             PIC S9(6)V99 COMP-3.
       01 WS-SCORTA             PIC S9(3) COMP-3.

       PROCEDURE DIVISION.

       MAIN.

      *---------------------------------
      * Ricezione dati dalla mappa
      *---------------------------------

           EXEC CICS RECEIVE
                MAP('PRDMAP')
                MAPSET('PRDMAP')
                INTO(PRDMAPI)
           END-EXEC

      *---------------------------------
      * PF3 = Uscita
      *---------------------------------

           IF EIBAID = DFHPF3

              EXEC CICS RETURN
              END-EXEC

           END-IF

      *---------------------------------
      * Codice immesso
      *---------------------------------

           MOVE CODCLII
             TO WS-CODICE-PROD

      *---------------------------------
      * Ricerca DB2
      *---------------------------------

           EXEC SQL

                SELECT DESCRIZIONE,
                       PREZZO,
                       SCORTA_MINIMA

                  INTO :WS-DESCRIZIONE,
                       :WS-PREZZO,
                       :WS-SCORTA

                  FROM PRODOTTI

                 WHERE CODICE_PRODOTTO =
                       :WS-CODICE-PROD

           END-EXEC

      *---------------------------------
      * Esito ricerca
      *---------------------------------

           EVALUATE SQLCODE

              WHEN 0

                   MOVE WS-DESCRIZIONE
                     TO DESCRO

                   MOVE WS-PREZZO
                     TO PREZZOO

                   MOVE WS-SCORTA
                     TO SCORTAO

                   MOVE
                   'PRODOTTO TROVATO'
                     TO MESSAGGO

              WHEN 100

                   MOVE SPACES
                     TO DESCRO

                   MOVE SPACES
                     TO PREZZOO

                   MOVE SPACES
                     TO SCORTAO

                   MOVE
                   'PRODOTTO NON TROVATO'
                     TO MESSAGGO

              WHEN OTHER

                   MOVE
                   'ERRORE DB2'
                     TO MESSAGGO

           END-EVALUATE

      *---------------------------------
      * Invio mappa aggiornata
      *---------------------------------

           EXEC CICS SEND
                MAP('PRDMAP')
                MAPSET('PRDMAP')
                FROM(PRDMAPO)
                ERASE
                CURSOR
           END-EXEC

      *---------------------------------
      * Ritorno in attesa
      *---------------------------------

           EXEC CICS RETURN
                TRANSID('TPRD')
                COMMAREA(DFHCOMMAREA)
           END-EXEC.
