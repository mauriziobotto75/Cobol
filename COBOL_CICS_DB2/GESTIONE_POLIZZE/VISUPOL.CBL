       IDENTIFICATION DIVISION.
       PROGRAM-ID. VISUPOL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.

       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.

       COPY MAPVISU.
       COPY MAPVISUI.

       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.

       01 HV-COD-POL.
          05 HV-CODICE       PIC X(10).

       01 HV-DATI-POLIZZA.
          05 HV-NOME         PIC X(40).
          05 HV-INDIRIZZO    PIC X(50).
          05 HV-IMPORTO      PIC S9(7)V99 COMP-3.
          05 HV-SCADENZA     PIC X(10).
          05 HV-SIGLA        PIC X(10).
          05 HV-CAVALLI      PIC S9(4) COMP.

       01 WS-MSG             PIC X(70).

       LINKAGE SECTION.

       01 DFHCOMMAREA.
          05 FILLER PIC X(1).

       PROCEDURE DIVISION.

       MAIN-PROGRAM.

           IF EIBCALEN = 0
              PERFORM VISUALIZZA-MASCHERA
              EXEC CICS RETURN
                   TRANSID('PVIS')
                   COMMAREA(DFHCOMMAREA)
                   LENGTH(1)
              END-EXEC
           END-IF

           PERFORM LEGGI-MAPPA

           PERFORM CONTROLLO-TASTI

           PERFORM LEGGI-POLIZZA

           PERFORM INVIA-RISULTATO

           EXEC CICS RETURN
           END-EXEC.

       *****************************************************************
       VISUALIZZA-MASCHERA.
       *****************************************************************

           MOVE LOW-VALUES TO MAPVISUO

           MOVE 'VISUALIZZAZIONE POLIZZA'
             TO VIMSGO

           EXEC CICS SEND
                MAP('MAPVISU')
                MAPSET('POLMAP')
                ERASE
                CURSOR
           END-EXEC.

       *****************************************************************
       LEGGI-MAPPA.
       *****************************************************************

           EXEC CICS RECEIVE
                MAP('MAPVISU')
                MAPSET('POLMAP')
           END-EXEC.

           MOVE VICODOI TO HV-CODICE.

       *****************************************************************
       CONTROLLO-TASTI.
       *****************************************************************

           IF EIBAID = DFHPF3

              EXEC CICS XCTL
                   PROGRAM('POLMENU')
              END-EXEC

           END-IF.

       *****************************************************************
       LEGGI-POLIZZA.
       *****************************************************************

           EXEC SQL

              SELECT
                     NOME_CLIENTE,
                     INDIRIZZO,
                     DECIMAL(IMPORTO,9,2),
                     CHAR(SCADENZA),
                     SIGLA_AUTO,
                     NUM_CAVALLI

                INTO
                     :HV-NOME,
                     :HV-INDIRIZZO,
                     :HV-IMPORTO,
                     :HV-SCADENZA,
                     :HV-SIGLA,
                     :HV-CAVALLI

                FROM POLIZZE

               WHERE COD_POLIZZA = :HV-CODICE

           END-EXEC

           EVALUATE SQLCODE

              WHEN 0

                   MOVE HV-CODICE
                     TO VICODOO

                   MOVE HV-NOME
                     TO VINOMEO

                   MOVE HV-INDIRIZZO
                     TO VIINDIO

                   MOVE HV-IMPORTO
                     TO VIIMPOO

                   MOVE HV-SCADENZA
                     TO VISCADO

                   MOVE HV-SIGLA
                     TO VISIGAO

                   MOVE HV-CAVALLI
                     TO VICAVAO

                   MOVE 'POLIZZA TROVATA'
                     TO VIMSGO

              WHEN 100

                   MOVE 'POLIZZA INESISTENTE'
                     TO VIMSGO

              WHEN OTHER

                   MOVE 'ERRORE DB2'
                     TO VIMSGO

           END-EVALUATE.

       *****************************************************************
       INVIA-RISULTATO.
       *****************************************************************

           EXEC CICS SEND
                MAP('MAPVISU')
                MAPSET('POLMAP')
                CURSOR
           END-EXEC.
