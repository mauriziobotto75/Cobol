 

        IDENTIFICATION DIVISION.
       PROGRAM-ID. VISUPOL.

       ENVIRONMENT DIVISION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       COPY DFHAID.
       COPY DFHBMSCA.

       COPY MAPVISU.
       COPY MAPVISUI.

       COPY POLCOMM.
       01 WS-COSTANTI.

          05 WS-PGM-MENU
                PIC X(8)
                VALUE 'POLMENU'.

          05 WS-MSG-NOTFOUND
                PIC X(30)
                VALUE 'POLIZZA NON PRESENTE'.

          05 WS-MSG-OK
                PIC X(30)
                VALUE 'POLIZZA TROVATA'.
       LINKAGE SECTION.

       01 DFHCOMMAREA.
          05 LK-DATI PIC X(256).
       PROCEDURE DIVISION.

       MAIN.

           IF EIBCALEN = 0
              PERFORM 1000-SEND-FIRST
              GOBACK
           END-IF

           PERFORM 2000-RECEIVE

           PERFORM 3000-CHECK-PFKEY

           PERFORM 4000-SELECT-POLIZZA

           PERFORM 5000-SEND-MAP

           EXEC CICS RETURN
           END-EXEC.
MAIN-EX.
    EXIT.
    STOP-RUN.
       1000-SEND-FIRST.

           MOVE LOW-VALUES
             TO MAPVISUO

           MOVE 'VISUALIZZAZIONE POLIZZA'
             TO VIMSGO

           EXEC CICS SEND
                MAP('MAPVISU')
                MAPSET('POLMAP')
                ERASE
                CURSOR
           END-EXEC.
       2000-RECEIVE.

           EXEC CICS RECEIVE
                MAP('MAPVISU')
                MAPSET('POLMAP')
           END-EXEC.

           MOVE VICODOI
             TO COD-POLIZZA.
       EXEC SQL
            INCLUDE SQLCA
       END-EXEC.
       3000-CHECK-PFKEY.

           IF EIBAID = DFHPF3

              EXEC CICS XCTL
                   PROGRAM('POLMENU')
              END-EXEC

           END-IF.
              4000-SELECT-POLIZZA.

           EXEC SQL

              SELECT
                  NOME_CLIENTE,
                  INDIRIZZO,
                  IMPORTO,
                  CHAR(SCADENZA),
                  SIGLA_AUTO,
                  NUM_CAVALLI,
                  STATO_PAGAMENTO

              INTO
                  :NOME-CLIENTE,
                  :INDIRIZZO,
                  :IMPORTO,
                  :SCADENZA,
                  :SIGLA-AUTO,
                  :NUM-CAVALLI,
                  :STATO-PAGAMENTO

              FROM POLIZZE

              WHERE COD_POLIZZA =
                    :COD-POLIZZA

           END-EXEC.

           EVALUATE SQLCODE

              WHEN 0
                   MOVE WS-MSG-OK
                     TO VIMSGO

              WHEN 100
                   MOVE WS-MSG-NOTFOUND
                     TO VIMSGO

              WHEN OTHER
                   MOVE SQLCODE
                     TO CA-RETURN-CODE
                   MOVE 'ERRORE DB2'
                     TO VIMSGO

           END-EVALUATE.
              5000-SEND-MAP.

           EXEC CICS SEND
                MAP('MAPVISU')
                MAPSET('POLMAP')
                CURSOR
           END-EXEC.
       EXEC SQL
            INCLUDE DCLPOL
       END-EXEC.
