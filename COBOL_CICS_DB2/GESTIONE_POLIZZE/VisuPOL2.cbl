 

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

       EXEC SQL
            INCLUDE DCLPOL
       END-EXEC.
