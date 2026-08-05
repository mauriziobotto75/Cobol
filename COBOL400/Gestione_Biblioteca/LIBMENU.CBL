       IDENTIFICATION DIVISION.
       PROGRAM-ID. LIBMENU.

       WORKING-STORAGE SECTION.

       01 WS-SCELTA      PIC X.
       01 WS-FINE        PIC X VALUE 'N'.

       PROCEDURE DIVISION.

       MAIN.

           PERFORM UNTIL WS-FINE = 'S'

              DISPLAY
              '1 INSERIMENTO LIBRO'

              DISPLAY
              '2 VARIAZIONE LIBRO'

              DISPLAY
              '3 CANCELLAZIONE LIBRO'

              DISPLAY
              '4 RICERCA LIBRO'

              DISPLAY
              '5 STAMPA LIBRI'

              DISPLAY
              '6 GESTIONE PRESTITI'

              DISPLAY
              '9 FINE'

              ACCEPT WS-SCELTA

              EVALUATE WS-SCELTA

                 WHEN '1'
                    CALL 'LIBINS'

                 WHEN '2'
                    CALL 'LIBVAR'

                 WHEN '3'
                    CALL 'LIBCAN'

                 WHEN '4'
                    CALL 'LIBRIC'

                 WHEN '5'
                    CALL 'LIBSTA'

                 WHEN '9'
                    MOVE 'S'
                      TO WS-FINE

              END-EVALUATE

           END-PERFORM

           GOBACK.
