* Dato il file sequenziale Movimenti, contenente:
* codice-prodotto, quantità e tipo-movimento (entrata, uscita);
* e dato il file a indici Prodotti, contenente:
* codice-prodotto (chiave primaria), descrizione, prezzo, scorta-minima
*
* Si chiede l'esecuzione dei seguenti punti:
* 1) Visualizzare i dati degli articoli compresi fra due codici-prodotto specificati dall'utente
* 2) Visualizzare l'elenco di tutti i prodotti con la relativa descrizione
* 3) Visualizzare la giacenza in magazzino segnando i prodotti sottoscorta
*
       identification division.
       program-id. Esercizio1.
       author. Matteo "netarrow" Tomasulo.
       date-written.  2-10-2007.
       date-compiled. 2-10-2007.
       
       environment division.
       input-output section.
       file-control.
           select Movimenti assign to disk
           file status is movimenti-status.
           
           select Prodotti assign to disk
                  organization is indexed
                  access mode is dynamic
                  record key is codice-prodotto
                  file status is prodotti-status.
          
       data division.
       file section.
       
       fd  Movimenti 
           value of file-id is "Movimenti.dat"
           label record is standard.
          
       01  Movimento.
           02  cod-prodotto    pic x(5).
           02  qta             pic 999.
           02  tipo-movimento  pic x.
           
       fd  Prodotti 
           value of file-id is "Prodotti.dat"
           label record is standard.
          
       01  Prodotto.
           02  codice-prodotto pic x(5).
           02  descrizione     pic x(30).
           02  prezzo          pic 9(6)V99.
           02  scorta-minima   pic 999.
           
       working-storage section.
      
       77  movimenti-status    pic xx.
       77  prodotti-status     pic xx.
       77  operazione          pic 9.
       77  codice-partenza     pic x(5).
       77  codice-arrivo       pic x(5).
       77  superato-limite     pic xx      value is "no".
       77  movimenti-finiti    pic xx.
       77  prodotti-finiti     pic xx.
       77  cod-prodotto-bak    pic x(5).
       77  giacenza-parziale   pic S999.
       77  file-esistenti      pic x.
       77  dev                 pic 9.
       77  ancora              pic xx      value is "si".
       
       procedure division.
       
       begin.
           display "I file esistono gia'?s/n"
           accept file-esistenti.
           
           if file-esistenti = "n" then
           		perform CreaFile
           end-if.
       
           perform ShowMenu.
           
       ShowMenu.      
       	   display " ".       
           display "Scegliere l'operazione da fare: ".
           display "1) Visualizza intervallo fra due codici".
           display "2) Visualizza tutto l'elenco dei prodotti".
           display "3) Visualizza la giacenza e indica le sottoscorte".
           display "4) Esci".
           accept operazione.
           
           evaluate operazione
                    when 1	    perform ShowInterval
                    when 2      perform ShowAll
                    when 3      perform ShowStock
                    when 4      stop run
                    when other  display "Scelta non valida" 
                                perform ShowMenu
           end-evaluate. 
           
       CreaFile.
           open output Prodotti.
       
           display " ".
           display "-----------------------------------------".
           display "|         Inserire i Prodotti            |".
           display "-----------------------------------------".
              
           perform ScriviProdotti until ancora = "no".
                                            
           close Prodotti.  
           
           move "si" to ancora.                                       
       
           open output Movimenti.
           move "si" to ancora.
              
           display " ".
           display "-----------------------------------------".
           display "|         Inserire i Movimenti          |".
           display "-----------------------------------------".
            
           perform ScriviMovimenti until ancora = "no".
                                            
           close Movimenti.                              
                  
       ScriviMovimenti.
           display " ".
           display "Inserire il codice-prodotto: ".
           accept cod-prodotto.
           display "Inserire quantita':".
           accept qta.
           display "Inserire tipo movimento'".
           accept tipo-movimento.
              
           write Movimento.
          
           display "Fare un altro inserimento?(si/no)".
           accept ancora.

                      
       ScriviProdotti.
       	   display " ".
       	   display "Inserire il codice-prodotto".
       	   accept codice-prodotto.
       	   display "Inserire descrizione".
       	   accept descrizione.
       	   display "Inserire prezzo".
       	   accept prezzo.
       	   display "Inserire scorta minima".
       	   accept scorta-minima.
       	   
       	   write Prodotto
       	   	   invalid key
                display "Si e' verificato un errore in scrittura"
                stop 2
       	   end-write.
       	   	   
           display "Fare un altro inserimento?(si/no)".
           accept ancora.
       
       LeggiMovimenti.
       	   read Movimenti
       	     at end
       	      move "si" to movimenti-finiti
       	     not at end
       	      move "no" to movimenti-finiti
           end-read.
       
       LeggiProdotti.
           read Prodotti next record
              at end
                move "si" to prodotti-finiti
              not at end
                move "no" to prodotti-finiti
           end-read.  
            
       ShowInterval.
           open input Movimenti.
           open input Prodotti.           
           
           perform LeggiMovimenti.
           
           display "Inserire il codice del prodotto da cui partire:".
           accept codice-partenza.
           display "Inserire il codice del prodotto a cui arrivare:".
           accept codice-arrivo.          
           perform until (superato-limite = "si") 
                    or   (movimenti-finiti = "si")
               if cod-prodotto < codice-partenza then
                  perform LeggiMovimenti
               else 
               if cod-prodotto > codice-arrivo then
                  move "si" to superato-limite
               else
               if cod-prodotto >= codice-partenza and 
                               <= codice-arrivo then
                  move cod-prodotto to codice-prodotto
                  read Prodotti
                  invalid key
                    display "Errore non ho trovato il codice-prodotto"
                  not invalid key
                  display Prodotto
               perform LeggiMovimenti
               end-if 
               end-if
           end-perform.
           close Movimenti.
           close Prodotti.          
           perform ShowMenu.             
           
       ShowAll.
           open input Prodotti.
           
           perform LeggiProdotti.
          
           perform until prodotti-finiti = "si"
               display codice-prodotto " " descrizione
               perform LeggiProdotti
           end-perform.
           
           close Prodotti.
           
           perform ShowMenu.
           
       ShowStock.
           open input Movimenti.
           open input Prodotti.
           set dev to 0.
	       set giacenza-parziale to 0.
	       perform LeggiMovimenti.

           perform until movimenti-finiti = "si"
	        if dev = 0 then
	           set dev to 1
	         else
	           if cod-prodotto-bak <> cod-prodotto then
		      perform Aggiorna
		    end-if  
	        end-if
	        move cod-prodotto to cod-prodotto-bak
	        if tipo-movimento = "e" then
	           add qta to giacenza-parziale
	        else
	           subtract qta from giacenza-parziale
	        end-if
	        perform LeggiMovimenti
	       end-perform.
	   
	       perform Aggiorna.          

           close Movimenti.
           close Prodotti.
           
           perform ShowMenu.

       Aggiorna.
           move cod-prodotto to codice-prodotto.
	       read Prodotti
	        invalid key display "Errore"
	        not invalid key
	         display "Giacenza " cod-prodotto-bak ": " 
	         giacenza-parziale
	        if giacenza-parziale < scorta-minima then
	         display cod-prodotto-bak " sotto scorta!"
	        end-if
	       end-read.
	       set giacenza-parziale to 0.
