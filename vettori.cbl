* Questo programma fa l'intersezione e la differenza di due tabelle

identification division.
* Questo è tipo il program di pascal
program-id. operazioni-su-vettori-e-tabelle.
author. Matteo.
data division.
* Questo sarebbe tipo var
working-storage section.

* Queste sono tabelle contenenti 20 elementi, che hanno
* come campi codice fiscale, nome e titolo di studio
01  abitanti.
    02  abitante occurs 20 times.
      03  codice-fiscale-abitante pic x(30).
      03  nome-abitante pic x(30).
      03  titolo-studio-abitante pic x(30).

01  dipendenti.
    02  dipendente occurs 20 times.
      03  codice-fiscale-dipendente pic x(30).
      03  nome-dipendente pic x(30).
      03  qualifica-dipendente pic x(30).

01  interurbani.
    02  interurbano occurs 20 times.
      03  cod-fis-interurbano pic x(30).
      03  nome-interurbano pic x(30).
      03  qualifica-interurbano pic x(30).

01  non-dipendenti.
    02  non-dipendente occurs 20 times.
      03  cod-fis-non-dipendente pic x(30).
      03  nome-non-dipendente pic x(30).
      03  titolo-studio-non-dipendente pic x(30).

* Crea della variabili intere normali
* in pascal sarebbe dim:integer ecc...
77  dim-abitanti pic 99.
77  dim-dipendenti pic 99.
77  i pic 99.
77  j pic 99.
77  k pic 99.
      
procedure division.

* Questa è la procedura iniziale
* in pascal era begin..end. indicato spesso col commento {MAIN}
inizio.
* Con perform si richiamano le procedure
    perform insDimAbitanti.
    perform insAbitanti.
    perform insDimDipendenti.
    perform insDipendenti.
    perform cercaNonDipendenti.
* Display sarebbe un writeln
    display "Gli abitanti non dipendenti sono:"
* In COBOL un vettore lo si può stampare direttamente
* senza creae la procedura visualizza con il ciclo for come in pascal
    display non-dipendenti.
    perform cercaInterurbani.
    display "I dipendenti non abitanti sono:".
    display interurbani.
    stop run.

* Questa procedura crea una tabella contenente i dipendenti
* che non sono abitanti in quel comune
cercaInterurbani.
* inizializza le variabili i, j e k a 1, in pascal sarebbe
* i:=1 j:=1 k:=1;
    set i j k to 1.

* Con perform until si crea un while del pascal, visto che in cobol
* i cicli iterano per falso o si aggiunge not o si scambiano le relazioni:
* (maggiore diventa minore, uguale diverso e così via)
    perform until not (i <= dim-abitanti and j <= dim-dipendenti)
* Almeno l'if è uguale a pascal, le () tonde sarebbero le [] di pascal per indicare
* gli indici dei vettori
      if codice-fiscale-abitante(i) = codice-fiscale-dipendente(j)
* aggiunge 1 a i e j
        add 1 to i j
      else
        if codice-fiscale-abitante(i) > codice-fiscale-dipendente(j)
* con move si assegna il valore della prima variabile/campo
* a quella/quello specificato/a dopo to: move sorgente to destinazione
* N.B il valore viene copiato non spostato, quindi entrambe le variabili avranno
* lo stesso valore
          move codice-fiscale-dipendente(j) to cod-fis-interurbano(k) 
          move nome-dipendente(j) to nome-interurbano(k) 
          move qualifica-dipendente(j) to qualifica-interurbano(k) 
          add 1 to j k
      else
          add 1 to i   
        end-if  
      end-if
    end-perform.
    perform until not (j <= dim-dipendenti)
      move codice-fiscale-dipendente(j) to cod-fis-interurbano(k) 
      move nome-dipendente(j) to nome-interurbano(k) 
      move qualifica-dipendente(j) to qualifica-interurbano(k) 
      add 1 to j k
    end-perform.
ex-cercaInterurbani. exit.

* Crea una tabella contenente gli abitanti non lavoratori
cercaNonDipendenti.
    set i j k to 1.
    perform until not (i <= dim-abitanti and j <= dim-dipendenti)
      if codice-fiscale-abitante(i) = codice-fiscale-dipendente(j)
        add 1 to i j
      else
        if codice-fiscale-abitante(i) < codice-fiscale-dipendente(j)
          move codice-fiscale-abitante(i) to cod-fis-non-dipendente(k) 
          move nome-abitante(i) to nome-non-dipendente(k) 
          move titolo-studio-abitante(i) to titolo-studio-non-dipendente(k) 
          add 1 to i k
        else
          add 1 to j
        end-if  
      end-if
    end-perform.
    perform until not (i <= dim-abitanti)
      move codice-fiscale-abitante(i) to cod-fis-non-dipendente(k) 
      move nome-abitante(i) to nome-non-dipendente(k) 
      move titolo-studio-abitante(i) to titolo-studio-non-dipendente(k) 
      add 1 to i k
    end-perform.
ex-cercaNonDipendenti. exit.

* Questo è l'inserimento della tabella dei dipendenti
insDipendenti.

* Questo sarebbe un ciclo for, tradotto sarebbe:
* inizializza i ad 1 e aumentala di 1 fino a quando
* non arriva alla dimensione inserita(dim-dipendenti)
   perform varying i from 1 by 1 until i > dim-dipendenti
* Come in pascal per stampare il valore di una variabile
* si scrive il suo nome fuori dalle ", in cobol non serve la virgola
     display "Inserire codice fiscale del dipendente "  i
* accept sarebbe un readln 
     accept codice-fiscale-dipendente(i)
     display "Inserire nome del dipendente "  i
     accept nome-dipendente(i)
     display "Inserire qualifica del dipendente "  i
     accept qualifica-dipendente(i)
   end-perform.

ex-insDipendenti. exit.

* Questo è l'inserimento della dimensione della tabella controllando che
* sia compreso fra 1 e il massimo
insDimDipendenti.
    set dim-dipendenti to 0.
    
    perform until dim-dipendenti >= 1 and <= 20
      display "Inserire numero dipendenti, max 20"
      accept dim-dipendenti
    end-perform.
    
ex-insDimDipendenti. exit.

insAbitanti.

   perform varying i from 1 by 1 until i > dim-abitanti
     display "Inserire codice fiscale dell'abitante "  i
     accept codice-fiscale-abitante(i)
     display "Inserire nome dell'abitante "  i
     accept nome-abitante(i)
     display "Inserire titolo di studio dell'abitante "  i
     accept titolo-studio-abitante(i)
   end-perform.

ex-insAbitanti. exit.   

insDimAbitanti.
    set dim-abitanti to 0.

    perform until dim-abitanti >= 1 and <= 20
      display "Inserire numero abitanti, max 20"
      accept dim-abitanti
    end-perform.

ex-insDimAbitanti. exit.
