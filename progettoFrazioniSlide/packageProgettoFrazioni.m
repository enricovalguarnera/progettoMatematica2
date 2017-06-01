(* ::Package:: *)

(*
------------------------------------------------
Titolo : Le Frazioni 
Autori : 
	|- Team matematico: Francesca Larese Filon, Sara De Marchi
	|- Team informatico: Enrico Ceccolini, Enrico Valguarnera
Anno del corso: 2016 / 2017
Versione Mathematica 10.2
Descrizione :
	Package contenente le funzioni per la lezione di matematica sulle frazioni.
Modifica : 25/05/2017
------------------------------------------------
*)
(* Pulisce tutte le variabili utilizzate nel progetto ProgettoFrazioni *)
ClearAll["ProgettoFrazioni`*"];

(* Funzione che definisce il contesto di lavoro. Inizio package *)
BeginPackage["ProgettoFrazioni`"]

(* Definizione funzioni visibili all'esterno del package *)
getEsempioTipoFrazione::usage = "Funzione che restituisce l'esempio interattivo sul tipo della frazione"
getEsempioBottiglie::usage = "Funzione che restituisce l'esempio interattivo con bicchieri e bottiglie"
getEsempioNipoti::usage = "Funzione che restituisce l'esempio interattivo della paghetta"
getEsempioDifferenza::usage = "Funzione che restituisce l'esempio interattivo di differenza tra frazioni"
getEsempioProdotto::usage = "Funzione che restituisce l'esempio interattivo di prodotto tra frazioni"
getEsempioSomma::usage = "Funzione che restituisce l'esempio interattivo di somma tra frazioni"
getEsempioDivisione::usage = "Funzione che restituisce l'esempio interattivo di divisione tra frazioni" 
getEsempioRicetta::usage = "Funzione che restituisce l'esempio interattivo della ricetta"
getConfrontoFrazioni::usage = "Funzione che resituisce l'esempio del confronto di due frazioni. Calcola se una frazione \[EGrave] minore, maggiore o uguale di un'altra"

(* Inizio parte privata del package *)
Begin["Private`"];

(*
* Funzione che analizza una frazione e restituisce il suo tipo.
* Input: numeratore_, denominatore_
* Output: "frazione apparente", "frazione propria", "frazione impropria"
*)
es1getTipoFrazione[numeratore_,denominatore_] := (
	Module[{tipo = "frazione "},
		If[numeratore==denominatore || Mod[numeratore,denominatore]==0,tipo =tipo <>"apparente",
			If[numeratore<denominatore,tipo =tipo <>"propria",
				If[numeratore>denominatore,tipo =tipo <>"impropria"];
			]
		];
		tipo (* Restituisco la variabile tipo *)
		]
	) 


(* es1
* Summary: restituisce l'esempio 1, tipo interattivo della frazione
* Parameters: 
* Return: Manipulate dell'esempio 1
*)
getEsempioTipoFrazione[] := (
	es1coloreNumeratore = Blue;(*colore per i vari numeratori*)
	es1coloreDenominatore = RGBColor["#008C00"]; (*colore per i vari denumeratori*)
		Manipulate[
			Pane[
				Text[ (* Definizione dell'unica riga da stampare *)
					Row[{With[{es1numeratore=es1numeratore,es1denominatore=es1denominatore},
					HoldForm[Style[HoldForm[es1numeratore],Directive[es1coloreNumeratore,100]]/Style[HoldForm[es1denominatore],Directive[es1coloreDenominatore,100]]]],
					Style["   \[EGrave] una ",20], Style[es1getTipoFrazione[es1numeratore,es1denominatore],30]}]
				],
				{600,250}, (*dimensioni pannello*)
				Alignment->Center  (*allineamento centrale degli elementi contenuti*)
			],
			{{es1numeratore, (* Variabile legata allo slider *)
				1,(*valore iniziale slider numeratore*)
				Style["Numeratore",Directive[es1coloreNumeratore,Large]]
				},
				1,100,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"}, (*rimuovo tutto tranne l'inputField*)
				LabelStyle->Directive[es1coloreNumeratore,Large],
				ImageSize-> 400 (* Lunghezza dello slide *)
			},(*--fine slider numeratore*)
			{{es1denominatore,10,Style["Denominatore",Directive[es1coloreDenominatore,Large]]},1,100,1,
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
				LabelStyle->Directive[es1coloreDenominatore,Large],ImageSize->400}
			]
	)




(* Funzione per es2
* Summary: restituisce disegni di bicchieri per la Graphics
* Parameters: 
	offX_ : offset X del primo bicchiere
	offY_ : altezza in cui disegnare i bicchieri
	spX_ : spazio tra un bicchiere e il successivo
	numBottiglie_ : numero di bottiglie (utile se pieni_ \[EGrave] True)
	numBicchieri_ : numero bicchieri da disegnare
	altezzaObj_ : altezza del bicchiere
	larghezzaObj_ : larghezza del bicchiere
	pieni_ : True se contengono la bevanda, False se vuoti
* Return: Lista di bicchieri
*)
es2getBicchieri[offX_,offY_,spX_,numBottiglie_,numBicchieri_,altezzaObj_,larghezzaObj_,pieni_]:=(
	Module[
		{
		objBicchieri = {},(*pulisco il vettore degli oggetti da disegnare*)
		raggioBase = (larghezzaObj/2), (*raggio della base della bottiglia e del bicchiere*)
		altezzaLiquido = numBottiglie /numBicchieri*altezzaObj(*livello della bevanda per ciascun bicchiere*),
		i
		},

		For[i = 0, i< numBicchieri,i++,(*per ogni bicchiere aggiungo il suo disegno nel vettore*)
			AppendTo[objBicchieri,{
				(*se le bottiglie sono vuote allora disegno il liquido*)
				If[pieni,(*disegno la bevanda per prima per non nascondere le linee del bicchiere*)
					{Orange,(*colore della bevanda*)
					Rectangle[(*coordinate per il liquido dell'i-esimo bicchiere*)
						{-raggioBase+(i*spX)+offX,offY},
						{raggioBase+(i*spX)+offX,altezzaLiquido+offY}]}
				],
				(*disegno i-esimo bicchiere*)
				Black,
				Thick,
				Line[{{-raggioBase+(i*spX)+offX,0+offY},{-raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea lato sinistro*)
				Line[{{raggioBase+(i*spX)+offX,0+offY},{raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea lato destro*)
				Line[{{-raggioBase+(i*spX)+offX,0+offY},{+raggioBase+(i*spX)+offX,0+offY}}](*linea base*)
			}]
		];
objBicchieri (*return degli oggetti da disegnare*)
])


(* Funzione per es2
* Summary: restituisce disegni di bottiglie per la Graphics
* Parameters: 
	offX_ : offset X della prima bottiglia
	offY_ : altezza in cui disegnare i bicchieri
	spX_ : spazio tra una bottiglia e la successiva
	numBottiglie_ : numero di bottiglie
	altezzaObj_ : altezza della bottiglia
	larghezzaObj_ : larghezza della bottiglia
	altezzaCollo_ : altezza del collo della bottiglia
	pieni_ : True se contengono la bevanda, False se vuote
* Return: Lista di bottiglie
*)
es2getBottiglie[offX_,offY_,spX_,numBottiglie_,altezzaObj_,larghezzaObj_,altezzaCollo_,piene_]:=(
	Module[{
		objBottiglie = {},(*pulisco il vettore degli oggetti da disegnare*)
		raggioBase = (larghezzaObj/2),(*raggio della base della bottiglia e del bicchiere*)
		raggioCollo = (larghezzaObj/7),(*raggio del collo della bottiglia, 1/7 della base*)
		staccoCollo = (altezzaCollo/3),(*altezza della parte conica del collo*)
		i},

		For[i = 0, i< numBottiglie,i++,(*per ogni bottiglia aggiungo il suo disegno nel vettore*)
			AppendTo[objBottiglie,{
				If[piene,(*disegno la bevanda per prima per non nascondere le linee della bottiglia*)
				{
					Orange,(*colore della bevanda*)
					Rectangle[{-raggioBase+(i*spX)+offX,offY},{raggioBase+(i*spX)+offX,altezzaObj+offY}],
					Scale[Disk[{(i*spX)+offX,altezzaObj+offY},raggioBase],{1,.5}],
					Scale[Disk[{(i*spX)+offX,offY},raggioBase],{1,.5}]
				}
				],
				(*disegno i-esima bottiglia*)
				Black,
				Scale[Circle[{(i*spX)+offX,altezzaObj+altezzaCollo+offY},raggioCollo],{1,.5}],
				Scale[Circle[{(i*spX)+offX,altezzaObj+staccoCollo+offY},raggioCollo],{1,.5}],
				Line[{{-raggioCollo+(i*spX)+offX,altezzaObj+staccoCollo+offY},{-raggioCollo+(i*spX)+offX,altezzaObj+altezzaCollo+offY}}],
				Line[{{raggioCollo+(i*spX)+offX,altezzaObj+staccoCollo+offY},{raggioCollo+(i*spX)+offX,altezzaObj+altezzaCollo+offY}}],

				Scale[Circle[{(i*spX)+offX,altezzaObj+offY},raggioBase],{1,.5}], (*cerchio superiore*)
				Line[{{-raggioBase+(i*spX)+offX,offY},{-raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea sinistra*)
				Line[{{raggioBase+(i*spX)+offX,offY},{raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea destra*)
				Scale[Circle[{(i*spX)+offX,offY},raggioBase],{1,.5}](*cerchio base*)
			}]
		];
		objBottiglie (*return degli oggetti da disegnare*)
	]
)

(* Funzione per es2
* Summary: restituisce disegni di linee di separazione la Graphics
* Parameters: 
	offY_ : altezza in cui disegnare il divisorio
	plotRange_ : dimensioni del pannello di disegno
	altezzaDivisorio_ : altezza del divisorio
* Return: disegno divisorio
*)
es2getDivisiorioGrafico[offY_,plotRange_,altezzaDivisorio_] := (
	Module[{
		objDivisorioGrafico = {}(*pulisco il vettore degli oggetti da disegnare*)},
		AppendTo[objDivisorioGrafico,{
			Black,
			Line[{{plotRange[[1]][[1]],offY+altezzaDivisorio},{plotRange[[1]][[2]]/2,offY+altezzaDivisorio}}],
			Line[{{plotRange[[1]][[2]]/2,plotRange[[1]][[1]]},{plotRange[[1]][[2]]/2,plotRange[[1]][[2]]}}],
			Arrow[{{plotRange[[1]][[1]]/2,offY+altezzaDivisorio},{plotRange[[1]][[1]]/2,offY}}]
		}];
		objDivisorioGrafico(*return degli oggetti da disegnare*)
	]
)

(* Funzione per es2
* Summary: restituisce informazioni graficabili degli input precedenti
* Parameters: 
	offY_ : altezza in cui scrivere la frase finale
	plotRange_ : dimensioni del pannello di disegno
	numBottiglie_ : numero di bottiglie
	numBicchieri_ : numero di bicchieri
	altezzaObj_ : altezza del bicchiere
	larghezzaObj_ : larghezza del bicchiere
	vecchiVal_ : vettore degli input precedenti
	coloreNum_ : colore per il numeratore
	coloreDen_ : colore per il denominatore
* Return: vettore di elementi di input precedenti
*)
es2getTestoFinale[offY_,plotRange_,numBottiglie_,numBicchieri_,altezzaObj_,larghezzaObj_,vecchiVal_,coloreNum_,coloreDen_]:=(
	Module[{
		objTesto= {},(*pulisco il vettore degli oggetti da disegnare*)
		offsetXTesto = 50,
		offsetXBicchiere = 100,
		offsetYStep = 200,
		offsetYElementi = 50,
		raggioBase = larghezzaObj/2,
		offXStoria = plotRange[[1]][[2]],
		offYStoria = plotRange[[2]][[2]]+100,
		frazioneSemplificata = numBottiglie/numBicchieri,
		i
		},

		AppendTo[objTesto,{
			Text[Style[StringForm["In questo caso ogni bicchiere \[EGrave] riempito per ``",
				Style[Numerator[frazioneSemplificata],coloreNum]/Style[Denominator[frazioneSemplificata],coloreDen]],20],
				{plotRange[[1]][[1]]/2,offY}]
		}];
		Do[AppendTo[objTesto,{
			Text[Style[StringForm["Bottiglie: ``",vecchiVal[[i]][[1]]],20,coloreNum],{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep},{-1,0}],
			Text[Style[StringForm["Bicchieri: ``",vecchiVal[[i]][[2]]],20,coloreDen],{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep-offsetYElementi},{-1,0}],
			Line[{{offXStoria/2,offYStoria-i*offsetYStep-offsetYElementi*2},{offXStoria,offYStoria-i*offsetYStep-offsetYElementi*2}}],
			(*disegno bevanda*)
			Orange,
			Rectangle[{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},
				{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi+altezzaObj*(vecchiVal[[i]][[1]]/vecchiVal[[i]][[2]])}],
			(*disegno bicchiere*)
			Black,
			Thick,
			Line[{{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},
				{-raggioBase+offXStoria-offsetXBicchiere,altezzaObj+offYStoria-i*offsetYStep-offsetYElementi}}],(*linea sinistra*)
			Line[{{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},
				{raggioBase+offXStoria-offsetXBicchiere,altezzaObj+offYStoria-i*offsetYStep-offsetYElementi}}],(*linea destra*)
			Line[{{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},
				{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi}}](*linea base*)
		}],{i,Range[Length[vecchiVal]]}];
		objTesto(*return degli oggetti da disegnare*)
	]
)


(* es2
* Summary: restituisce l'esempio 2, "quattro amici al bar..." interattivo.
* Parameters: 
* Return: Manipulate dell'esempio 2
*)
getEsempioBottiglie[] := (
	es2objBottiglieVuote = {};
	es2objBicchieriPieni ={};
	es2objBottigliePiene= {};
	es2objBicchieriVuoti= {};
	es2objDivisorioGrafico= {};
	es2objTestoFinale={};
	es2vecchiValori = {};
	es2maxValoreX = 1000;(*massimo valore di plot per la X*)
	es2maxValoreY=650;(*massimo valore di plot per la Y*)
	es2myPlotRange = {{-es2maxValoreX,es2maxValoreX},{-es2maxValoreY,es2maxValoreY}};(*{{minX,maxX},{minY,maxY}}*)
	es2coloreNumeratore = Blue;(*colore per i vari numeratori*)
	es2coloreDenominatore = RGBColor["#008C00"]; (*colore per i vari denumeratori*)

	es2altezzaOggetti = 110; (*altezza della bottiglia e del bicchiere*)
	es2larghezzaOggetti = 70;(*larghezza della bottiglia e del bicchiere*)
	es2altezzaColloBottiglia = 25; (*altezza del collo della bottiglia*)
	es2altezzaDivisorio = 60;(*altezza del divisorio tra tavoli*)
	es2spaceX = es2larghezzaOggetti*2; (*distanza tra un oggetto e un altro sull'asse X*)
	es2stepOffsetY = es2altezzaOggetti*2;(*distanza tra una riga di oggetti e un'altra sull'asse Y*)
	es2offsetXIniziale = es2myPlotRange[[1]][[1]]+ es2larghezzaOggetti; (*padding dal confine di sinistra*)
	es2offsetYIniziale = es2myPlotRange[[2]][[2]] -(es2altezzaOggetti+es2altezzaColloBottiglia)*1.2;(*padding dal confine dal confine superiore*)
	es2versa = False; (*True se viene premuto il bottone versa, False altrimenti*)
	es2maxNumStorico = 5; (*numero massimo di valori da tenere in memoria*)

	Manipulate[
		(*set e reset dei padding*)
		es2offsetX = es2offsetXIniziale;
		es2offsetY =es2offsetYIniziale;

		(*richiesta delle bottiglie da disegnare nella riga 0*)
		es2objBottigliePiene = es2getBottiglie[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,
			es2altezzaOggetti,es2larghezzaOggetti,es2altezzaColloBottiglia,True];

		es2offsetY = es2offsetYIniziale - 1*es2stepOffsetY;(*spostamento alla riga 1*)
		(*richiesta delle bottiglie da disegnare nella riga 1*)
		es2objBicchieriVuoti = es2getBicchieri[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,
			es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,False];

		es2offsetY = es2offsetYIniziale - 2*es2stepOffsetY;(*spostamento alla riga 2*)
		(*richiesta delle bottiglie da disegnare nella riga 2*)
		es2objDivisorioGrafico = es2getDivisiorioGrafico[es2offsetY, es2myPlotRange,es2altezzaDivisorio];
		If[es2versa  == True,(*se \[EGrave] stato premuto il bottone versa*)
			If[ es2numeroBicchieri>=es2numeroBottiglie,(*se i valori degli slider sono validi*)
			{
				es2offsetY = es2offsetYIniziale - 3*es2stepOffsetY;(*spostamento alla riga 3*)
				(*richiesta delle bottiglie da disegnare nella riga 3*)
				es2objBottiglieVuote =es2getBottiglie[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,
					es2altezzaOggetti,es2larghezzaOggetti,es2altezzaColloBottiglia,False];

				es2offsetY = es2offsetYIniziale - 4*es2stepOffsetY;(*spostamento alla riga 4*)
				(*richiesta delle bottiglie da disegnare nella riga 4*)
				es2objBicchieriPieni = es2getBicchieri[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,es2numeroBicchieri,
					es2altezzaOggetti,es2larghezzaOggetti,True];

				es2offsetY = es2offsetYIniziale - 4.5*es2stepOffsetY;(*spostamento alla riga 5*)
				(*richiesta delle bottiglie da disegnare nella riga 5*)
				es2objTestoFinale = es2getTestoFinale[es2offsetY,es2myPlotRange,es2numeroBottiglie,es2numeroBicchieri,
					es2altezzaOggetti,es2larghezzaOggetti,es2vecchiValori,es2coloreNumeratore,es2coloreDenominatore];
				If[Length[es2vecchiValori]<es2maxNumStorico,(*se c'\[EGrave] ancora spazio in memoria*)
					AppendTo[es2vecchiValori,{es2numeroBottiglie,es2numeroBicchieri}],(*aggiungo il valore in memoria*)
					{es2vecchiValori = Delete[es2vecchiValori,1];(*candello il valore pi\[UGrave] vecchio*)
					AppendTo[es2vecchiValori,{es2numeroBottiglie,es2numeroBicchieri}]}
				];
			},
			(* Se il valore non \[EGrave] valido viene visualizzata una finestra con un messaggio di errore *)
			MessageDialog["Attenzione! Il numero di bicchieri deve essere superiore o uguale al numero di bottiglie"];
			];
			es2versa=False;(*imposto al bottone a rilasciato*)
		];

		(*disegno gli oggetti grafici ottenuti*)
		Graphics[{
			es2objBottigliePiene,
			es2objBicchieriVuoti,
			es2objDivisorioGrafico,
			es2objBottiglieVuote,
			es2objBicchieriPieni,
			es2objTestoFinale
			},PlotRange->es2myPlotRange,
			ImageSize -> {es2maxValoreX-100,es2maxValoreY}],
			(*paramentri dello slider numeratore*)
			{{es2numeroBottiglie,1,(*valore iniziale slider numeratore*)
				Style["Numero bottiglie",Directive[es2coloreNumeratore,Large]]},
				1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
				LabelStyle->Directive[es2coloreNumeratore,Large],
				ImageSize->400
			},(*--fine slider numeratore*)
			(*paramentri dello slider denominatore*)
			{{es2numeroBicchieri,2,Style["Numero bicchieri",Directive[es2coloreDenominatore,Large]]}
			,1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es2coloreDenominatore,Large],ImageSize->400},
			Button["Versa nei bicchieri",es2versa=True,BaseStyle->{"GenericButton",16,Bold}]
			,TrackedSymbols:>{es2numeroBottiglie,es2numeroBicchieri,es2versa}
		]
	)


(* Funzione per es3
* Summary: Funzione che restituisce la grafica delle "monete del nonno" .
* Parameters: 
	y_ : valore intero, identifica il numero di cicli eseguiti dal for in questa funzione . 
* Return: vettore contenente oggetti grafici.
*)

es3getMoneteArray[y_] := (
	Module[
		{ objMonete,spaceXMonete,offsetX , raggio1 ,raggio2 , sizeDollaro , offsetY2 , quota1,  quota2,quota3,i},
		objMonete = {};  (* Array che conterr\[AGrave] il valore da ritornare *)
		spaceXMonete = 70; (* spazio tra una moneta e l'altra *)
		offsetX = -700; 
		raggio1 = 22; (* raggio per il disegno del primo cerchio della moneta *)
		raggio2 = 32; (* raggio per il disegno del secondo cerchio della moneta *)
		sizeDollaro = 20; (* dimensione del simbolo "$" *)
		offsetY2 = 350;
		quota1 = 0; (* Le monete vengono visualizzare in colonne da 3 partendo da sinistra. Quindi ci sono 3 diverse quote.*)
		quota2 = -70; 
		quota3 = -140;
		(* Ciclo For: ad ogni ciclo disegna una fila verticale di tre monete. *)
		For[i=0, i<y, i++,
			AppendTo[objMonete,
				{(*moneta 1*)
					Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota1 +offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota1+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize->sizeDollaro], {0 + (i*spaceXMonete) + offsetX,quota1 + offsetY2 },Automatic ],
					(*moneta 2*)
					Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota2 +offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota2 +offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize->sizeDollaro], {0 + (i*spaceXMonete) + offsetX, quota2  + offsetY2 },Automatic ],
					(*moneta 3*)
					Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota3+offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota3+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize-> sizeDollaro], {0 + (i*spaceXMonete) + offsetX,quota3 + offsetY2 },Automatic ]}
			]
		];
	objMonete
	]
)


(* Funzione per es3
PREREQUISITI RICHIESTI
\[Checkmark] Come calcolare il minimo comune multiplo (m.c.m.) e il massimo comune divisore (M.C.D.)
\[Checkmark] Operazioni tra numeri interi
\[Checkmark] Dimestichezza col concetto di congruenza
\[Checkmark] Numeri primi e numeri primi tra loro
\[Checkmark] Moltiplicare e dividere per le potenze di 10

CONSIDERAZIONI DIDATTICHE
Abbiamo scelto di concentrarci sulla visione della frazione come numero razionale, dando quindi largo spazio alle operazioni e 
al fondamentale significato di frazione come risultato di una divisione.
Non abbiamo pertanto insistito sul concetto di frazione come operatore perch\[EAcute] a nostro avviso \[EGrave] la parte trattata maggiormente  nella Scuola Primaria.
La scelta dei colori e de riquadri interattivi \[EGrave] stata fatta per permettere a tutti gli studenti di seguire le spiegazioni.
La visualizzazione a slide \[EGrave] dovuta ai diversi argomenti trattati.

POSSIBILI AMPLIAMENTI DEL PROGETTO:
\[LightBulb] Frazioni complementari
\[LightBulb] Espressioni e problemi con le frazioni
\[LightBulb] Trattazione dei casi con numeratore o denominatore uguale a 0
\[LightBulb] Frazioni che rappresentano numeri decimali periodici 

PROBLEMI RISCONTRATI DAL PUNTO DI VISTA INFORMATICO (VERSIONE MATHEMATICA 10.4):
\[WarningSign] L'editor fornito da Mathematica per la scrittura del package (file con formato .m) risulta scomodo da utilizzare. 
L'identazione non persiste nel tempo e non \[EGrave] prevista la visualizzazione del numero di riga, strumento indispensabile per effettuare il debug del programma.
Queste mancanze ci hanno portato ad utilizzare dei notebook (file con formato .nb) per la creazione dei nostri esercizi e a copiarli nel package solo quando risultavano nella loro versione definitiva.
\[WarningSign] La funzione "Manipulate" \[EGrave] definita in modo da valutare continuamente l'espressione definita al suo interno. 
Questo sovraccarica l'utilizzo delle risorse del computer, soprattutto quando l'espressione contiene la creazione e il disegno di oggetti grafici complessi.
\[CapitalEGrave] stata raggiunta una soluzione parziale al problema legando i cicli di valutazione dell'espressione all'effettiva modifica delle variabili tramite la fuznione "TrackedSymbols:>{nomeVariabileSlider1, nomeVariabileSlider2}".
\[WarningSign] Lo scope delle variabili definite all'interno di una funzione viene esteso all'intero ambiente.
Questo problema \[EGrave] risolvibile tramite l'utilizzo della funzione "Module" che per\[OGrave], per costruzione, non pu\[OGrave] essere combinata con la funzione "Manipulate" utilizzata largamente nel progetto. La funzione "Module" \[EGrave] stata utilizzata ove possibile, negli altri casi abbiamo adottato una linea guida che prevede l'aggiunta del numero di esercizio come prefisso del nome delle variabili in modo da renderle uniche ed evitare conflitti (e.g. es3nomeVariabile).




* Summary: Funzione che restituisce gli oggetti grafici per il disegno dei "nipoti".
* Parameters: 
	x_ : valore intero, identifica il numero di cicli eseguiti dal for in questa funzione. 
* Return: vettore contenente oggetti grafici.
*)

(* FUNZIONE che riempe l'array objNipoti (3 figure) *)
es3getNipotiArray[x_] := (
	Module[
		{objNipoti, spaceX, spaceY, offsetX,i},
		objNipoti = {};
		spaceX = 510;
		spaceY = -280;
		offsetX = -600;
		
		For[i=0, i<x, i++,
			AppendTo[objNipoti, (* aggiungo l'insieme di oggetti grafici all'array objNipoti gia esistente *)
				{
					Thickness[.005], (* Spessore delle linee che compongono il disgno *)
					Line[{{0 + (i*spaceX)+offsetX,0 + spaceY},{40 + (i*spaceX)+offsetX,60+ spaceY}}],           (*Gamba destra*)
					Line[{{40 + (i*spaceX)+offsetX,60+ spaceY},{80+ (i*spaceX)+offsetX,0 + spaceY}}],          (*Gamba sinistra*)
					Line[{{40 + (i*spaceX)+offsetX,60 + spaceY},{40 + (i*spaceX)+offsetX,140 + spaceY}}],   (*Busto*)
					Line[{{40 + (i*spaceX)+offsetX,140 + spaceY},{80 + (i*spaceX)+offsetX,100 + spaceY}}], (* Braccio destro*)
					Line[{{40 + (i*spaceX)+offsetX,140 + spaceY},{0 + (i*spaceX)+offsetX,100 + spaceY}}],   (* Braccio sinistro*)
					Circle[{38 + (i*spaceX)+offsetX,161 + spaceY},20.4]                                                                                     (*Testa*)
				}
			]
		];
	objNipoti
	]
)


(* Funzione per es3
* Summary: Funzione che restituisce gli oggetti grafici per il disegno delle "monete dei nipoti"
* Parameters: 
	numMonete_ : valore intero, identifica il numero di monete da disegnare. 
	offsetX_ : valore per il posizionamento del gruppo di oggetti grafici.
* Return: vettore contenente oggetti grafici.
*)
(* Funzione per calcolare l'array di monete che spettano ad ogni nipote, in base a quante monete puo dare il nonno*)
es3getDivisioneMonete[numMonete_, offsetX_] := (
	Module[
		{objFinal, spaceXMonete, raggio1, raggio2, sizeDollaro, offsetY2},
		objFinal = {}; (* Pulisco l'array objFinal *)
		spaceXMonete = 35; (* Spazio tra il disegno di una moneta e un'altra *)
		raggio1 = 11;  (* raggio per il disegno del primo cerchio della moneta *)
		raggio2 = 16; (* raggio per il disegno del secondo cerchio della moneta *)
		sizeDollaro = 10; (* dimensione del simbolo dollaro *)
		offsetY2 = -340; (* coordinata y del disegno rispetto al centro *)
		
		Table[ (* Ripete il disegno specificato all'interno *)
			AppendTo[objFinal,  (* crea l'array di monete , appendendo gli oggetti seguenti *)
			{
				Thick,Yellow,EdgeForm[{Thick, Black}], 
				Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2}, raggio2],
				Thick,Yellow,EdgeForm[{Thick, Black}], 
				Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2},raggio1],
				Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize-> sizeDollaro],
				{0 + (x*spaceXMonete) + offsetX,0+ offsetY2},Automatic ]}],
			{x,1,numMonete/3,1}  (* definizione della variabile legata alla Table ,intervallo di definizione ,  e numero di volte che deve essere ripetuto il disegno*)
		];
	(* restituisco l'array objFinal *)
	objFinal
	]
)


(* es3
* Summary: restituisce l'esempio 3, paghetta interattiva.
* Parameters: 
* Return: Manipulate dell'esempio 3
*)

getEsempioNipoti[] := (

	es3coloreNumeratore = Blue;  (* colore per identificare i denominatori*)
	es3coloreDenominatore = RGBColor["#008C00"];  (* colore per identificare i numeratori *)
	Manipulate[
		es3objMonete = es3getMoneteArray[es3numeroMonete/3];(*Chiamata alla funzione es3MoneteArray. Viene passato il valore netto per ogni nipote. Quindi il valore delle monete del nonno (es3numeroMonete) diviso i tre nipoti*)
		es3objFinal1 = es3getDivisioneMonete[es3numeroMonete, -700]; (*Chiamata alla funzione Divisione monete. Questa funzione disegna le monete che spettano ad ogni nipote. Il primo paramentro passato \[EGrave] lo stesso...*)
		es3objFinal2 = es3getDivisioneMonete[es3numeroMonete, -200]; (*...varia il secondo paramentro che indica l'offset di ascissa*)
		es3objFinal3 = es3getDivisioneMonete[es3numeroMonete, 300];
		es3objNipoti = es3getNipotiArray[3]; (* Funzione che restituisce la grafica per il disegno dei tre nipoti. *)
		es3paghetta = es3numeroMonete/3; (* variabile per il valore numerico della paghetta per ogni nipote. (Numero di monete del nonno diviso 3) *)

		Graphics[{
			Text[Style["Monete del nonno",FontSize->40, Bold, Black],{-400,450}],(*Label: "Monete del nonno"*)
			es3objNipoti,(* Array contenente gli oggetti grafici per disegnare i nipoti*)
			Text[Style["Nipoti",FontSize->40, Bold, Black],{-630,100}],
			es3objMonete,(* Array contenente oggetti grafici per disegnare le monete del nonno *)
			es3objFinal1,(* Array contenente oggetti graficiper disegnare le monete del nonno *)
			es3objFinal2,
			es3objFinal3,
			Table[Text[Style["Euro",FontSize-> 22, Bold, Black],{  x,-450}],{x, -420,580,500}],   (* Table: disegna le strighe "Euro" in basso accanto al risultato *)
			Table[Text[Style[es3paghetta,FontSize->25, Bold, Black],{x,-450}], {x, -510,490,500}],(* Table: disegna il RISULTATO da mostrare : paghetta *)
			Table[Text[Style[es3numeroMonete,FontSize->23, Bold, es3coloreNumeratore],{ x,-420}],{x,-630,380,500}],  (* Table: disegna il Numeratore della frazione in basso. Dipendente dal valore dello slider*)
			Table[Text[Style[3,FontSize->23, Bold, es3coloreDenominatore],{ x,-480}],{x,-630,380,500}], (* Table: disegna il Denominatore della frazione in basso. Numero non variabile = 3 *)
			Table[                                                                  
				{
					{Black,Thick,Line[{{x,-450},{x-45,-450}}]}, (* Disegna la linea di frazione*) 
					Text[Style["=",FontSize->23, Bold, Black],{ x+35,-450}]}, (* Disenga il simbolo "=" *)
				{x,-610,390,500} (*valori di ripetizione della table *)
			], 
			Text[Style["Ogni nipote riceve :",FontSize->25, Bold, Black],{ -35,10}],              (*Label: "Ogni nipote riceve :"*)
			{Black,Thick,Line[{{-740,40},{740,40}}]},                                            (*Linea divisoria oridzzonatale *) 
			{Black,Thick,Line[{{-280,-480},{-280,-80}}]},                                        (*Linea divisoria sinistra *)
			{Black,Thick,Line[{{210,-480},{210,-80}}]}                                           (* Linea divisoria destra *)
		},ImageSize->{800, 500}, PlotRange->{{-800,800},{-500,500}}]
		(*paramentri primo slider*)
		,{{es3numeroMonete,3,                                                                      (*valore iniziale slider numeratore*)
			Style["Numero monete",Directive[es3coloreNumeratore,Large, FontFamily -> TimesBy ]]},
			3,30,3,                                                                                (*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},
			LabelStyle->Directive[es3coloreNumeratore,Large],
			ImageSize->170
		},
		Text[Style["Numero nipoti  =  3",Directive[es3coloreDenominatore,Large, FontFamily -> TimesBy ]]], (*Label: "NUmero nipoti  =  3"*)
		TrackedSymbols:> {es3numeroMonete}
	]
)



(* es4
* Summary: restituisce l'esempio 4, differenza interattiva tra due frazioni
* Parameters: 
* Return: Manipulate dell'esempio 4
*)
getEsempioDifferenza[]:=(
	es4coloreNumeratore = Blue;
	es4coloreDenominatore = RGBColor["#008C00"];

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
		(* eseguo tutti i calcoli prima di passare alla stampa a video *)
		es4numeratoreSemplificato1 = Numerator[ es4numeratore1/es4denominatore1];
		es4denominatoreSemplificato1 = Denominator[ es4numeratore1/es4denominatore1];
		es4numeratoreSemplificato2 = Numerator[ es4numeratore2/es4denominatore2];
		es4denominatoreSemplificato2 = Denominator[es4numeratore2/es4denominatore2];
		es4frazioneSemplificata1 = es4numeratore1/es4denominatore1;
		es4frazioneSemplificata2 = es4numeratore2/es4denominatore2;
		es4mcm = LCM[es4denominatoreSemplificato1,es4denominatoreSemplificato2];
		es4numeratoreEq1= (es4mcm/es4denominatoreSemplificato1*es4numeratoreSemplificato1);
		es4numeratoreEq2= (es4mcm/es4denominatoreSemplificato2*es4numeratoreSemplificato2);
		es4numeratoreDifferenza = es4numeratoreEq1 - es4numeratoreEq2;
		es4numeratoreSemplificatoDifferenza= Numerator[es4numeratoreDifferenza/es4mcm];
		es4denominatoreSemplificatoDifferenza= Denominator[es4numeratoreDifferenza/es4mcm];
		es4TextFontSize = 15;
		es4FractionsFontSize = 40;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare. Definizione del colore *)
			{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
				n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],
				numS1=Style[es4numeratoreSemplificato1,es4coloreNumeratore],numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],
				denS1=Style[es4denominatoreSemplificato1,es4coloreDenominatore],denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore],
				nEq1=Style[es4numeratoreEq1,es4coloreNumeratore],mcm = Style[es4mcm,es4coloreDenominatore],
				nEq2=Style[es4numeratoreEq2,es4coloreNumeratore],nDif =Style[es4numeratoreDifferenza ,es4coloreNumeratore],
				numDS = Style[es4numeratoreSemplificatoDifferenza,es4coloreNumeratore],denDS = Style[es4denominatoreSemplificatoDifferenza,es4coloreDenominatore]},
			Grid[(*griglia 13x1 per organizzare il testo in righe*)
				{
					(*prima riga della griglia*)
					{Text[Style["Se vogliamo calcolare: ",FontSize->es4TextFontSize]]},
					(*seconda riga della griglia*)
					{Text[Style[StringForm["`` - ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es4FractionsFontSize]]},
					(*terza riga della griglia*)
					{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->es4TextFontSize]]},
					(*quarta riga della griglia*)
					{Text[Style["1. Ridurre ogni frazione ai minimi termini",FontSize->es4TextFontSize]]},
					(*quinta riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[denS1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]],FontSize->es4FractionsFontSize]]},
					(*sesta riga della griglia*)
					{Text[Style[StringForm["2. Trovare il minimo comune multiplo (m.c.m.) tra i ``",Style["DENOMINATORI",es4coloreDenominatore]],FontSize->es4TextFontSize]]},
					(*settima riga della griglia*)
					{Text[Style[StringForm["m.c.m.(``, ``) = ``",denS1,denS2,mcm],FontSize->20]]},
					(*ottava riga della griglia*)
					{Text[Style[StringForm["3. Trovare le frazioni equivalenti che abbiano come `` il m.c.m.",Style["DENOMINATORE",es4coloreDenominatore]],FontSize->es4TextFontSize]]},
					(*nona riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[HoldForm[nEq1]/HoldForm[mcm]],
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]]],FontSize->es4FractionsFontSize]]},
					(*decima riga della griglia*)
					{Text[Style["4. Ora sei pronto per calcolare la differenza!",FontSize->es4TextFontSize]]},
					(*undicesima riga della griglia*)
					{Text[Style[StringForm["`` - ``  =  `` - ``  = ``  =  ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[nEq1]/HoldForm[mcm]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]],HoldForm[HoldForm[nEq1-nEq2]/HoldForm[mcm]],
							HoldForm[HoldForm[nDif]/HoldForm[mcm]]],FontSize->es4FractionsFontSize]]},
			(*dodicesima riga della griglia*)
					{Text[Style["5. Nel caso fosse necessario ridurre ai minimi termini il risultato.",FontSize->es4TextFontSize]]},
					(*tredicesima riga della griglia*)
					{Text[Style[StringForm["`` = ``", HoldForm[HoldForm[nDif]/HoldForm[mcm]],numDS/denDS],FontSize->es4FractionsFontSize]]}
				},Alignment->{Left}
			]
		],{800,620}(*dimensioni pannello "pane"*)	
	],	
		{(*paramentri primo slider*)
			{
			es4numeratore1,(*variabile agganciata allo slider*)
			30,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es4coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->400
		},	
		{(*paramentri secondo slider*)
			{es4denominatore1,
			14,
			Style["denominatore prima frazione",
			Directive[es4coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->400
		},
			{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es4numeratore2,(*variabile agganciata allo slider*)
			6,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es4coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->400
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es4denominatore2,
			15,
			Style["denominatore seconda frazione",Directive[es4coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->400
		},
		TrackedSymbols:>{es4numeratore1,es4denominatore1,es4numeratore2,es4denominatore2}
	]
)



(* es5
* Summary: restituisce l'esempio 5, somma interattiva di due frazioni 
* Parameters: 
* Return: Manipulate dell'esempio 5
*)
getEsempioSomma[]:=(
	es5coloreNumeratore = Blue;
	es5coloreDenominatore = RGBColor["#008C00"];

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
		es5numeratoreSemplificato1 = Numerator[ es5numeratore1/es5denominatore1];
		es5denominatoreSemplificato1 = Denominator[ es5numeratore1/es5denominatore1];
		es5numeratoreSemplificato2 = Numerator[ es5numeratore2/es5denominatore2];
		es5denominatoreSemplificato2 = Denominator[es5numeratore2/es5denominatore2];
		es5frazioneSemplificata1 = es5numeratore1/es5denominatore1;
		es5frazioneSemplificata2 = es5numeratore2/es5denominatore2;
		es5mcm = LCM[es5denominatoreSemplificato1,es5denominatoreSemplificato2];
		es5numeratoreEq1= (es5mcm/es5denominatoreSemplificato1*es5numeratoreSemplificato1);
		es5numeratoreEq2= (es5mcm/es5denominatoreSemplificato2*es5numeratoreSemplificato2);
		es5numeratoreSomma = es5numeratoreEq1 + es5numeratoreEq2;		
		es5numeratoreSemplificatoSomma= Numerator[es5numeratoreSomma/es5mcm];
		es5denominatoreSemplificatoSomma = Denominator[es5numeratoreSomma/es5mcm];

		es5TextFontSize = 15;
		es5FractionsFontSize = 40;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
		{n1=Style[es5numeratore1,es5coloreNumeratore],d1=Style[es5denominatore1,es5coloreDenominatore],
				n2=Style[es5numeratore2,es5coloreNumeratore],d2=Style[es5denominatore2,es5coloreDenominatore],
			numS1=Style[es5numeratoreSemplificato1,es5coloreNumeratore],numS2 = Style[es5numeratoreSemplificato2,es5coloreNumeratore],
			denS1=Style[es5denominatoreSemplificato1,es5coloreDenominatore],denS2=Style[es5denominatoreSemplificato2,es5coloreDenominatore],
				nEq1=Style[es5numeratoreEq1,es5coloreNumeratore],mcm = Style[es5mcm,es5coloreDenominatore],
				nEq2=Style[es5numeratoreEq2,es5coloreNumeratore],nSom =Style[es5numeratoreSomma ,es5coloreNumeratore],
		numSS = Style[es5numeratoreSemplificatoSomma,es5coloreNumeratore],denSS = Style[es5denominatoreSemplificatoSomma,es5coloreDenominatore]},
			Grid[(*griglia 13x1 per organizzare il testo in righe*)
				{
					(*prima riga della griglia*)
					{Text[Style["Se vogliamo calcolare: ",FontSize->es5TextFontSize]]},
					(*seconda riga della griglia*)
					{Text[Style[StringForm["`` + ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es5FractionsFontSize]]},
					(*terza riga della griglia*)
					{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->es5TextFontSize]]},
					(*quarta riga della griglia*)
					{Text[Style["1. Ridurre ogni frazione ai minimi termini",FontSize->es5TextFontSize]]},
					(*quinta riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[denS1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]],FontSize->es5FractionsFontSize]]},
					(*sesta riga della griglia*)
					{Text[Style[StringForm["2. Trovare il minimo comune multiplo (m.c.m.) tra i ``",Style["DENOMINATORI",es5coloreDenominatore]],FontSize->es5TextFontSize]]},
					(*settima riga della griglia*)
					{Text[Style[StringForm["m.c.m.(``, ``) = ``",denS1,denS2,mcm],FontSize->20]]},
					(*ottava riga della griglia*)
					{Text[Style[StringForm["3. Trovare le frazioni equivalenti che abbiano come `` il m.c.m.",Style["DENOMINATORE",es5coloreDenominatore]],FontSize->es5TextFontSize]]},
					(*nona riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[HoldForm[nEq1]/HoldForm[mcm]],
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]]],FontSize->es5FractionsFontSize]]},
					(*decima riga della griglia*)
					{Text[Style["4. Ora sei pronto per calcolare la somma!",FontSize->es5TextFontSize]]},
					(*undicesima riga della griglia*)
					{Text[Style[StringForm["`` + ``  =  `` + ``  = ``  =  ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[nEq1]/HoldForm[mcm]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]],HoldForm[HoldForm[nEq1+nEq2]/HoldForm[mcm]],
							HoldForm[HoldForm[nSom]/HoldForm[mcm]]],FontSize->es5FractionsFontSize]]},
			(*dodicesima riga della griglia*)
					{Text[Style["5. Nel caso fosse necessario ridurre ai minimi termini il risultato.",FontSize->es5TextFontSize]]},
					(*tredicesima riga della griglia*)
					{Text[Style[StringForm["`` = ``", HoldForm[HoldForm[nSom]/HoldForm[mcm]],numSS/denSS],FontSize->es5FractionsFontSize]]}
				},Alignment->{Left}
			]
		],{800,620}(*dimensioni pannello "pane"*)	
	],	
		{(*paramentri primo slider*)
			{
			es5numeratore1,(*variabile agganciata allo slider*)
			8,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es5coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreNumeratore,Large],ImageSize->400
		},	
		{(*paramentri secondo slider*)
			{es5denominatore1,
			20,
			Style["denominatore prima frazione",
			Directive[es5coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreDenominatore,Large],ImageSize->400
		},
			{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es5numeratore2,(*variabile agganciata allo slider*)
			9,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es5coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreNumeratore,Large],ImageSize->400
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es5denominatore2,
			6,
			Style["denominatore seconda frazione",Directive[es5coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreDenominatore,Large],ImageSize->400
		},
		TrackedSymbols:>{es5numeratore1,es5denominatore1,es5numeratore2,es5denominatore2}
	]
)


(* es6
* Summary: restituisce l'esempio 6, prodotto interattivo tra due frazioni
* Parameters: 
* Return: Manipulate dell'esempio 6
*)
getEsempioProdotto[]:=(
	es6coloreNumeratore = Blue;
	es6coloreDenominatore = RGBColor["#008C00"];

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
	es6numeratoreSemplificato1 = Numerator[ es6numeratore1/es6denominatore1];
		es6denominatoreSemplificato1 = Denominator[ es6numeratore1/es6denominatore1];
		es6numeratoreSemplificato2 = Numerator[ es6numeratore2/es6denominatore2];
		es6denominatoreSemplificato2 = Denominator[es6numeratore2/es6denominatore2];
		es6frazioneSemplificata1 = es6numeratore1/es6denominatore1;
		es6frazioneSemplificata2 = es6numeratore2/es6denominatore2;
	
		es6prodottoNumeratori = es6numeratoreSemplificato1 * es6numeratoreSemplificato2;
		es6prodottoDenominatori= es6denominatoreSemplificato1 * es6denominatoreSemplificato2;
		es6numeratoreSemplificatoProdotto = Numerator[es6prodottoNumeratori/es6prodottoDenominatori];
		es6denominatoreSemplificatoProdotto = Denominator[es6prodottoNumeratori/es6prodottoDenominatori];
		es6TextFontSize = 15;
		es6FractionsFontSize = 40;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
		{n1=Style[es6numeratore1,es6coloreNumeratore],d1=Style[es6denominatore1,es6coloreDenominatore],
				n2=Style[es6numeratore2,es6coloreNumeratore],d2=Style[es6denominatore2,es6coloreDenominatore],
			numS1=Style[es6numeratoreSemplificato1,es6coloreNumeratore],numS2 = Style[es6numeratoreSemplificato2,es6coloreNumeratore],
			dens1=Style[es6denominatoreSemplificato1,es6coloreDenominatore],dens2=Style[es6denominatoreSemplificato2,es6coloreDenominatore],
				numS = Style[es6prodottoNumeratori,es6coloreNumeratore],dens = Style[es6prodottoDenominatori,es6coloreDenominatore],
			numSP = Style[es6numeratoreSemplificatoProdotto,es6coloreNumeratore],denSP = Style[es6denominatoreSemplificatoProdotto,es6coloreDenominatore]
		},
			Grid[(*griglia 8x1 per organizzare il testo in righe*)
				{
					(*prima riga della griglia*)
					{Text[Style["Se vogliamo calcolare: ",FontSize->es6TextFontSize]]},
					(*seconda riga della griglia*)
					{Text[Style[StringForm["`` \[Times] ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es6FractionsFontSize]]},
					(*terza riga della griglia*)
					{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->es6TextFontSize]]},
					(*quarta riga della griglia*)
					{Text[Style["1. Ridurre ogni frazione ai minimi termini",FontSize->es6TextFontSize]]},
					(*quinta riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[dens1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[dens2]]],FontSize->es6FractionsFontSize]]},
					{Text[Style["2. Ora sei pronto per calcolare il prodotto!",FontSize->es6TextFontSize]]},
					(*sesta riga della griglia*)
					{Text[Style[StringForm["`` \[Times] ``  =  `` \[Times] ``  = ``  =  ``",
				HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[numS1]/HoldForm[dens1]],HoldForm[HoldForm[numS2]/HoldForm[dens2]],
				HoldForm[HoldForm[numS1 * numS2]/HoldForm[dens1 * dens2]],
				HoldForm[HoldForm[numS]/HoldForm[dens]]],FontSize->es6FractionsFontSize]]},
			(*settima riga della griglia*)
					{Text[Style["3. Nel caso fosse necessario ridurre ai minimi termini il risultato.",FontSize->es6TextFontSize]]},
					(*ottava riga della griglia*)
					{Text[Style[StringForm["`` = ``", HoldForm[HoldForm[numS]/HoldForm[dens]],numSP/denSP],FontSize->es6FractionsFontSize]]}
				},Alignment->{Left}
			]
		],{800,450}(*dimensioni pannello "pane"*)	
	],	
		{(*paramentri primo slider*)
			{
			es6numeratore1,(*variabile agganciata allo slider*)
			18,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es6coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreNumeratore,Large],ImageSize->400
		},	
		{(*paramentri secondo slider*)
			{es6denominatore1,
			6,
			Style["denominatore prima frazione",
			Directive[es6coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreDenominatore,Large],ImageSize->400
		},
			{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es6numeratore2,(*variabile agganciata allo slider*)
			4,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es6coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreNumeratore,Large],ImageSize->400
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es6denominatore2,
			21,
			Style["denominatore seconda frazione",Directive[es6coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreDenominatore,Large],ImageSize->400
		},
		TrackedSymbols:>{es6numeratore1,es6denominatore1,es6numeratore2,es6denominatore2}
	]
)



(* es7
* Summary: restituisce l'esempio 7, divisione interattiva tra due frazioni
* Parameters: 
* Return: Manipulate dell'esempio 7
*)
getEsempioDivisione[]:=(
	es7coloreNumeratore = Blue;
	es7coloreDenominatore = RGBColor["#008C00"];

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
	es7numeratoreSemplificato1 = Numerator[ es7numeratore1/es7denominatore1];
		es7denominatoreSemplificato1 = Denominator[ es7numeratore1/es7denominatore1];
		es7numeratoreSemplificato2 = Numerator[ es7numeratore2/es7denominatore2];
		es7denominatoreSemplificato2 = Denominator[es7numeratore2/es7denominatore2];
		es7frazioneSemplificata1 = es7numeratore1/es7denominatore1;
		es7frazioneSemplificata2 = es7numeratore2/es7denominatore2;
	
	es7prodottoNumeratori = es7numeratoreSemplificato1 * es7denominatoreSemplificato2;
	es7prodottoDenominatori= es7denominatoreSemplificato1 * es7numeratoreSemplificato2;

	es7numeratoreSemplificatoProdotto = Numerator[es7prodottoNumeratori/es7prodottoDenominatori];
	es7denominatoreSemplificatoProdotto = Denominator[es7prodottoNumeratori/es7prodottoDenominatori];

		es7TextFontSize = 15;
		es7FractionsFontSize = 40;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
		{n1=Style[es7numeratore1,es7coloreNumeratore],d1=Style[es7denominatore1,es7coloreDenominatore],
				n2=Style[es7numeratore2,es7coloreNumeratore],d2=Style[es7denominatore2,es7coloreDenominatore],
			numS1=Style[es7numeratoreSemplificato1,es7coloreNumeratore],numS2 = Style[es7numeratoreSemplificato2,es7coloreNumeratore],
			dens1=Style[es7denominatoreSemplificato1,es7coloreDenominatore],denS2=Style[es7denominatoreSemplificato2,es7coloreDenominatore],
				numS = Style[es7prodottoNumeratori,es7coloreNumeratore],dens = Style[es7prodottoDenominatori,es7coloreDenominatore],
			numSP = Style[es7numeratoreSemplificatoProdotto,es7coloreNumeratore],denSP = Style[es7denominatoreSemplificatoProdotto,es7coloreDenominatore],
			numS2Inverted=Style[es7numeratoreSemplificato2,es7coloreDenominatore],denS2Inverted=Style[es7denominatoreSemplificato2,es7coloreNumeratore]
		},
			Grid[(*griglia 11x1 per organizzare il testo in righe*)
				{
					(*prima riga della griglia*)
					{Text[Style["Se vogliamo calcolare: ",FontSize->es7TextFontSize]]},
					(*seconda riga della griglia*)
					{Text[Style[StringForm["`` : ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es7FractionsFontSize]]},
					(*terza riga della griglia*)
					{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->es7TextFontSize]]},
					(*quarta riga della griglia*)
					{Text[Style["1. Ridurre ogni frazione ai minimi termini",FontSize->es7TextFontSize]]},
					(*quinta riga della griglia*)
					{Text[Style[StringForm["`` \[LeftRightArrow] ``   e   `` \[LeftRightArrow] ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[dens1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]],FontSize->es7FractionsFontSize]]},
			(*sesta riga della griglia*)
			{Text[Style["2. Calcolare la frazione inversa della frazione divisore",FontSize->es7TextFontSize]]},
			(*settima riga della griglia*)
					{Text[Style[StringForm["`` \[RightArrow] ``",
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[denS2Inverted]/HoldForm[numS2Inverted]]],FontSize->es7FractionsFontSize]]},
			
			(*ottava riga della griglia*)
					{Text[Style["3. Ora sei pronto per calcolare il quoziente!",FontSize->es7TextFontSize]]},
					(*nona riga della griglia*)
					{Text[Style[StringForm["`` : ``  =  `` \[Times] ``  = ``  =  ``",
				HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[numS1]/HoldForm[dens1]],HoldForm[HoldForm[denS2Inverted]/HoldForm[numS2Inverted]],
				HoldForm[HoldForm[numS1 * denS2Inverted]/HoldForm[dens1 * numS2Inverted]],
				HoldForm[HoldForm[numS]/HoldForm[dens]]],FontSize->es7FractionsFontSize]]},
			(*decima riga della griglia*)
					{Text[Style["4. Nel caso fosse necessario ridurre ai minimi termini il risultato.",FontSize->es7TextFontSize]]},
					(*undicesima riga della griglia*)
					{Text[Style[StringForm["`` = ``", HoldForm[HoldForm[numS]/HoldForm[dens]],numSP/denSP],FontSize->es7FractionsFontSize]]}
			},Alignment->{Left}
			]
		],{800,550}(*dimensioni pannello "pane"*)	
	],	
		{(*paramentri primo slider*)
			{
			es7numeratore1,(*variabile agganciata allo slider*)
			45,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es7coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreNumeratore,Large],ImageSize->400
		},	
		{(*paramentri secondo slider*)
			{es7denominatore1,
			18,
			Style["denominatore prima frazione",
			Directive[es7coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreDenominatore,Large],ImageSize->400
		},
			{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es7numeratore2,(*variabile agganciata allo slider*)
			15,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es7coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreNumeratore,Large],ImageSize->400
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es7denominatore2,
			9,
			Style["denominatore seconda frazione",Directive[es7coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreDenominatore,Large],ImageSize->400
		},
		TrackedSymbols:>{es7numeratore1,es7denominatore1,es7numeratore2,es7denominatore2}
	]
)


(* es9
* Summary: restituisce l'esempio 9, ricetta interattiva
* Parameters: 
* Return: Manipulate dell'esempio 9
*)
getEsempioRicetta[]:=(
	es9coloreNumeratore = Blue;
	es9coloreDenominatore = RGBColor["#008C00"];
	es9mostraSoluzione = False;

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
		es9numeratoreSemplificato1 = Numerator[ es9numeratore1/es9denominatore1];
		es9denominatoreSemplificato1 = Denominator[ es9numeratore1/es9denominatore1];
		es9numeratoreSemplificato2 = Numerator[ es9numeratore2/es9denominatore2];
		es9denominatoreSemplificato2 = Denominator[es9numeratore2/es9denominatore2];
		es9numeratoreSemplificato3 = Numerator[ es9numeratore3/es9denominatore3];
		es9denominatoreSemplificato3 = Denominator[ es9numeratore3/es9denominatore3];
		es9numeratoreSemplificato4 = Numerator[ es9numeratore4/es9denominatore4];
		es9denominatoreSemplificato4 = Denominator[es9numeratore4/es9denominatore4];

		es9kgPrimoIngrediente = ToString[N[es9numeratore1 /es9denominatore1,4]];
		es9kgSecondoIngrediente = ToString[N[es9numeratore2 /es9denominatore2,4]];
		es9kgTerzoIngrediente = ToString[N[es9numeratore3 /es9denominatore3,4]];
		es9kgQuartoIngrediente = ToString[N[es9numeratore4 /es9denominatore4,4]];
        es9resto1 = ToString[es9numeratore1 - N[es9numeratore1 /es9denominatore1,4]*es9denominatore1];

		es9TextFontSize = 16;
		es9FractionsFontSize = 24;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
			{n1=Style[es9numeratore1,es9coloreNumeratore,FontSize->es9FractionsFontSize],d1=Style[es9denominatore1,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				n2=Style[es9numeratore2,es9coloreNumeratore,FontSize->es9FractionsFontSize],d2=Style[es9denominatore2,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				n3=Style[es9numeratore3,es9coloreNumeratore,FontSize->es9FractionsFontSize],d3=Style[es9denominatore3,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				n4=Style[es9numeratore4,es9coloreNumeratore,FontSize->es9FractionsFontSize],d4=Style[es9denominatore4,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				numS1=Style[es9numeratoreSemplificato1,es9coloreNumeratore,FontSize->es9FractionsFontSize],
				numS2 = Style[es9numeratoreSemplificato2,es9coloreNumeratore,FontSize->es9FractionsFontSize],
				denS1=Style[es9denominatoreSemplificato1,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				denS2=Style[es9denominatoreSemplificato2,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				numS3=Style[es9numeratoreSemplificato3,es9coloreNumeratore,FontSize->es9FractionsFontSize],
				numS4 = Style[es9numeratoreSemplificato4,es9coloreNumeratore,FontSize->es9FractionsFontSize],
				denS3=Style[es9denominatoreSemplificato3,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				denS4=Style[es9denominatoreSemplificato4,es9coloreDenominatore,FontSize->es9FractionsFontSize],
				kg1 = Style[es9kgPrimoIngrediente,FontSize->es9FractionsFontSize],kg2 = Style[es9kgSecondoIngrediente,FontSize->es9FractionsFontSize],
				kg3 = Style[es9kgTerzoIngrediente,FontSize->es9FractionsFontSize],kg4 = Style[es9kgQuartoIngrediente,FontSize->es9FractionsFontSize]},
			
				If[!es9mostraSoluzione, (*non mostrare i calcoli*)
					Grid[(*griglia 4x1 per organizzare il testo in righe*)
						{
							(*prima riga della griglia*)
							{Text[Style[StringForm["``  del 1\[Degree] ingr.",HoldForm[HoldForm[n1]/HoldForm[d1]]],FontSize->es9TextFontSize]]},
							(*seconda riga della griglia*)
							{Text[Style[StringForm["``  del 2\[Degree] ingr.",HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es9TextFontSize]]},
							(*terza riga della griglia*)
							{Text[Style[StringForm["``  del 3\[Degree] ingr.",HoldForm[HoldForm[n3]/HoldForm[d3]]],FontSize->es9TextFontSize]]},
							(*quarta riga della griglia*)
							{Text[Style[StringForm["``  del 4\[Degree] ingr.",HoldForm[HoldForm[n4]/HoldForm[d4]]],FontSize->es9TextFontSize]]}
						},Alignment->{Left},Spacings->{0,3}
						],
						(*altrimenti mostra i calcoli*)
					Grid[(*griglia 4x1 per organizzare il testo in righe*)
						{
							(*prima riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del 1\[Degree] ingr.",HoldForm[HoldForm[n1]/HoldForm[d1]],
							HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[numS1],HoldForm[denS1],kg1],FontSize->es9TextFontSize]]},
							(*seconda riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del 2\[Degree] ingr.",HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[numS2],HoldForm[denS2],kg2],FontSize->es9TextFontSize]]},
							(*terza riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del 3\[Degree] ingr.",HoldForm[HoldForm[n3]/HoldForm[d3]],
							HoldForm[HoldForm[numS3]/HoldForm[denS3]],HoldForm[numS3],HoldForm[denS3],kg3],FontSize->es9TextFontSize]]},
							(*quarta riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del 4\[Degree] ingr.",HoldForm[HoldForm[n4]/HoldForm[d4]],
							HoldForm[HoldForm[numS4]/HoldForm[denS4]],HoldForm[numS4],HoldForm[denS4],kg4],FontSize->es9TextFontSize]]}
						},Alignment->{Left},Spacings->{0,3}
						]
				](*chiusura If*)
			](*chiusura With*)
			,{480,500}(*dimensioni pannello "pane"*)	
		],	
		Spacer[{0,10}],
		{(*paramentri primo slider*)
			{
			es9numeratore1,(*variabile agganciata allo slider*)
			8,(*valore iniziale*)
			Style["numeratore 1\[Degree] ingr.",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
		},	
		{(*paramentri secondo slider*)
			{es9denominatore1,
			20,
			Style["denominatore 1\[Degree] ingr.",
			Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es9numeratore2,(*variabile agganciata allo slider*)
			9,(*valore iniziale*)
			Style["numeratore 2\[Degree] ingr.",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es9denominatore2,
			20,
			Style["denominatore 2\[Degree] ingr.",Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		{(*paramentri quinto slider*)
			{
			es9numeratore3,(*variabile agganciata allo slider*)
			10,(*valore iniziale*)
			Style["numeratore 3\[Degree] ingr.",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
		},	
		{(*paramentri sesto slider*)
			{es9denominatore3,
			20,
			Style["denominatore 3\[Degree] ingr.",Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		{(*paramentri settimo slider, numeratore frazione 2*)
			{
			es9numeratore4,(*variabile agganciata allo slider*)
			3,(*valore iniziale*)
			Style["numeratore 4\[Degree] ingr.",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
			},
		{(*paramentri ottavo slider, denominatore frazione 2*)
			{es9denominatore4,
			20,
			Style["denominatore 4\[Degree] ingr.",Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		Button["Verifica",es9mostraSoluzione=True,BaseStyle->{"GenericButton",16,Bold}],
		Button["Nascondi soluzione",es9mostraSoluzione=False,BaseStyle->{"GenericButton",16,Bold}],
		TrackedSymbols:>{es9numeratore1,es9denominatore1,es9numeratore2,es9denominatore2,es9numeratore3,es9denominatore3,es9numeratore4,es9denominatore4,es9mostraSoluzione}
	]
)


(* Funzione per es8
* Summary: restituisce l'oggetto es8objRiga contentente una Grid con i contenuti da mostrare dentro la Manipulate.
		   Il compito di questa funzione \[EGrave] discriminare i diversi casi e confrontare due frazioni, concludendo se la prima frazione \[EGrave] maggiore, minore o ugule della seconda
* Parameters: 
	denSS1_ : denominatore della prima frazione
	denSS2_ : denominatore della seconda frazione
	numSS1_ : numeratore della prima frazione
	numSS2_ : numeratore della seconda frazione
* Return: vettore contenente una Grid con le informazioni da stampare
*)

getRisultato[denSS1_, denSS2_, numSS1_, numSS2_] := (
	es8objRiga = {};
	es8TextFontSize = 18;
    es8FractionsFontSize = 40;
	es8BigSizeResult = 50;
	es8mcm = LCM[es8denominatoreSemplificato1,es8denominatoreSemplificato2];
	(* es8numEq1 contiene il valore di numSS1 (numeratore ridotto ai minimi termini) calcolato dopo il calcolo del mcm  *)
	es8numEq1= (es8mcm/es8denominatoreSemplificato1*es8numeratoreSemplificato1);   
	(* es8numEq2 contiene il valore di numSS2 (numeratore ridotto ai minimi termini) calcolato dopo il calcolo del mcm  *)
	es8numEq2= (es8mcm/es8denominatoreSemplificato2*es8numeratoreSemplificato2);


	With[{
	(* Applicazione della built-in Style per associare i colori sopra definiti alle variabili sotto elencate *)
		labelDenominator = Style["DENOMINATORI", es8coloreDenominatore],
	labelNumerator= Style["NUMERATORI", es8coloreNumeratore],
	labelDen =  Style["DENOMINATORE",  es8coloreDenominatore],
labelNum = Style["NUMERATORE", es8coloreNumeratore],
	denominatoreS1 = Style[denSS1,es8coloreDenominatore],
	denominatoreS2 = Style[denSS2,es8coloreDenominatore],
	numeratoreS1 = Style[numSS1,es8coloreNumeratore],
	numeratoreS2 = Style[numSS2,es8coloreNumeratore],
	mcm = Style[es8mcm,es8coloreDenominatore],
	es8numeratoreEq1 = Style[es8numEq1, es8coloreNumeratore],
	es8numeratoreEq2 = Style[es8numEq2, es8coloreNumeratore],
	resultFraction1 = numSS1/denSS1,
	resultFraction2 = numSS2/ denSS2
     },
	(* Tutti i seguenti controlli contengono l'elemento es8objRiga, che contiene una Grid. La Grid (Griglia) verr\[AGrave] mostrata all'interno della Manipulate nella funzione getConfrontoFrazioni *)
	(* Se i denominatori sono uguali \[Rule] confronta i numeratori*)
	If[denSS1 == denSS2,

		(* Codice per il caso in cui il primo numeratore sia minore del secondo*)
		If[ numSS1 < numSS2,
			(*Then*)
			es8objRiga = 
			{Grid[(* Griglia per organizzare il testo in righe*)
				{
					{Text[Style[StringForm["2. Stesso ``? SI `` = ``", labelDen, denominatoreS1,denominatoreS2],FontSize->es8TextFontSize ]]},	
					{Text[Style[StringForm["Si confrontano i `` : ``  <  ``",labelNumerator, numeratoreS1,numeratoreS2],FontSize->es8TextFontSize]]},
					{Text[Style[StringForm["\nLa prima frazione \[EGrave] minore\ndella seconda frazione"],FontSize->es8BigSizeResult]]},
					{Text[Style[StringForm["`` < ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}		
				},Alignment->{Left}
			]
		},
		(* Codice per il caso in cui il primo numeratore sia maggiore del secondo*)
		If[ numSS1 > numSS2,
			(*Then*)
			es8objRiga = {
				Grid[(* Griglia per organizzare il testo in righe*)
					{
						{Text[Style[StringForm["2. Stesso ``? SI `` = ``", labelDen,  denominatoreS1,denominatoreS2],FontSize->es8TextFontSize ]]},
						{Text[Style[StringForm["Si confrontano i `` : ``  >  ``", labelNumerator,numeratoreS1,numeratoreS2],FontSize->es8TextFontSize]]},
						{Text[Style[StringForm["\nLa prima frazione \[EGrave] maggiore\ndella seconda frazione"],FontSize->es8BigSizeResult ]]},
						{Text[Style[StringForm["`` > ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}
					},Alignment->{Left}
				]
			},
			(*Else*)
			(* Codice per il caso in cui le due frazioni siano uguali*)
			If[numSS1 == numSS2,
				(*Then*)
				es8objRiga = {
					Grid[(* Griglia per organizzare il testo in righe*)
						{
							{Text[Style[StringForm["2. Stesso `` ? SI `` = ``", labelDen, denominatoreS1, denominatoreS2],FontSize->es8TextFontSize]]},	
							{Text[Style[StringForm["Stesso ``? SI ``  =  ``",labelNum, numeratoreS1,numeratoreS2],FontSize->es8TextFontSize]]},	
							{Text[Style[StringForm["\nLe due frazioni sono uguali"],FontSize->es8BigSizeResult]]},
							{Text[Style[StringForm["`` = ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}	
						},Alignment->{Left}
					]
				}
			]
		]
	],
	(*Else: il caso in cui non denS1 \[NotEqual] denS2*)
	(* Caso in cui i numeratori siano uguali e i il primo denominatore \[EGrave] minore del secondo*)
	If[(numSS1 == numSS2) && (denSS1 < denSS2), 
		es8objRiga = {
			Grid[(* Griglia per organizzare il testo in righe*)
				{
					{Text[Style[StringForm["2. Stesso ``? SI ``  =  ``", labelNum, numeratoreS1, numeratoreS2],FontSize->es8TextFontSize]]},	
					{Text[Style[StringForm["Si confrontano i `` : ``  <  ``",labelDenominator, denominatoreS1,denominatoreS2],FontSize->es8TextFontSize]]},
					{Text[Style[StringForm["\nLa prima frazione \[EGrave] maggiore\ndella seconda frazione"],FontSize->es8BigSizeResult]]},
					{Text[Style[StringForm["`` > ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}
				},Alignment->{Left}
			]
		},
		(*Else*)
		(*Caso in cui i numeratori siano uguali e i il primo denominatore \[EGrave] maggiore del secondo*)
		If[(numSS1 == numSS2) && (denSS1 > denSS2),
			es8objRiga = {
				Grid[(* Griglia per organizzare il testo in righe*)
					{
						{Text[Style[StringForm["2. Stesso ``? SI ``  =  ``", labelNum, numeratoreS1, numeratoreS2],FontSize->es8TextFontSize]]},	
						{Text[Style[StringForm["Si confrontano i `` : ``  >  ``",labelDenominator, denominatoreS1,denominatoreS2],FontSize->es8TextFontSize]]},
						{Text[Style[StringForm["\nLa prima frazione \[EGrave] minore\ndella seconda frazione"],FontSize->es8BigSizeResult]]},
						{Text[Style[StringForm["`` < ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}
					},Alignment->{Left}
				]
			},
			(*Else: Confronto tra una frazione IMPROPRIA e una frazione PROPRIA. Una frazione impropria \[EGrave] sempre maggiore di una frazione propria*)
			If[ (numSS1 > denSS1 && numSS2 < denSS2), 
				es8objRiga = {
					Grid[(* Griglia per organizzare il testo in righe*)
						{
							{Text[Style[StringForm["\nLa frazione impropria \[EGrave] sempre maggiore della frazione propria"],FontSize->es8BigSizeResult]]},
							{Text[Style[StringForm["`` > ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}
						},Alignment->{Left}
					]
				},
				(* Confronto tra una frazione PROPRIA e una frazione IMPROPRIA. Una frazione propria \[EGrave] sempre minore di una frazione impropria *)
				If[( numSS1 < denSS1 && numSS2 > denSS2),
					es8objRiga = {
						Grid[(* Griglia per organizzare il testo in righe*)
							{
								{Text[Style[StringForm["\nLa frazione propria \[EGrave] sempre minore della frazione impropria", labelNumerator],FontSize->es8BigSizeResult]]},
								{Text[Style[StringForm["`` < ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8BigSizeResult]]}
							},Alignment->{Left}
						]
					},
					(* Caso generico . In questo caso i numeratori sono diversi e i denominatori sono diversi.*)
					If[(numSS1 != numSS2 && denSS1 != denSS2),
					(*Il costrutto If seguente controlla il risultato del confronto tra le due frazioni semplificate.
						In base al risultato definisce la variabile 'segno' e assegna il carattere "<" o ">", definisce la variabile resultStr e assegna la stringa "maggiore" o "minore". Questi valori verranno mostrati nella Grid *)
						If[resultFraction1 > resultFraction2,
							segno = ">";
							resultStr = "maggiore",
							If[resultFraction1 < resultFraction2,
								segno = "<";
								resultStr = "minore"
							]
						];
						es8objRiga = {
							Grid[(* Griglia per organizzare il testo in righe. Ogni elemento costituisce una riga *)
								{
									{Text[Style[StringForm["2. Calcolare il minimo comune multiplo tra i `` : mcm(`` , ``) = ``", labelDenominator, denominatoreS1, denominatoreS2, mcm],FontSize->   es8TextFontSize]]},	{Text[Style[StringForm["e passare alle frazioni equivalenti : "],FontSize->es8TextFontSize]]},
									{Text[Style[StringForm["`` e `` ", HoldForm[HoldForm[es8numeratoreEq1]/HoldForm[mcm]], HoldForm[HoldForm[es8numeratoreEq2]/HoldForm[mcm]]],FontSize->es8FractionsFontSize]]},
									{Text[Style[StringForm["3.  Stesso ``? SI  `` = `` ", labelDen, mcm, mcm],FontSize->es8TextFontSize ]]},
{Text[Style[StringForm["Si confrontano i `` : ``  ``  ``", labelNumerator,es8numeratoreEq1,segno,es8numeratoreEq2],FontSize->es8TextFontSize]]},
									{Text[Style[StringForm["\nLa prima frazione\n\[EGrave] `` della seconda frazione", resultStr],FontSize->es8BigSizeResult]]},
									{Text[Style[StringForm["`` `` ``",HoldForm[HoldForm[numeratoreS1]/HoldForm[denominatoreS1]],segno,HoldForm[HoldForm[numeratoreS2]/HoldForm[denominatoreS2]]],FontSize->es8FractionsFontSize]]}
								},Alignment->{Left}
								]
							}
						]
					]
				]
			]
		]
	]
]

es8objRiga
)



(* es8
* Summary: restituisce l'esempio 8, confronto interattivo tra due frazioni
* Parameters: 
* Return: Manipulate dell'esempio 8
*)
getConfrontoFrazioni[]:=(
	es8coloreNumeratore = Blue;  (* colore per identificare i denominatori*)
	es8coloreDenominatore = RGBColor["#008C00"]; (* colore per identificare i numeratori *)

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
		es8numeratoreSemplificato1 = Numerator[ es8numeratore1/es8denominatore1]; 
		es8denominatoreSemplificato1 = Denominator[ es8numeratore1/es8denominatore1];
		es8numeratoreSemplificato2 = Numerator[ es8numeratore2/es8denominatore2];
		es8denominatoreSemplificato2 = Denominator[es8numeratore2/es8denominatore2];
		es8frazioneSemplificata1 = es8numeratore1/es8denominatore1;
		es8frazioneSemplificata2 = es8numeratore2/es8denominatore2;
		es8mcm = LCM[es8denominatoreSemplificato1,es8denominatoreSemplificato2];
		es8numeratoreEq1= (es8mcm/es8denominatoreSemplificato1*es8numeratoreSemplificato1);
		es8numeratoreEq2= (es8mcm/es8denominatoreSemplificato2*es8numeratoreSemplificato2);
		es8numeratoreDifferenza = es8numeratoreEq1 - es8numeratoreEq2;
		es8numeratoreSemplificatoDifferenza= Numerator[es8numeratoreDifferenza/es8mcm];
		es8denominatoreSemplificatoDifferenza= Denominator[es8numeratoreDifferenza/es8mcm];
		es8objRisultato = {};
	    (*assegnamento del risultato della funzione getRisultato. I paramentri in input sono i numeratori e i denominatori ridotti ai minimi termini.
			L'oggetto es8objRisultato contiene una Grid. Verr\[AGrave] mostrato dentro al Pane.*)
		es8objRisultato = getRisultato[es8denominatoreSemplificato1,es8denominatoreSemplificato2 ,es8numeratoreSemplificato1, es8numeratoreSemplificato2 ];

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
			{n1=Style[es8numeratore1,es8coloreNumeratore],d1=Style[es8denominatore1,es8coloreDenominatore],
			n2=Style[es8numeratore2,es8coloreNumeratore],d2=Style[es8denominatore2,es8coloreDenominatore],
			numS1=Style[es8numeratoreSemplificato1,es8coloreNumeratore],numS2 = Style[es8numeratoreSemplificato2,es8coloreNumeratore],
			denS1=Style[es8denominatoreSemplificato1,es8coloreDenominatore],denS2=Style[es8denominatoreSemplificato2,es8coloreDenominatore],
			nEq1=Style[es8numeratoreEq1,es8coloreNumeratore],mcm = Style[es8mcm,es8coloreDenominatore],
			nEq2=Style[es8numeratoreEq2,es8coloreNumeratore],nDif =Style[es8numeratoreDifferenza ,es8coloreNumeratore],
			numDS = Style[es8numeratoreSemplificatoDifferenza,es8coloreNumeratore],denDS = Style[es8denominatoreSemplificatoDifferenza,es8coloreDenominatore]},
			Grid[(*griglia per organizzare il testo in righe*)
					{(*prima riga della griglia*)
						{Text[Style["Per capire se una frazione \[EGrave] maggiore, minore o uguale di un'altra dobbiamo svolgere i seguenti passaggi:",FontSize->es8TextFontSize]]},
						(*Seconda riga della griglia*)
						{Text[Style["1. Ridurre ogni frazione ai minimi termini",FontSize->es8TextFontSize]]},
						(*terza riga della griglia*)
						{Text[Style[StringForm["`` = ``   e   `` = ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[denS1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]],FontSize->es8FractionsFontSize]]},
						es8objRisultato  (* Array che contiene il risultato *)
					},Alignment->{Left}
				]
			],{950,600}(*dimensioni pannello "pane"*)	
		],	
		{(*paramentri primo slider, es8numeratore1 \[EGrave] la variabile che indentificher\[AGrave] il numeratore della prima frazione*)
			{
			es8numeratore1,(*variabile agganciata allo slider*)
			11,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es8coloreNumeratore,Large]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es8coloreNumeratore,Large],ImageSize->450
		},	
		{(*paramentri secondo slider, es8denominatore1 \[EGrave] la variabile che indentificher\[AGrave] il denominatore della prima frazione*)
			{es8denominatore1,
			7,
			Style["denominatore prima frazione",
			Directive[es8coloreDenominatore,Large]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es8coloreDenominatore,Large],ImageSize->450
		},
			{(*paramentri terzo slider,es8numeratore1 \[EGrave] la variabile che indentificher\[AGrave] il numeratore della prima frazione *)
			{
			es8numeratore2,(*variabile agganciata allo slider*)
			11,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es8coloreNumeratore,Large]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es8coloreNumeratore,Large],ImageSize->450
			},
		{(*paramentri quarto slider, es8denominatore2 \[EGrave] la variabile che identificher\[AGrave] il denominatore della seconda frazione *)
			{es8denominatore2,
			10,
			Style["denominatore seconda frazione",Directive[es8coloreDenominatore,Large]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es8coloreDenominatore,Large],ImageSize->450
		},
		TrackedSymbols:>{es8numeratore1,es8denominatore1,es8numeratore2,es8denominatore2}
	]
)


	
End[] (* Fine parte privata del package *)

EndPackage[](* Chiusura del package*)
