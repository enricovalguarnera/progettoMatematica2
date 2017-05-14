(* ::Package:: *)

(*
------------------------------------------------
Titolo : 
Autori : 
	|- Team matematico: Francesca Larese, Sara De Marchi
	|- Team informatico: Enrico Ceccolini, Enrico Valguarnera
Descrizione :
Creazione :
Modifica : 
------------------------------------------------
*)
ClearAll["ProgettoFrazioni`*"];

BeginPackage["ProgettoFrazioni`"]

getEsempioTipoFrazione::usage = "Funzione che restituisce il gioco delle frazioni"
getEsempioBottiglie::usage = "Funzione che restituisce il gioco delle bottiglie"
getEsempioNipoti::usage = "Funzione che restituisce il gioco dei Nipoti"
getEsempioDifferenza::usage = "Funzione che restituisce quarto gioco"

Begin["Private`"];

(* Funzione per es1
* Summary: analizza una frazione e restituisce il suo tipo ("frazione apparente" o "frazione propria" o "frazione impropria")
* Parameters: numeratore_, denominatore_
* Return: Stringa indicante il tipo della frazione
*)
es1getTipoFrazione[numeratore_,denominatore_] := (
	Module[{
	tipo = "frazione "
	},
	If[numeratore==denominatore || Mod[numeratore,denominatore]==0, (*se il numeratore \[EGrave] divisibile per il denominatore*)
		tipo =tipo <>"apparente",
		If[numeratore<denominatore, (*se il numeratore \[EGrave] inferiore al denominatore*)
			tipo =tipo <>"propria",
			If[numeratore>denominatore,(*se il numeratore \[EGrave] maggiore al denominatore*)
				tipo =tipo <>"impropria"];
		]
	];
	tipo (*ritorno del valore*)
	]
) 


(* es1
* Summary: restituisce l'esempio 1
* Parameters: 
* Return: Manipulate dell'esempio 1
*)	
getEsempioTipoFrazione[] := (
	es1coloreNumeratore = Blue;(*colore per i vari numeratori*)
	es1coloreDenominatore = RGBColor["#008C00"]; (*colore per i vari denumeratori*)
	Manipulate[(*genera una voersione interattiva del corpo*)
		Pane[ (*pannello dove scrivere la frazione e il risultato*)
			Text[Row[{With[{es1numeratore=es1numeratore,es1denominatore=es1denominatore},
				StringForm["``   \[EGrave] una ``", (*preparazione stampa stringa*)
				HoldForm[Style[HoldForm[es1numeratore],Directive[es1coloreNumeratore,100]]/
				Style[HoldForm[es1denominatore],Directive[es1coloreDenominatore,100]]],
				Style[es1getTipoFrazione[es1numeratore,es1denominatore],30]]
				]},30
				]
			],
			{600,250}, (*dimensioni pannello*)
			Alignment->Center  (*allieneamento centrale degli elementi contenuti*)
		],
		{(*primo slider*)
			{es1numeratore, (*variabile per lo slider numeratore*)
			1,(*valore iniziale slider numeratore*)
			Style["Numeratore",Directive[es1coloreNumeratore,Large]] (*label per slider*)
			},
			1,100,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"}, (*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es1coloreNumeratore,Large], (*stile label*)
			ImageSize->400
		},
		{(*secondo slider*)
			{es1denominatore,(*variabile per lo slider denominatore*)
			10,(*valore iniziale slider denominatore*)
			Style["Denominatore",Directive[es1coloreDenominatore,Large]]},
			1,100,1, (*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es1coloreDenominatore,Large], (*stile label*)
			ImageSize->400
			}
		,TrackedSymbols:>{es1numeratore,es1denominatore}(*blocco loop incondizionato della manipulate*)
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
	Module[ (*definizione parametri locali*)
		{objBicchieri = {},(*pulisco il vettore degli oggetti da disegnare*)
		raggioBase = (larghezzaObj/2), (*raggio della base della bottiglia e del bicchiere*)
		altezzaLiquido = numBottiglie /numBicchieri*altezzaObj(*livello della bevanda per ciascun bicchiere*),
		i(*variabile per ciclo*)
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
			Thick, (*linee bicchiere pi\[UGrave] spesse*)
			Line[{{-raggioBase+(i*spX)+offX,0+offY},
				{-raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea lato sinistro*)
			Line[{{raggioBase+(i*spX)+offX,0+offY},
				{raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea lato destro*)
			Line[{{-raggioBase+(i*spX)+offX,0+offY},
				{raggioBase+(i*spX)+offX,0+offY}}](*linea base*)
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
	Module[(*definizione parametri locali*)
		{objBottiglie = {},(*pulisco il vettore degli oggetti da disegnare*)
		raggioBase = (larghezzaObj/2),(*raggio della base della bottiglia e del bicchiere*)
		raggioCollo = (larghezzaObj/7),(*raggio del collo della bottiglia, 1/7 della base*)
		staccoCollo = (altezzaCollo/3),(*altezza della parte conica del collo*)
		i
		},
		For[i = 0, i< numBottiglie,i++,(*per ogni bottiglia aggiungo il suo disegno nel vettore*)
			AppendTo[objBottiglie,{
				If[piene,(*disegno prima la bevanda per non nascondere le linee dell' i-esima bottiglia*)
					{Orange,(*colore della bevanda*)
					Rectangle[{-raggioBase+(i*spX)+offX,offY},
						{raggioBase+(i*spX)+offX,altezzaObj+offY}],
					Scale[Disk[{(i*spX)+offX,altezzaObj+offY},raggioBase],{1,.5}],
					Scale[Disk[{(i*spX)+offX,offY},raggioBase],{1,.5}]
					}
				],
				(*disegno i-esima bottiglia*)
				Black,
				Scale[Circle[{(i*spX)+offX,altezzaObj+altezzaCollo+offY},raggioCollo],{1,.5}],
				Scale[Circle[{(i*spX)+offX,altezzaObj+staccoCollo+offY},raggioCollo],{1,.5}],
				Line[{{-raggioCollo+(i*spX)+offX,altezzaObj+staccoCollo+offY},
					{-raggioCollo+(i*spX)+offX,altezzaObj+altezzaCollo+offY}}],
				Line[{{raggioCollo+(i*spX)+offX,altezzaObj+staccoCollo+offY},
					{raggioCollo+(i*spX)+offX,altezzaObj+altezzaCollo+offY}}],
		
				Scale[Circle[{(i*spX)+offX,altezzaObj+offY},raggioBase],{1,.5}], (*cerchio superiore*)
				Line[{{-raggioBase+(i*spX)+offX,offY},
					{-raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea sinistra*)
				Line[{{raggioBase+(i*spX)+offX,offY},
					{raggioBase+(i*spX)+offX,altezzaObj+offY}}],(*linea destra*)
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
	Module[
		{objDivisorioGrafico = {}(*pulisco il vettore degli oggetti da disegnare*)
		},
		AppendTo[objDivisorioGrafico,{
			Black,
			Line[{{plotRange[[1]][[1]],offY+altezzaDivisorio},
				{plotRange[[1]][[2]]/2,offY+altezzaDivisorio}}],
			Line[{{plotRange[[1]][[2]]/2,plotRange[[1]][[1]]},
				{plotRange[[1]][[2]]/2,plotRange[[1]][[2]]}}],
			Arrow[{{plotRange[[1]][[1]]/2,offY+altezzaDivisorio},
				{plotRange[[1]][[1]]/2,offY}}]
			}
		];
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
	Module[ (*definizioe di variabili locali*)
		{objTesto= {},(*pulisco il vettore degli oggetti da disegnare*)
		offsetXTesto = 50,(*padding X per testo input precedenti*)
		offsetXBicchiere = 100,(*padding X per disegno bicchieri precedenti*)
		offsetYStep = 200, (*distanza Y tra gli input precedenti*)
		offsetYElementi = 50, (*distanza Y scritte degli input precedenti*)
		raggioBase = larghezzaObj/2, (*raggio della base della bottiglia e del bicchiere*)
		offXStoria = plotRange[[1]][[2]], (*offset X per testo input precedenti*)
		offYStoria = plotRange[[2]][[2]]+100, (*offset Y per testo input precedenti*)
		frazioneSemplificata = numBottiglie/numBicchieri, (*frazione senza la HoldForm*)
		i (*variabile per i cicli*)
		},
		AppendTo[objTesto,{ (*aggiunta testo valori correnti al vettore*)
			Text[Style[StringForm["In questo caso ogni bicchiere \[EGrave] riempito per ``",
				Style[Numerator[frazioneSemplificata],coloreNum]/Style[Denominator[frazioneSemplificata],coloreDen]],20]
			,{plotRange[[1]][[1]]/2,offY+15}]
		}];
		
		Do[ (*ogni ciclo aggiungo un valore precedente al vettore*)
			AppendTo[objTesto,{
				Text[Style[StringForm["Bottiglie: ``",vecchiVal[[i]][[1]]],20,coloreNum],
					{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep},{-1,0}],
				Text[Style[StringForm["Bicchieri: ``",vecchiVal[[i]][[2]]],20,coloreDen],
					{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep-offsetYElementi},{-1,0}],
				Line[{{offXStoria/2,offYStoria-i*offsetYStep-offsetYElementi*2},
					{offXStoria,offYStoria-i*offsetYStep-offsetYElementi*2}}],
				(*disegno bevanda*)
				Orange,
				Rectangle[{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},
					{raggioBase+offXStoria-offsetXBicchiere,
					offYStoria-i*offsetYStep-offsetYElementi+altezzaObj*(vecchiVal[[i]][[1]]/vecchiVal[[i]][[2]])}],
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
* Summary: restituisce l'esempio 2
* Parameters: 
* Return: Manipulate dell'esempio 2
*)
getEsempioBottiglie[] := (
	(*definizioni di vettori disegnabili dalla Graphics*)
	es2objBottiglieVuote = {}; (*vettore graficabile di bottiglie vuote*)
	es2objBicchieriPieni ={}; (*vettore graficabile di bicchieri pieni*)
	es2objBottigliePiene= {}; (*vettore graficabile di bottiglie piene*)
	es2objBicchieriVuoti= {}; (*vettore graficabile di bicchieri vuoti*)
	es2objDivisorioGrafico= {}; (*vettore graficabile di linee di divisione*)
	es2objTestoFinale={}; (*vettore graficabile di input precedenti*)
	(*fine*)
	es2vecchiValori = {}; (*vettore di input precedenti*)
	es2maxValoreX = 1000;(*massimo valore di plot per la X*)
	es2maxValoreY=500;(*massimo valore di plot per la Y*)
	es2myPlotRange = {{-es2maxValoreX,es2maxValoreX},{-es2maxValoreY,es2maxValoreY}};
	es2coloreNumeratore = Blue;(*colore per i vari numeratori*)
	es2coloreDenominatore = RGBColor["#008C00"]; (*colore per i vari denumeratori*)
	es2altezzaOggetti = 90; (*altezza della bottiglia e del bicchiere*)
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
		(*set e reset dei padding per nuovo ciclo di manipulate*)
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
				(*disegno bottiglie vuote, bicchieri con bevanda e storico*)
				{
					es2offsetY = es2offsetYIniziale - 3*es2stepOffsetY;(*spostamento alla riga 3*)
					(*richiesta delle bottiglie da disegnare nella riga 3*)
					es2objBottiglieVuote =es2getBottiglie[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,
						es2altezzaOggetti,es2larghezzaOggetti,es2altezzaColloBottiglia,False];

					es2offsetY = es2offsetYIniziale - 4*es2stepOffsetY;(*spostamento alla riga 4*)
					(*richiesta delle bottiglie da disegnare nella riga 4*)
					es2objBicchieriPieni = es2getBicchieri[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,
						es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,True];

					es2offsetY = es2offsetYIniziale - 4.5*es2stepOffsetY;(*spostamento alla riga 5*)
					(*richiesta delle bottiglie da disegnare nella riga 5*)
					es2objTestoFinale = es2getTestoFinale[es2offsetY,es2myPlotRange,es2numeroBottiglie,
						es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,es2vecchiValori,
						es2coloreNumeratore,es2coloreDenominatore];
					If[Length[es2vecchiValori]<es2maxNumStorico,(*se c'\[EGrave] ancora spazio in memoria*)
						AppendTo[es2vecchiValori,{es2numeroBottiglie,es2numeroBicchieri}],(*aggiungo il valore in memoria*)
							{es2vecchiValori = Delete[es2vecchiValori,1];(*candello il valore pi\[UGrave] vecchio*)
						AppendTo[es2vecchiValori,{es2numeroBottiglie,es2numeroBicchieri}]}
					];
				},
				MessageDialog["Attenzione! Il il numero di bicchieri deve essere superiore o uguale al numero di bottiglie"];
			];
		es2versa=False;(*imposto al bottone a rilasciato*)
		];
		(*disegno gli oggetti grafici precedentemente definiti*)
		Graphics[{
			es2objBottigliePiene,
			es2objBicchieriVuoti,
			es2objDivisorioGrafico,
			es2objBottiglieVuote,
			es2objBicchieriPieni,
			es2objTestoFinale
			},
			PlotRange->es2myPlotRange, (*range di plot*)
			ImageSize -> {es2maxValoreX,es2maxValoreY} (*dimensione del disegno*)
		]
		,{(*paramentri dello slider numeratore*)
			{es2numeroBottiglie,
			1,(*valore iniziale slider numeratore*)
			Style["Numero bottiglie",Directive[es2coloreNumeratore,Large]]},
			1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es2coloreNumeratore,Large],
			ImageSize->600 (*lunghezza slider*)
		},	
		{(*paramentri dello slider denominatore*)
			{es2numeroBicchieri,
			2,(*valore iniziale slider denominatore*)
			Style["Numero bicchieri",Directive[es2coloreDenominatore,Large]]}
			,1,10,1,
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es2coloreDenominatore,Large],
			ImageSize->600 (*lunghezza slider*)
		},
		Button["Versa nei bicchieri",es2versa=True,BaseStyle->{"GenericButton",16,Bold}],
		TrackedSymbols:>{es2numeroBottiglie,es2numeroBicchieri,es2versa}(*blocco loop incondizionato della manipulate*)
	]
)

(* FUNZIONE che riempre l'array objMonete. Ad ogni iterazione vengono create 3 monete.
   In base al parametro passato vengono disegnate 3*x monete.*)
es3getMoneteArray[y_] := (
Module[{ objMonete,spaceXMonete,offsetX , raggio1 ,raggio2 , sizeDollaro , offsetY2 , quota1,  quota2,quota3,i},
objMonete = {};
spaceXMonete = 70;
offsetX = -700;
raggio1 = 22;
raggio2 = 32;
sizeDollaro = 20;
offsetY2 = 350;
quota1 = 0; (* Le monete vengono visualizzare in colonne da 3 partendo da sinistra. Quindi ci sono 3 diverse quote.*)
quota2 = -70;
quota3 = -140;

For[i=0, i<y, i++,
AppendTo[objMonete,
{(*moneta 1*)
Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota1 +offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota1+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize->sizeDollaro], {0 + (i*spaceXMonete) + offsetX,quota1 + offsetY2 },Automatic ],
(*moneta 2*)
Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota2 +offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota2 +offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize->sizeDollaro], {0 + (i*spaceXMonete) + offsetX, quota2  + offsetY2 },Automatic ],
(*moneta 3*)
Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota3+offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(i*spaceXMonete) + offsetX,quota3+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize-> sizeDollaro], {0 + (i*spaceXMonete) + offsetX,quota3 + offsetY2 },Automatic ]}
]];
objMonete
])


(* FUNZIONE che riempe l'array objNipoti (3 figure) *)
es3getNipotiArray[x_] := (
Module[{objNipoti, spaceX, spaceY, offsetX,i},
objNipoti = {};
spaceX = 510;
spaceY = -280;
offsetX = -600;

For[i=0, i<x, i++,
AppendTo[objNipoti,
{Thickness[.005],
Line[{{0 + (i*spaceX)+offsetX,0 + spaceY},{40 + (i*spaceX)+offsetX,60+ spaceY}}],           (*Gamba destra*)
Line[{{40 + (i*spaceX)+offsetX,60+ spaceY},{80+ (i*spaceX)+offsetX,0 + spaceY}}],          (*Gamba sinistra*)
Line[{{40 + (i*spaceX)+offsetX,60 + spaceY},{40 + (i*spaceX)+offsetX,140 + spaceY}}],   (*Busto*)
Line[{{40 + (i*spaceX)+offsetX,140 + spaceY},{80 + (i*spaceX)+offsetX,100 + spaceY}}], (* Braccio destro*)
Line[{{40 + (i*spaceX)+offsetX,140 + spaceY},{0 + (i*spaceX)+offsetX,100 + spaceY}}],   (* Braccio sinistro*)
Circle[{38 + (i*spaceX)+offsetX,161 + spaceY},20.4]                                                                                     (*Testa*)
}
]];
objNipoti
])


(* Funzione per calcolare l'array di monete che spettano ad ogni nipote, in base a quante monete puo dare il nonno*)
es3getDivisioneMonete[numMonete_, offsetX_] := (
Module[{objFinal, spaceXMonete, raggio1, raggio2, sizeDollaro, offsetY2},
objFinal = {};
spaceXMonete = 35;
raggio1 = 11;
raggio2 = 16;
sizeDollaro = 10;
offsetY2 = -340;

Table[
AppendTo[objFinal, {Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize-> sizeDollaro], {0 + (x*spaceXMonete) + offsetX,0+ offsetY2},Automatic ]}],
{x,1,numMonete/3,1}];
objFinal
])


getEsempioNipoti[] := (

es3coloreNumeratore = Blue;
es3coloreDenominatore = RGBColor["#008C00"];

Manipulate[

es3objMonete = es3getMoneteArray[es3numeroMonete/3];
es3objFinal1 = es3getDivisioneMonete[es3numeroMonete, -700];
es3objFinal2 = es3getDivisioneMonete[es3numeroMonete, -200];
es3objFinal3 = es3getDivisioneMonete[es3numeroMonete, 300];
es3objNipoti = es3getNipotiArray[3];
es3paghetta = es3numeroMonete/3;

Graphics[{
Text[Style["Monete del nonno",FontSize->40, Bold, Black],{-400,450}], (*Label "Monete del nonno"*)
es3objNipoti,                                                                                                                                   (* Array contenente gli oggetti grafici per disegnare i nipoti*)
Text[Style["Nipoti",FontSize->40, Bold, Black],{-630,100}],
es3objMonete,                                                                                                                                   (* Array contenente oggetti grafici per disegnare le monete del nonno *)
es3objFinal1,                                                                                                                                   (* Array contenente oggetti graficiper disegnare le monete del nonno *)
es3objFinal2,
es3objFinal3,
Table[Text[Style["Euro",FontSize-> 22, Bold, Black],{  x,-450}],{x, -420,580,500}],   (* Table per le strighe Euro in basso accanto al risultato*)
Table[Text[Style[es3paghetta,FontSize->25, Bold, Black],{x,-450}], {x, -510,490,500}],(*Table il RISULTATO (Text) da mostrare : paghetta *)
Table[Text[Style[es3numeroMonete,FontSize->23, Bold, es3coloreNumeratore],{ x,-420}],{x,-630,380,500}],  (* Numeratore VARIABILE*)
Table[Text[Style[3,FontSize->23, Bold, es3coloreDenominatore],{ x,-480}],{x,-630,380,500}],                           (* Denominatore FISSO = 3*)
Table[                                                                  
{{Black,Thick,Line[{{x,-450},{x-45,-450}}]}, Text[Style["=",FontSize->23, Bold, Black],{ x+35,-450}]},
{x,-610,390,500}],                                                                                                                                                                  (* Linea di frazione ed uguale *)
Text[Style["Ogni nipote riceve :",FontSize->25, Bold, Black],{ -35,10}],                   (*Text "Ogni nipote riceve :"*)
{Black,Thick,Line[{{-740,40},{740,40}}]},                                                                                       (*Linea divisoria oridzzonatale *) 
{Black,Thick,Line[{{-280,-480},{-280,-80}}]},                                                                             (*Linea divisoria sinistra *)
{Black,Thick,Line[{{210,-480},{210,-80}}]}                                                                                      (* Linea divisoria destra *)
},ImageSize->{800, 500}, PlotRange->{{-800,800},{-500,500}}]

(*paramentri primo slider*)
,{{es3numeroMonete,3,                                                                                                                   (*valore iniziale slider numeratore*)
		Style["Numero monete",Directive[es3coloreNumeratore,Large, FontFamily -> TimesBy ]]},
	3,30,3,                                                                                                                                          (*valore iniziale, valore finale, step di incremento/decremento slider*)
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},
	LabelStyle->Directive[es3coloreNumeratore,Large],
	ImageSize->170
	},
Text[Style["Numero nipoti  =  3",Directive[es3coloreDenominatore,Large, FontFamily -> TimesBy ]]],
TrackedSymbols:> {es3numeroMonete}                              (*Attributo che serve a modificare il valore numeroMonete *)

])



(* es4
* Summary: restituisce l'esempio 4
* Parameters: 
* Return: Manipulate dell'esempio 4
*)
getEsempioDifferenza[]:=(
	es4coloreNumeratore = Blue;
	es4coloreDenominatore = RGBColor["#008C00"];

	Manipulate[(*interfaccia interattiva per modifica valori variabili all'interno di espressioni*)
		es4numeratoreSemplificato1 = Numerator[ es4numeratore1/es4denominatore1];
		es4denominatoreSemplificato1 = Denominator[ es4numeratore1/es4denominatore1];
		es4numeratoreSemplificato2 = Numerator[ es4numeratore2/es4denominatore2];
		es4denominatoreSemplificato2 = Denominator[es4numeratore2/es4denominatore2];
		es4frazioneSemplificata1 = es4numeratore1/es4denominatore1;
		es4frazioneSemplificata2 = es4numeratore2/es4denominatore2;
		es4mcm = LCM[es4denominatoreSemplificato1,es4denominatoreSemplificato2];
		es4numeratoreEq1= (es4mcm/es4denominatoreSemplificato1*es4numeratoreSemplificato1);
		es4numeratoreEq2= (es4mcm/es4denominatoreSemplificato2*es4numeratoreSemplificato2);
		es4numeratoreSottrazione = es4numeratoreEq1 - es4numeratoreEq2;
		es4TextFontSize = 15;
		Pane[(*interfaccia a finestra*)
			Grid[(*gliglia 14x1 per organizzare il testo*)
				{
					(*prima riga della griglia*)
					{Text[Style["Se vogliamo calcolare: ",FontSize->es4TextFontSize]]},
					(*seconda riga della griglia*)
					{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
						n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore]},
						StringForm["`` - ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]]],FontSize->40]]},
					(*terza riga della griglia*)
					{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->es4TextFontSize]]},
					(*quarta riga della griglia*)
					{Text[Style["1. ridurre ogni frazione ai minimi termini",FontSize->es4TextFontSize]]},
					(*quinta riga della griglia*)
					{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
						n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],
						numS1=Style[es4numeratoreSemplificato1,es4coloreNumeratore],denS1=Style[es4denominatoreSemplificato1,es4coloreDenominatore],
						numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore]},
						StringForm["`` = ``   e   `` = ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[denS1]],
							HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]]],FontSize->40]]},
					(*sesta riga della griglia*)
					{Text[Style["2.trovare il minimo comune multiplo (m.c.m.) tra i DENOMINATORI",FontSize->es4TextFontSize]]},
					(*settima riga della griglia*)
					{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
						n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],denS1=es4denominatoreSemplificato1,
						denS2=es4denominatoreSemplificato2},StringForm["m.c.m.(``, ``) = ``",denS1,denS2,es4mcm]],FontSize->30]]},
					(*ottava riga della griglia*)
					{Text[Style["3. trovare le frazioni equivalenti che abbiano come DENOMINATORE il m.c.m.",FontSize->es4TextFontSize]]},
					(*nona riga della griglia*)
					{Text[Style[With[{n1=Style[es4numeratoreEq1,es4coloreNumeratore],n2=Style[es4numeratoreEq2,es4coloreNumeratore],
						mcm=Style[es4mcm,es4coloreDenominatore],numS1=Style[es4numeratoreSemplificato1,es4coloreNumeratore],
						denS1=Style[es4denominatoreSemplificato1,es4coloreDenominatore],numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],
						denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore]},
						StringForm["`` = ``   e   `` = ``",HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[HoldForm[n1]/HoldForm[mcm]],
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[n2]/HoldForm[mcm]]]],FontSize->40]]},
					(*decima riga della griglia*)
					{Text[Style["4. Ora sei pronto per calcolare la differenza!",FontSize->es4TextFontSize]]},
					(*undicesima riga della griglia*)
					{Text[Style["Il risultato della somma \[EGrave] una frazione che ha:",FontSize->es4TextFontSize]]},
					(*dodicesima riga della griglia*)
					{Text[Style["-come NUMERATORE la differenza dei NUMERATORI delle frazioni equivalenti trovate nel punto 3;",
						FontSize->es4TextFontSize]]},
					(*tredicesima riga della griglia*)
					{Text[Style["-come DENOMINATORE il m.c.m. trovato nel punto 2.",FontSize->es4TextFontSize]]},
					(*quattordicesima riga della griglia*)
					{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
						n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],
						nEq1=Style[es4numeratoreEq1,es4coloreNumeratore],mcm = Style[es4mcm,es4coloreDenominatore],
						nEq2=Style[es4numeratoreEq2,es4coloreNumeratore],nSot =Style[es4numeratoreSottrazione ,es4coloreNumeratore]},
						StringForm["`` - ``  =  `` - ``  = ``  =  ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[nEq1]/HoldForm[mcm]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]],HoldForm[HoldForm[nEq1-nEq2]/HoldForm[mcm]],
							HoldForm[HoldForm[nSot]/HoldForm[mcm]]]],FontSize->40]]}
				}
				,Alignment->{Left}
			],
			{1000,550}(*dimensioni pannello "pane"*)
		],	
		{(*paramentri primo slider*)
			{
			es4numeratore1,(*variabile agganciata allo slider*)
			30,(*valore iniziale*)
			Style["numeratore prima frazione",Directive[es4coloreNumeratore,Large]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600
		},	
		{(*paramentri secondo slider*)
			{es4denominatore1,
			14,
			Style["denominatore prima frazione",
			Directive[es4coloreDenominatore,Large]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600
		},
			{(*paramentri terzo slider, numeratore frazione 2*)
			{
			es4numeratore2,(*variabile agganciata allo slider*)
			6,(*valore iniziale*)
			Style["numeratore seconda frazione",Directive[es4coloreNumeratore,Large]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es4denominatore2,
			15,
			Style["denominatore seconda frazione",Directive[es4coloreDenominatore,Large]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600
		},
		TrackedSymbols:>{es4numeratore1,es4denominatore1,es4numeratore2,es4denominatore2}
	]
)


	
End[] (*End of *)

(**)
EndPackage[](*End of package*)
