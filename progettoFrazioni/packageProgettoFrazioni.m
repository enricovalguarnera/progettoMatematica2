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

getEsempioTipoFrazione::usage = "Funzione che restituisce l'esempio interattivo sul tipo della frazione"
getEsempioBottiglie::usage = "Funzione che restituisce l'esempio interattivo con bicchieri e bottiglie"
getEsempioNipoti::usage = "Funzione che restituisce l'esempio interattivo della paghetta"
getEsempioDifferenza::usage = "Funzione che restituisce l'esempio interattivo di differenza tra frazioni"
getSchemeCell1::usage = "Funzione che mostra il contenuto della prima cella"
getEsempioProdotto::usage = "Funzione che restituisce l'esempio interattivo di prodotto tra frazioni"
getEsempioSomma::usage = "Funzione che restituisce l'esempio interattivo di somma tra frazioni"
getEsempioDivisione::usage = "Funzione che restituisce l'esempio interattivo di divisione tra frazioni" 
getEsempioRicetta::usage = "Funzione che restituisce l'esempio interattivo della ricetta"

Begin["Private`"];

(*
	Funzione che analizza una frazione e restituisce il suo tipo.
	Input: numeratore_, denominatore_
	Output: "frazione apparente", "frazione propria", "frazione impropria"*)
es1getTipoFrazione[numeratore_,denominatore_] := (
	Module[{tipo = "frazione "},
		If[numeratore==denominatore || Mod[numeratore,denominatore]==0,tipo =tipo <>"apparente",
			If[numeratore<denominatore,tipo =tipo <>"propria",
				If[numeratore>denominatore,tipo =tipo <>"impropria"];
			]
		];
		tipo
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
		Manipulate[
			Pane[
				Text[
					Row[{With[{es1numeratore=es1numeratore,es1denominatore=es1denominatore},
					HoldForm[Style[HoldForm[es1numeratore],Directive[es1coloreNumeratore,100]]/Style[HoldForm[es1denominatore],Directive[es1coloreDenominatore,100]]]],
					Style["   \[EGrave] una ",20], Style[es1getTipoFrazione[es1numeratore,es1denominatore],30]}]
				],
				{600,250}, (*dimensioni pannello*)
				Alignment->Center  (*allieneamento centrale degli elementi contenuti*)
			],
			{{es1numeratore,
				1,(*valore iniziale slider numeratore*)
				Style["Numeratore",Directive[es1coloreNumeratore,Large]]
				},
				1,100,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},
				LabelStyle->Directive[es1coloreNumeratore,Large],
				ImageSize->400
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
* Summary: restituisce l'esempio 2
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
	es2maxValoreY=520;(*massimo valore di plot per la Y*)
	es2myPlotRange = {{-es2maxValoreX,es2maxValoreX},{-es2maxValoreY,es2maxValoreY}};(*{{minX,maxX},{minY,maxY}}*)
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
			MessageDialog["Attenzione! Il il numero di bicchieri deve essere superiore o uguale al numero di bottiglie"];
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
			ImageSize -> {es2maxValoreX,es2maxValoreY}],
			(*paramentri dello slider numeratore*)
			{{es2numeroBottiglie,1,(*valore iniziale slider numeratore*)
				Style["Numero bottiglie",Directive[es2coloreNumeratore,Large]]},
				1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
				LabelStyle->Directive[es2coloreNumeratore,Large],
				ImageSize->600
			},(*--fine slider numeratore*)
			(*paramentri dello slider denominatore*)
			{{es2numeroBicchieri,2,Style["Numero bicchieri",Directive[es2coloreDenominatore,Large]]}
			,1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
			Appearance->{"Labeled"},
			AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
			LabelStyle->Directive[es2coloreDenominatore,Large],ImageSize->600},
			Button["Versa nei bicchieri",es2versa=True,BaseStyle->{"GenericButton",16,Bold}]
			,TrackedSymbols:>{es2numeroBottiglie,es2numeroBicchieri,es2versa}
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
		es4numeratoreDifferenza = es4numeratoreEq1 - es4numeratoreEq2;
		es4numeratoreSemplificatoDifferenza= Numerator[es4numeratoreDifferenza/es4mcm];
		es4denominatoreSemplificatoDifferenza= Denominator[es4numeratoreDifferenza/es4mcm];
		es4TextFontSize = 15;
		es4FractionsFontSize = 40;

		Pane[(*interfaccia a finestra*)
		With[(*specifica delle occorrenze per le variabili da stampare*)
			{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],
				n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],
				numS1=Style[es4numeratoreSemplificato1,es4coloreNumeratore],numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],
				denS1=Style[es4denominatoreSemplificato1,es4coloreDenominatore],denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore],
				nEq1=Style[es4numeratoreEq1,es4coloreNumeratore],mcm = Style[es4mcm,es4coloreDenominatore],
				nEq2=Style[es4numeratoreEq2,es4coloreNumeratore],nDif =Style[es4numeratoreDifferenza ,es4coloreNumeratore],
				numDS = Style[es4numeratoreSemplificatoDifferenza,es4coloreNumeratore],denDS = Style[es4denominatoreSemplificatoDifferenza,es4coloreDenominatore]},
			Grid[(*gliglia 13x1 per organizzare il testo in righe*)
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
		],{1000,600}(*dimensioni pannello "pane"*)	
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
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600
		},	
		{(*paramentri secondo slider*)
			{es4denominatore1,
			14,
			Style["denominatore prima frazione",
			Directive[es4coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600
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
			LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es4denominatore2,
			15,
			Style["denominatore seconda frazione",Directive[es4coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600
		},
		TrackedSymbols:>{es4numeratore1,es4denominatore1,es4numeratore2,es4denominatore2}
	]
)



(* es5
* Summary: restituisce l'esempio 5
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

		es5TextFontSize = 18;
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
			Grid[(*gliglia 13x1 per organizzare il testo in righe*)
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
					{Text[Style[StringForm["2.Trovare il minimo comune multiplo (m.c.m.) tra i ``",Style["DENOMINATORI",es5coloreDenominatore]],FontSize->es5TextFontSize]]},
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
		],{1000,600}(*dimensioni pannello "pane"*)	
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
			LabelStyle->Directive[es5coloreNumeratore,Large],ImageSize->600
		},	
		{(*paramentri secondo slider*)
			{es5denominatore1,
			20,
			Style["denominatore prima frazione",
			Directive[es5coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreDenominatore,Large],ImageSize->600
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
			LabelStyle->Directive[es5coloreNumeratore,Large],ImageSize->600
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es5denominatore2,
			6,
			Style["denominatore seconda frazione",Directive[es5coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es5coloreDenominatore,Large],ImageSize->600
		},
		TrackedSymbols:>{es5numeratore1,es5denominatore1,es5numeratore2,es5denominatore2}
	]
)


(* es6
* Summary: restituisce l'esempio 6
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
			Grid[(*gliglia 13x1 per organizzare il testo in righe*)
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
		],{1000,450}(*dimensioni pannello "pane"*)	
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
			LabelStyle->Directive[es6coloreNumeratore,Large],ImageSize->600
		},	
		{(*paramentri secondo slider*)
			{es6denominatore1,
			6,
			Style["denominatore prima frazione",
			Directive[es6coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreDenominatore,Large],ImageSize->600
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
			LabelStyle->Directive[es6coloreNumeratore,Large],ImageSize->600
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es6denominatore2,
			21,
			Style["denominatore seconda frazione",Directive[es6coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es6coloreDenominatore,Large],ImageSize->600
		},
		TrackedSymbols:>{es6numeratore1,es6denominatore1,es6numeratore2,es6denominatore2}
	]
)



(* es7
* Summary: restituisce l'esempio 7
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
			numSP = Style[es7numeratoreSemplificatoProdotto,es7coloreNumeratore],denSP = Style[es7denominatoreSemplificatoProdotto,es7coloreDenominatore]
		},
			Grid[(*gliglia 13x1 per organizzare il testo in righe*)
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
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[denS2]/HoldForm[numS2]]],FontSize->es7FractionsFontSize]]},
			
			(*ottava riga della griglia*)
					{Text[Style["3. Ora sei pronto per calcolare il quoziente!",FontSize->es7TextFontSize]]},
					(*nona riga della griglia*)
					{Text[Style[StringForm["`` : ``  =  `` \[Times] ``  = ``  =  ``",
				HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[numS1]/HoldForm[dens1]],HoldForm[HoldForm[denS2]/HoldForm[numS2]],
				HoldForm[HoldForm[numS1 * denS2]/HoldForm[dens1 * numS2]],
				HoldForm[HoldForm[numS]/HoldForm[dens]]],FontSize->es7FractionsFontSize]]},
			(*decima riga della griglia*)
					{Text[Style["4. Nel caso fosse necessario ridurre ai minimi termini il risultato.",FontSize->es7TextFontSize]]},
					(*undicesima riga della griglia*)
					{Text[Style[StringForm["`` = ``", HoldForm[HoldForm[numS]/HoldForm[dens]],numSP/denSP],FontSize->es7FractionsFontSize]]}
			},Alignment->{Left}
			]
		],{1000,550}(*dimensioni pannello "pane"*)	
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
			LabelStyle->Directive[es7coloreNumeratore,Large],ImageSize->600
		},	
		{(*paramentri secondo slider*)
			{es7denominatore1,
			18,
			Style["denominatore prima frazione",
			Directive[es7coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreDenominatore,Large],ImageSize->600
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
			LabelStyle->Directive[es7coloreNumeratore,Large],ImageSize->600
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es7denominatore2,
			9,
			Style["denominatore seconda frazione",Directive[es7coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es7coloreDenominatore,Large],ImageSize->600
		},
		TrackedSymbols:>{es7numeratore1,es7denominatore1,es7numeratore2,es7denominatore2}
	]
)


(* es9
* Summary: restituisce l'esempio 9
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

		es9kgPrimoIngrediente = N[es9numeratore1 /es9denominatore1,4];
		es9kgSecondoIngrediente = N[es9numeratore2 /es9denominatore2,4];
		es9kgTerzoIngrediente = N[es9numeratore3 /es9denominatore3,4];
		es9kgQuartoIngrediente = N[es9numeratore4 /es9denominatore4,4];

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
					Grid[(*gliglia 4x1 per organizzare il testo in righe*)
						{
							(*prima riga della griglia*)
							{Text[Style[StringForm["``  del primo ingrediente",HoldForm[HoldForm[n1]/HoldForm[d1]]],FontSize->es9TextFontSize]]},
							(*seconda riga della griglia*)
							{Text[Style[StringForm["``  del secondo ingrediente",HoldForm[HoldForm[n2]/HoldForm[d2]]],FontSize->es9TextFontSize]]},
							(*terza riga della griglia*)
							{Text[Style[StringForm["``  del terzo ingrediente",HoldForm[HoldForm[n3]/HoldForm[d3]]],FontSize->es9TextFontSize]]},
							(*quarta riga della griglia*)
							{Text[Style[StringForm["``  del quarto ingrediente",HoldForm[HoldForm[n4]/HoldForm[d4]]],FontSize->es9TextFontSize]]}
						},Alignment->{Left},Spacings->{0,5}
						],
						(*altrimenti mostra i calcoli*)
					Grid[(*gliglia 4x1 per organizzare il testo in righe*)
						{
							(*prima riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del primo ingrediente",HoldForm[HoldForm[n1]/HoldForm[d1]],
							HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[numS1],HoldForm[denS1],kg1],FontSize->es9TextFontSize]]},
							(*seconda riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del secondo ingrediente",HoldForm[HoldForm[n2]/HoldForm[d2]],
							HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[numS2],HoldForm[denS2],kg2],FontSize->es9TextFontSize]]},
							(*terza riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del terzo ingrediente",HoldForm[HoldForm[n3]/HoldForm[d3]],
							HoldForm[HoldForm[numS3]/HoldForm[denS3]],HoldForm[numS3],HoldForm[denS3],kg3],FontSize->es9TextFontSize]]},
							(*quarta riga della griglia*)
							{Text[Style[StringForm["`` \[LeftRightArrow] `` = `` : `` = `` kg del quarto ingrediente",HoldForm[HoldForm[n4]/HoldForm[d4]],
							HoldForm[HoldForm[numS4]/HoldForm[denS4]],HoldForm[numS4],HoldForm[denS4],kg4],FontSize->es9TextFontSize]]}
						},Alignment->{Left},Spacings->{0,5}
						]
				](*chiusura If*)
			](*chiusura With*)
			,{550,500}(*dimensioni pannello "pane"*)	
		],	
		Spacer[{0,10}],
		{(*paramentri primo slider*)
			{
			es9numeratore1,(*variabile agganciata allo slider*)
			8,(*valore iniziale*)
			Style["numeratore primo ingrediente",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
		},	
		{(*paramentri secondo slider*)
			{es9denominatore1,
			20,
			Style["denominatore primo ingrediente",
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
			Style["numeratore secondo ingrediente",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
			},
		{(*paramentri quarto slider, denominatore frazione 2*)
			{es9denominatore2,
			20,
			Style["denominatore secondo ingrediente",Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		{(*paramentri quinto slider*)
			{
			es9numeratore3,(*variabile agganciata allo slider*)
			10,(*valore iniziale*)
			Style["numeratore primo ingrediente",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
		},	
		{(*paramentri sesto slider*)
			{es9denominatore3,
			20,
			Style["denominatore primo ingrediente",Directive[es9coloreDenominatore,FontSize->13]]},
			1,100,1,
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreDenominatore,Large],ImageSize->200
		},
		Spacer[{0,50}],
		{(*paramentri settimo slider, numeratore frazione 2*)
			{
			es9numeratore4,(*variabile agganciata allo slider*)
			3,(*valore iniziale*)
			Style["numeratore secondo ingrediente",Directive[es9coloreNumeratore,FontSize->13]](*etichetta dello slider*)
			},
			1,100,(*range*)
			1,(*step di modifica*)
			Appearance->"Labeled",
			LabelStyle->Directive[es9coloreNumeratore,Large],ImageSize->200
			},
		{(*paramentri ottavo slider, denominatore frazione 2*)
			{es9denominatore4,
			20,
			Style["denominatore secondo ingrediente",Directive[es9coloreDenominatore,FontSize->13]]},
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


getSchemeCell1[] :=( 
Module[{image1,image2, image3, testo1, testo2},
image1 = Import["C:/Users/enry_/Desktop/Alma Mater Studiorum/MatComputazionale/progettoMatematica2/progettoFrazioni/mezzo_litro.jpg"];

image2 = Import["C:/Users/enry_/Desktop/Alma Mater Studiorum/MatComputazionale/progettoMatematica2/progettoFrazioni/pizza.jpg"];
image3 = Import["C:/Users/enry_/Desktop/Alma Mater Studiorum/MatComputazionale/progettoMatematica2/progettoFrazioni/niente_paura.png"];

testo1 = "Queste frasi in realt\[AGrave] derivano dal linguaggio matematico, e in particolare dalle FRAZIONI!
Non hai mai visto una frazione? Le hai studiate ma non le hai capite bene?";

testo2 = "Alla fine di questo tutorial ti sentirai fortissimo nel calcolo con le frazioni, perch\[EAcute] si, le frazioni sono numeri... Ma andiamo con calma! Non mettiamo fretta! In questo tutorial, i guideremo passo passo nella scoperta del mondo delle frazioni: impareremo a fare calcoli con esse, a confrontarle e a risolvere tantissimi problemi, anche di vita quotidiana! Ti insegneremo infine quando conviene lasciare la frazione o quando invece e necessario ricondursi alla forma razionale.

Buon lavoro!";
Grid[{
(*prima riga*){image1,testo1,image2} 
}
]

Grid[{
	{image3, testo2}
},Alignment->{Center, Center}]

]

)


	
End[] (*End of *)

(**)
EndPackage[](*End of package*)
