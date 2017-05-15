(* ::Package:: *)

(**)

ClearAll["ProgettoFrazioni`*"];

(*info su di noi e sulla versione di Mathematica, versione del nostro package*)

BeginPackage["ProgettoFrazioni`"]

	getEsempioTipoFrazione::usage = "Funzione che restituisce l'esempio 1"
	getEsempioBottiglie::usage = "Funzione che restituisce il gioco delle bottiglie"	
	getEsempioNipoti::usage = "Funzione che restituisce il gioco delle monete"
	getNipotiArray::usage = "Funzione che restituisce le grafiche per disegnare i nipoti"

Begin["Private`"];

(*Funzioni riguardanti l'esempio 1*)

	(*
	Funzione che analizza una frazione e restituisce il suo tipo.
	Input: numeratore_, denominatore_
	Output: "frazione apparente", "frazione propria", "frazione impropria"*)
	getTipoFrazione[numeratore_,denominatore_] := (
		tipo = "frazione ";
		If[numeratore==denominatore || Mod[numeratore,denominatore]==0,tipo =tipo <>"apparente",
			If[numeratore<denominatore,tipo =tipo <>"propria",
				If[numeratore>denominatore,tipo =tipo <>"impropria"];
			]
		];
		tipo
	) 

	(*
	Funzione che restituisce l'esempio 1*)
	getEsempioTipoFrazione[] := (
	
		Manipulate[
			coloreNumeratore = Blue; (*colore per il numeratore*)
			coloreDenominatore = RGBColor["#008C00"];(*colore per il denominatore*)
			Pane[
				Text[
					Row[{With[{numeratore=numeratore,denominatore=denominatore},
					HoldForm[Style[HoldForm[numeratore],Directive[coloreNumeratore,100]]/Style[HoldForm[denominatore],Directive[coloreDenominatore,100]]]],
					Style["   \[EGrave] una ",20], Style[getTipoFrazione[numeratore,denominatore],30]}]
				],
				{600,250}, (*dimensioni pannello*)
				Alignment->Center  (*allieneamento centrale degli elementi contenuti*)
			],
			{{numeratore,
				1,(*valore iniziale slider numeratore*)
				Style["Numeratore",Directive[coloreNumeratore,Large]]
				},
				1,100,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},
				LabelStyle->Directive[coloreNumeratore,Large],
				ImageSize->400
			},(*--fine slider numeratore*)
			{{denominatore,10,Style["Denominatore",Directive[coloreDenominatore,Large]]},1,100,1,
				Appearance->{"Labeled"},
				AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
				LabelStyle->Directive[coloreDenominatore,Large],ImageSize->400}
			]
	)


(* GIOCO DELLE BOTTIGLIE*)
getBicchieri[offX_,offY_,spX_,numBottiglie_,numBicchieri_,altezzaObj_,larghezzaObj_,pieni_]:=(
objBicchieri = {};(*pulisco il vettore degli oggetti da disegnare*)
raggioBase = (larghezzaObj/2); (*raggio della base della bottiglia e del bicchiere*)
altezzaLiquido = numBottiglie /numBicchieri*altezzaObj;(*livello della bevanda per ciascun bicchiere*)
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
)

getBottiglie[offX_,offY_,spX_,numBottiglie_,altezzaObj_,larghezzaObj_,altezzaCollo_,piene_]:=(
objBottiglie = {};(*pulisco il vettore degli oggetti da disegnare*)
raggioBase = (larghezzaObj/2);(*raggio della base della bottiglia e del bicchiere*)
raggioCollo = (larghezzaObj/7);(*raggio del collo della bottiglia, 1/7 della base*)
staccoCollo = (altezzaCollo/3);(*altezza della parte conica del collo*)
For[i = 0, i< numBottiglie,i++,(*per ogni bottiglia aggiungo il suo disegno nel vettore*)
AppendTo[objBottiglie,{
If[piene,(*disegno la bevanda per prima per non nascondere le linee della bottiglia*)
{Orange,(*colore della bevanda*)
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
)

getDivisiorioGrafico[offY_,plotRange_,altezzaDivisorio_] := (
objDivisorioGrafico = {};(*pulisco il vettore degli oggetti da disegnare*)
AppendTo[objDivisorioGrafico,{
Black,
Line[{{plotRange[[1]][[1]],offY+altezzaDivisorio},{plotRange[[1]][[2]]/2,offY+altezzaDivisorio}}],
Line[{{plotRange[[1]][[2]]/2,plotRange[[1]][[1]]},{plotRange[[1]][[2]]/2,plotRange[[1]][[2]]}}],
Arrow[{{plotRange[[1]][[1]]/2,offY+altezzaDivisorio},{plotRange[[1]][[1]]/2,offY}}]
}];
objDivisorioGrafico(*return degli oggetti da disegnare*)
)

getTestoFinale[offY_,plotRange_,numBottiglie_,numBicchieri_,altezzaObj_,larghezzaObj_,vecchiVal_,coloreNum_,coloreDen_]:=(
objTesto= {};(*pulisco il vettore degli oggetti da disegnare*)
offsetXTesto = 50;
offsetXBicchiere = 100;
offsetYElementi = 50;
offsetYStep = 200;
raggioBase = larghezzaObj/2;
offXStoria = plotRange[[1]][[2]];
offYStoria = plotRange[[2]][[2]]+offsetYElementi*2;
frazioneSemplificata = numBottiglie/numBicchieri;
AppendTo[objTesto,{
Text[Style[StringForm["In questo caso ogni bicchiere \[EGrave] riempito per ``",Style[Numerator[frazioneSemplificata],coloreNum]/Style[Denominator[frazioneSemplificata],coloreDen]],20],{myPlotRange[[1]][[1]]/2,offsetY}]
}];
Do[AppendTo[objTesto,{
Text[Style[StringForm["Bottiglie: ``",vecchiVal[[i]][[1]]],20,coloreNum],{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep},{-1,0}],
Text[Style[StringForm["Bicchieri: ``",vecchiVal[[i]][[2]]],20,coloreDen],{offXStoria/2+offsetXTesto,offYStoria-i*offsetYStep-offsetYElementi},{-1,0}],
Line[{{offXStoria/2,offYStoria-i*offsetYStep-offsetYElementi*2},{offXStoria,offYStoria-i*offsetYStep-offsetYElementi*2}}],
(*disegno bevanda*)
Orange,
Rectangle[{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi+altezzaObj*(vecchiVal[[i]][[1]]/vecchiVal[[i]][[2]])}],
(*disegno bicchiere*)
Black,
Thick,
Line[{{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},{-raggioBase+offXStoria-offsetXBicchiere,altezzaObj+offYStoria-i*offsetYStep-offsetYElementi}}],(*linea sinistra*)
Line[{{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},{raggioBase+offXStoria-offsetXBicchiere,altezzaObj+offYStoria-i*offsetYStep-offsetYElementi}}],(*linea destra*)
Line[{{-raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi},{raggioBase+offXStoria-offsetXBicchiere,offYStoria-i*offsetYStep-offsetYElementi}}](*linea base*)
}],{i,Range[Length[vecchiVal]]}];
objTesto(*return degli oggetti da disegnare*)
)

getEsempioBottiglie[] := (
ClearAll[objBottiglieVuote ,objBicchieriPieni,objBottigliePiene,objBicchieriVuoti,objDivisorioGrafico,objTestoFinale,vecchiValori];
objBottiglieVuote = {};
objBicchieriPieni ={};
objBottigliePiene= {};
objBicchieriVuoti= {};
objDivisorioGrafico= {};
objTestoFinale={};
vecchiValori = {};
maxValoreX = 1000;
maxValoreY=500;
myPlotRange = {{-maxValoreX,maxValoreX},{-maxValoreY,maxValoreY}};(*{{minX,maxX},{minY,maxY}}*)
coloreNumeratore = Blue;(*colore per i vari numeratori*)
coloreDenominatore = RGBColor["#008C00"]; (*colore per i vari denumeratori*)

altezzaOggetti = 90; (*altezza della bottiglia e del bicchiere*)
larghezzaOggetti = 70;(*larghezza della bottiglia e del bicchiere*)
altezzaColloBottiglia = 25; (*altezza del collo della bottiglia*)
altezzaDivisorio = 60;(*altezza del divisorio tra tavoli*)
<<<<<<< HEAD
spaceX2 = larghezzaOggetti*2; (*distanza tra un oggetto e un altro sull'asse X*)
=======
spaceX = larghezzaOggetti*2; (*distanza tra un oggetto e un altro sull'asse X*)
>>>>>>> ad521cfd24eaaec0f2ab2dd880f76eda939624f0
stepOffsetY = altezzaOggetti*2;(*distanza tra una riga di oggetti e un'altra sull'asse Y*)
offsetXIniziale = myPlotRange[[1]][[1]]+ larghezzaOggetti; (*padding dal confine di sinistra*)
offsetYIniziale = myPlotRange[[2]][[2]] -(altezzaOggetti+altezzaColloBottiglia)*1.2;(*padding dal confine dal confine superiore*)
versa = False; (*True se viene premuto il bottone versa, False altrimenti*)
maxNumStorico = 5; (*numero massimo di valori da tenere in memoria*)

Manipulate[
(*set e reset dei padding*)
offsetX = offsetXIniziale;
offsetY = offsetYIniziale;

(*richiesta delle bottiglie da disegnare nella riga 0*)
<<<<<<< HEAD
objBottigliePiene = getBottiglie[offsetX,offsetY,spaceX2,numeroBottiglie,altezzaOggetti,larghezzaOggetti,altezzaColloBottiglia,True];

offsetY = offsetYIniziale - 1*stepOffsetY;(*spostamento alla riga 1*)
(*richiesta delle bottiglie da disegnare nella riga 1*)
objBicchieriVuoti = getBicchieri[offsetX,offsetY,spaceX2,numeroBottiglie,numeroBicchieri,altezzaOggetti,larghezzaOggetti,False];
=======
objBottigliePiene = getBottiglie[offsetX,offsetY,spaceX,numeroBottiglie,altezzaOggetti,larghezzaOggetti,altezzaColloBottiglia,True];

offsetY = offsetYIniziale - 1*stepOffsetY;(*spostamento alla riga 1*)
(*richiesta delle bottiglie da disegnare nella riga 1*)
objBicchieriVuoti = getBicchieri[offsetX,offsetY,spaceX,numeroBottiglie,numeroBicchieri,altezzaOggetti,larghezzaOggetti,False];
>>>>>>> ad521cfd24eaaec0f2ab2dd880f76eda939624f0

offsetY = offsetYIniziale - 2*stepOffsetY;(*spostamento alla riga 2*)
(*richiesta delle bottiglie da disegnare nella riga 2*)
objDivisorioGrafico = getDivisiorioGrafico[offsetY, myPlotRange,altezzaDivisorio];
If[versa  == True,(*se \[EGrave] stato premuto il bottone versa*)
If[ numeroBicchieri>=numeroBottiglie,(*se i valori degli slider sono validi*)
{
offsetY = offsetYIniziale - 3*stepOffsetY;(*spostamento alla riga 3*)
(*richiesta delle bottiglie da disegnare nella riga 3*)
<<<<<<< HEAD
objBottiglieVuote = getBottiglie[offsetX,offsetY,spaceX2,numeroBottiglie,altezzaOggetti,larghezzaOggetti,altezzaColloBottiglia,False];

offsetY = offsetYIniziale - 4*stepOffsetY;(*spostamento alla riga 4*)
(*richiesta delle bottiglie da disegnare nella riga 4*)
objBicchieriPieni = getBicchieri[offsetX,offsetY,spaceX2,numeroBottiglie,numeroBicchieri,altezzaOggetti,larghezzaOggetti,True];
=======
objBottiglieVuote = getBottiglie[offsetX,offsetY,spaceX,numeroBottiglie,altezzaOggetti,larghezzaOggetti,altezzaColloBottiglia,False];

offsetY = offsetYIniziale - 4*stepOffsetY;(*spostamento alla riga 4*)
(*richiesta delle bottiglie da disegnare nella riga 4*)
objBicchieriPieni = getBicchieri[offsetX,offsetY,spaceX,numeroBottiglie,numeroBicchieri,altezzaOggetti,larghezzaOggetti,True];
>>>>>>> ad521cfd24eaaec0f2ab2dd880f76eda939624f0

offsetY = offsetYIniziale - 4.5*stepOffsetY;(*spostamento alla riga 5*)
(*richiesta delle bottiglie da disegnare nella riga 5*)
objTestoFinale = getTestoFinale[offsetY,myPlotRange,numeroBottiglie,numeroBicchieri,altezzaOggetti,larghezzaOggetti,vecchiValori,coloreNumeratore,coloreDenominatore];
If[Length[vecchiValori]<maxNumStorico,(*se c'\[EGrave] ancora spazio in memoria*)
AppendTo[vecchiValori,{numeroBottiglie,numeroBicchieri}],(*aggiungo il valore in memoria*)
{vecchiValori = Delete[vecchiValori,1];(*candello il valore pi\[UGrave] vecchio*)
AppendTo[vecchiValori,{numeroBottiglie,numeroBicchieri}]}
];
},
MessageDialog["Attenzione! Il il numero di bicchieri deve essere superiore o uguale al numero di bottiglie"];
];
versa=False;(*imposto al bottone a rilasciato*)
];

(*disegno gli oggetti grafici ottenuti*)
Graphics[{
objBottigliePiene,
objBicchieriVuoti,
objDivisorioGrafico,
objBottiglieVuote,
objBicchieriPieni,
objTestoFinale
},PlotRange->myPlotRange,
ImageSize -> {maxValoreX,maxValoreY}]
(*paramentri dello slider numeratore*)
,{{numeroBottiglie,1,(*valore iniziale slider numeratore*)
		Style["Numero bottiglie",Directive[coloreNumeratore,Large]]},
	1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
	LabelStyle->Directive[coloreNumeratore,Large],
	ImageSize->600
	},(*--fine slider numeratore*)
(*paramentri dello slider denominatore*)
{{numeroBicchieri,2,Style["Numero bicchieri",Directive[coloreDenominatore,Large]]}
,1,10,1,
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
	LabelStyle->Directive[coloreDenominatore,Large],ImageSize->600},
(*{{versa ,False},None},*)
Button["Versa nei bicchieri",versa=True,BaseStyle->{"GenericButton",16,Bold}]
,TrackedSymbols:>{numeroBottiglie,numeroBicchieri,versa}]
)

(*GIOCO DELLE MONETE*)
getEsempioNipoti[] := (
	coloreNumeratore = Blue;
	coloreDenominatore = RGBColor["#008C00"];
	valoreNumeroMonete = 0;
<<<<<<< HEAD
	objNipoti = {};
	objMonete = {};
	objFinal1 = {};
	objFinal2 = {};
	objFinal3 = {};
	
	Manipulate[
If[ valoreNumeroMonete != numeroMonete,
{valoreNumeroMonete = numeroMonete;
=======

	Manipulate[
If[ valoreNumeroMonete != numeroMonete,
valoreNumeroMonete = numeroMonete;
>>>>>>> ad521cfd24eaaec0f2ab2dd880f76eda939624f0

objMonete = getMoneteArray[numeroMonete/3];
objFinal1 = getDivisioneMonete[numeroMonete, -700];
objFinal2 = getDivisioneMonete[numeroMonete, -200];
objFinal3 = getDivisioneMonete[numeroMonete, 300];
objNipoti = getNipotiArray[3];
paghetta = numeroMonete/3;
<<<<<<< HEAD
}]; (*Chiusura IF*)
=======
]; (*Chiusura IF*)
>>>>>>> ad521cfd24eaaec0f2ab2dd880f76eda939624f0
Graphics[{
Text[Style["Monete del nonno",FontSize->40, Bold, Black],{-400,450}], (*Label "Monete del nonno"*)
objNipoti,                                                                                                                                   (* Array contenente gli oggetti grafici per disegnare i nipoti*)
Text[Style["Nipoti",FontSize->40, Bold, Black],{-630,100}],
objMonete,                                                                                                                                   (* Array contenente oggetti grafici per disegnare le monete del nonno *)
objFinal1,                                                                                                                                   (* Array contenente oggetti graficiper disegnare le monete del nonno *)
objFinal2,
objFinal3,
Table[Text[Style["Euro",FontSize-> 22, Bold, Black],{  x,-450}],{x, -420,580,500}],   (* Table per le strighe Euro in basso accanto al risultato*)
Table[Text[Style[paghetta,FontSize->25, Bold, Black],{x,-450}], {x, -510,490,500}],(*Table il RISULTATO (Text) da mostrare : paghetta *)
Table[Text[Style[numeroMonete,FontSize->23, Bold, coloreNumeratore],{ x,-420}],{x,-630,380,500}],  (* Numeratore VARIABILE*)
Table[Text[Style[3,FontSize->23, Bold, coloreDenominatore],{ x,-480}],{x,-630,380,500}],                           (* Denominatore FISSO = 3*)
Table[                                                                  
{{Black,Thick,Line[{{x,-450},{x-45,-450}}]}, Text[Style["=",FontSize->23, Bold, Black],{ x+35,-450}]},
{x,-610,390,500}],                                                                                                                                                                  (* Linea di frazione ed uguale *)
Text[Style["Ogni nipote riceve :",FontSize->25, Bold, Black],{ -35,10}],                   (*Text "Ogni nipote riceve :"*)
{Black,Thick,Line[{{-740,40},{740,40}}]},                                                                                       (*Linea divisoria oridzzonatale *) 
{Black,Thick,Line[{{-280,-480},{-280,-80}}]},                                                                             (*Linea divisoria sinistra *)
{Black,Thick,Line[{{210,-480},{210,-80}}]}                                                                                      (* Linea divisoria destra *)
},ImageSize->{800, 500}, PlotRange->{{-800,800},{-500,500}}]
(*paramentri primo slider*)
,{{numeroMonete,3,                                                                                                                   (*valore iniziale slider numeratore*)
		Style["Numero Monete",Directive[coloreNumeratore,Large]]},
	3,30,3,                                                                                                                                          (*valore iniziale, valore finale, step di incremento/decremento slider*)
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},
	LabelStyle->Directive[coloreNumeratore,Large],
	ImageSize->170
	},
Text[Style["Numero Monete  =  3",Directive[coloreDenominatore,Large, FontFamily -> TimesBy ]]],
TrackedSymbols:> {numeroMonete}                              (*Attributo che serve a modificare il valore numeroMonete *)
]
)


(* FUNZIONE che riempre l'array objMonete. Ad ogni iterazione vengono create 3 monete.
   In base al parametro passato vengono disegnate 3*x monete.*)
getMoneteArray[y_] := (
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
)

(* FUNZIONE che riempe l'array objNipoti (3 figure) *)
getNipotiArray[x_] := (
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
)

(* Funzione per calcolare l'array di monete che spettano ad ogni nipote, in base a quante monete puo dare il nonno*)
getDivisioneMonete[numeroMonete_, offsetX_] := (
objFinal = {};
spaceXMonete = 35;
raggio1 = 11;
raggio2 = 16;
sizeDollaro = 10;
offsetY2 = -340;
(* divisione effettiva per calcolare le monete per ogni nipote  *)
y = x/3;
Table[
AppendTo[objFinal, {Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2}, raggio2],Thick,Yellow,EdgeForm[{Thick, Black}], Disk[{0 +(x*spaceXMonete) + offsetX,0+offsetY2},raggio1],Text[Style["\[Euro]",Large,Bold, Black, Thick, FontSize-> sizeDollaro], {0 + (x*spaceXMonete) + offsetX,0+ offsetY2},Automatic ]}],
{x,1,numeroMonete/3,1}];
objFinal
)



 
	
End[] (*End of *)

(**)
EndPackage[](*End of package*)



(*
https://reference.wolfram.com/workbench/index.jsp?topic=/com.wolfram.eclipse.help/html/tasks/applications/packages.html

Caricare package per renderlo visibile al notebook, le funzionalit\[AGrave] diventano come delle Built-in:
Get (va chiuso e ricaricato il kernel)
Needs  (un solo caricamento,una tantum, dovrebbe non essere necessario ricaricare il kernel no problema shadowing)

Non necessario stesso nome tra il file .m e il Package[]
Tenere tutto all'interno della stessa cartella, che poi zipperemo e consegneremo

Commentare tutto con i commenti classici

Unprotect[] e Protect[] protegge il nome del package, necessaria

Le funzionalit\[AGrave] che non devono essere viste dal .nb vanno tra Begin["Private"] e End[]
Solo il nome di tali funzioni va messo sopra la Privarte mettendo "nomeFunzione::usage"

f usage per mandare messaggi all'utente, far uscire messaggi dall'area di private

NOTE PER NOTEBOOK
Chiudi celle da menu' non con AutoCollapse[]

Sarebbe utile non rendere le mofiche salvabili

Strumento utile: Format > Options inspector
*)
