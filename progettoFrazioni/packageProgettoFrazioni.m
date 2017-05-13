(* ::Package:: *)

ClearAll["ProgettoFrazioni`*"];

BeginPackage["ProgettoFrazioni`"]

getEsempioTipoFrazione::usage = "Funzione che restituisce il gioco delle frazioni"
getEsempioBottiglie::usage = "Funzione che restituisce il gioco delle bottiglie"
getEsempioNipoti::usage = "Funzione che restituisce il gioco dei Nipoti"
getEsempioDifferenza::usage = "Funzione che restituisce quarto gioco"

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


(*
	Funzione che restituisce l'esempio 1*)
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




es2getBicchieri[offX_,offY_,spX_,numBottiglie_,numBicchieri_,altezzaObj_,larghezzaObj_,pieni_]:=(
Module[
{objBicchieri = {},(*pulisco il vettore degli oggetti da disegnare*)
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


es2getBottiglie[offX_,offY_,spX_,numBottiglie_,altezzaObj_,larghezzaObj_,altezzaCollo_,piene_]:=(
Module[{objBottiglie = {},(*pulisco il vettore degli oggetti da disegnare*)
raggioBase = (larghezzaObj/2),(*raggio della base della bottiglia e del bicchiere*)
raggioCollo = (larghezzaObj/7),(*raggio del collo della bottiglia, 1/7 della base*)
staccoCollo = (altezzaCollo/3),(*altezza della parte conica del collo*)
i},

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
])

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
])

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
Text[Style[StringForm["In questo caso ogni bicchiere \[EGrave] riempito per ``",Style[Numerator[frazioneSemplificata],coloreNum]/Style[Denominator[frazioneSemplificata],coloreDen]],20],{plotRange[[1]][[1]]/2,offY}]
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
])


getEsempioBottiglie[] := (
es2objBottiglieVuote = {};
es2objBicchieriPieni ={};
es2objBottigliePiene= {};
es2objBicchieriVuoti= {};
es2objDivisorioGrafico= {};
es2objTestoFinale={};
es2vecchiValori = {};
es2maxValoreX = 1000;(*massimo valore di plot per la X*)
es2maxValoreY=500;(*massimo valore di plot per la Y*)
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
es2objBottigliePiene = es2getBottiglie[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,es2altezzaOggetti,es2larghezzaOggetti,es2altezzaColloBottiglia,True];

es2offsetY = es2offsetYIniziale - 1*es2stepOffsetY;(*spostamento alla riga 1*)
(*richiesta delle bottiglie da disegnare nella riga 1*)
es2objBicchieriVuoti = es2getBicchieri[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,False];

es2offsetY = es2offsetYIniziale - 2*es2stepOffsetY;(*spostamento alla riga 2*)
(*richiesta delle bottiglie da disegnare nella riga 2*)
es2objDivisorioGrafico = es2getDivisiorioGrafico[es2offsetY, es2myPlotRange,es2altezzaDivisorio];
If[es2versa  == True,(*se \[EGrave] stato premuto il bottone versa*)
If[ es2numeroBicchieri>=es2numeroBottiglie,(*se i valori degli slider sono validi*)
{
es2offsetY = es2offsetYIniziale - 3*es2stepOffsetY;(*spostamento alla riga 3*)
(*richiesta delle bottiglie da disegnare nella riga 3*)
es2objBottiglieVuote =es2getBottiglie[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,es2altezzaOggetti,es2larghezzaOggetti,es2altezzaColloBottiglia,False];

es2offsetY = es2offsetYIniziale - 4*es2stepOffsetY;(*spostamento alla riga 4*)
(*richiesta delle bottiglie da disegnare nella riga 4*)
es2objBicchieriPieni = es2getBicchieri[es2offsetX,es2offsetY,es2spaceX,es2numeroBottiglie,es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,True];

es2offsetY = es2offsetYIniziale - 4.5*es2stepOffsetY;(*spostamento alla riga 5*)
(*richiesta delle bottiglie da disegnare nella riga 5*)
es2objTestoFinale = es2getTestoFinale[es2offsetY,es2myPlotRange,es2numeroBottiglie,es2numeroBicchieri,es2altezzaOggetti,es2larghezzaOggetti,es2vecchiValori,es2coloreNumeratore,es2coloreDenominatore];
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
ImageSize -> {es2maxValoreX,es2maxValoreY}]
(*paramentri dello slider numeratore*)
,{{es2numeroBottiglie,1,(*valore iniziale slider numeratore*)
		Style["Numero bottiglie",Directive[es2coloreNumeratore,Large]]},
	1,10,1,(*valore iniziale, valore finale, step di incremento/decremento slider*)
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
	LabelStyle->Directive[es2coloreNumeratore,Large],
	ImageSize->600
	},(*--fine slider numeratore*)
(*paramentri dello slider denominatore*)
{{es2numeroBicchieri,2,Style["Numero bicchieri",Directive[es2coloreDenominatore,Large]]}
,1,10,1,
	Appearance->{"Labeled"},
	AppearanceElements->{"InputField"},(*rimuovo tutto tranne l'inputField*)
	LabelStyle->Directive[es2coloreDenominatore,Large],ImageSize->600},
(*{{versa ,False},None},*)
Button["Versa nei bicchieri",es2versa=True,BaseStyle->{"GenericButton",16,Bold}]
,TrackedSymbols:>{es2numeroBottiglie,es2numeroBicchieri,es2versa}]

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



(* Esempio 4 *)

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

(*numS1 = es3numeratoreSemplificato1,denS1 = es3denominatoreSemplificato1,numS2 = es3numeratoreSemplificato2,denS2=es3denominatoreSemplificato2*)

Pane[(*interfaccia a finestra*)
Grid[
{
(*prima riga*)
{Text[Style["Se vogliamo calcolare: ",FontSize->20]]},
(*seconda riga*)
{
Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore]},StringForm["`` - ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]]]],FontSize->40]]
},
(*terza riga*)
{Text[Style["dobbiamo svolgere i seguenti passaggi:",FontSize->20]]},
(*quarta riga*)
{Text[Style["1. ridurre ogni frazione ai minimi termini",FontSize->20]]},
(*quinta riga*)
{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],numS1 = Style[es4numeratoreSemplificato1,es4coloreNumeratore],denS1 = Style[es4denominatoreSemplificato1,es4coloreDenominatore],numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore]},StringForm["`` = ``   e   `` = ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[numS1]/HoldForm[denS1]] ,HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[numS2]/HoldForm[denS2]]]],FontSize->40]]},
(*sesta riga*)
{Text[Style["2.trovare il minimo comune multiplo (m.c.m.) tra i \!\(\*
StyleBox[\"DENOMINATORI\",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0., 0.6666666666666666, 0.]]\)",FontSize->20]]},
(*settima riga*)
{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],denS1 =es4denominatoreSemplificato1,denS2=es4denominatoreSemplificato2},StringForm["m.c.m.(``, ``) = ``",denS1,denS2,es4mcm]],FontSize->30]]},
(*ottava riga*)
{Text[Style["3. trovare le frazioni equivalenti che abbiano come \!\(\*
StyleBox[\"DENOMINATORE\",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0., 0.6666666666666666, 0.]]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\",\nFontColor->RGBColor[1, 0.5, 0.5]]\)il m.c.m.",FontSize->20]]},
(*nona riga*)
{Text[Style[With[{n1=Style[es4numeratoreEq1,es4coloreNumeratore],n2=Style[es4numeratoreEq2,es4coloreNumeratore],mcm = Style[es4mcm,es4coloreDenominatore],numS1 = Style[es4numeratoreSemplificato1,es4coloreNumeratore],denS1 = Style[es4denominatoreSemplificato1,es4coloreDenominatore],numS2 = Style[es4numeratoreSemplificato2,es4coloreNumeratore],denS2=Style[es4denominatoreSemplificato2,es4coloreDenominatore]},StringForm["`` = ``   e   `` = ``",HoldForm[HoldForm[numS1]/HoldForm[denS1]],HoldForm[HoldForm[n1]/HoldForm[mcm]],HoldForm[HoldForm[numS2]/HoldForm[denS2]],HoldForm[HoldForm[n2]/HoldForm[mcm]]]],FontSize->40]]},
(*decima riga*)
{Text[Style["4. Ora sei pronto per calcolare la differenza!",FontSize->20]]},
(*undicesima riga*)
{Text[Style["\!\(\*
StyleBox[\"Il\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"risultato\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"della\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"somma\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"\[EGrave]\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"una\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"frazione\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"che\",\nFontSize->16]\)\!\(\*
StyleBox[\" \",\nFontSize->16]\)\!\(\*
StyleBox[\"ha\",\nFontSize->16]\)\!\(\*
StyleBox[\":\",\nFontSize->16]\)",FontSize->20]]},
(*dodicesima riga*)
{Text[Style["-come \!\(\*
StyleBox[\"NUMERATORE\",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0, 0, 1]]\) la differenza dei \!\(\*
StyleBox[\"NUMERATORI\",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0, 0, 1]]\)\!\(\*
StyleBox[\" \",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0, 0, 1]]\)delle frazioni equivalenti trovate nel punto \!\(\*
StyleBox[\"3\",\nFontWeight->\"Bold\"]\)\!\(\*
StyleBox[\";\",\nFontWeight->\"Bold\"]\)",FontSize->20]]},
(*tredicesima riga*)
{Text[Style["-come \!\(\*
StyleBox[\"DENOMINATORE\",\nFontWeight->\"Bold\",\nFontColor->RGBColor[0., 0.6666666666666666, 0.]]\) il m.c.m. trovato nel punto \!\(\*
StyleBox[\"2.\",\nFontWeight->\"Bold\"]\)",FontSize->20]]},
(*quattordicesima riga*)
{Text[Style[With[{n1=Style[es4numeratore1,es4coloreNumeratore],d1=Style[es4denominatore1,es4coloreDenominatore],n2=Style[es4numeratore2,es4coloreNumeratore],d2=Style[es4denominatore2,es4coloreDenominatore],nEq1=Style[es4numeratoreEq1,es4coloreNumeratore],mcm = Style[es4mcm,es4coloreDenominatore],nEq2=Style[es4numeratoreEq2,es4coloreNumeratore],nSot =Style[es4numeratoreSottrazione ,es4coloreNumeratore]},StringForm["`` - ``  =  `` - ``  = ``  =  ``",HoldForm[HoldForm[n1]/HoldForm[d1]],HoldForm[HoldForm[n2]/HoldForm[d2]],HoldForm[HoldForm[nEq1]/HoldForm[mcm]],HoldForm[HoldForm[nEq2]/HoldForm[mcm]],HoldForm[HoldForm[nEq1-nEq2]/HoldForm[mcm]],HoldForm[HoldForm[nSot]/HoldForm[mcm]]]],FontSize->40]]}
}
,Alignment->{Left}
],
{1000,550}(*dimensioni pannello "pane"*)
],
(*paramentri primo slider*)
{{
es4numeratore1,(*variabile agganciata allo slider*)
30,(*valore iniziale*)
Style["numeratore prima frazione",Directive[es4coloreNumeratore,Large]](*etichetta dello slider*)
},
1,100,(*range*)
1,(*step di modifica*)
Appearance->"Labeled",
LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600},
(*fine paramentri primo slider*)

(*paramentri secondo slider*)
{{es4denominatore1,14,Style["denominatore prima frazione",Directive[es4coloreDenominatore,Large]]},1,100,1,Appearance->"Labeled",
LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600}
(*fine paramentri secondo slider*),
(*paramentri primo slider*)
{{
es4numeratore2,(*variabile agganciata allo slider*)
6,(*valore iniziale*)
Style["numeratore seconda frazione",Directive[es4coloreNumeratore,Large]](*etichetta dello slider*)
},
1,100,(*range*)
1,(*step di modifica*)
Appearance->"Labeled",
LabelStyle->Directive[es4coloreNumeratore,Large],ImageSize->600
},
(*fine paramentri primo slider*)

(*paramentri secondo slider*)
{{es4denominatore2,15,Style["denominatore seconda frazione",Directive[es4coloreDenominatore,Large]]},1,100,1,Appearance->"Labeled",
LabelStyle->Directive[es4coloreDenominatore,Large],ImageSize->600}
(*fine paramentri secondo slider*)
,TrackedSymbols:>{es4numeratore1,es4denominatore1,es4numeratore2,es4denominatore2}]
)


	
End[] (*End of *)

(**)
EndPackage[](*End of package*)
