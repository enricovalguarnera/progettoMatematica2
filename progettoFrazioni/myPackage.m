(* ::Package:: *)

(**)

ClearAll["ProgettoFrazioni`*"];

(*info su di noi e sulla versione di Mathematica, versione del nostro package*)

BeginPackage["ProgettoFrazioni`"]

	getGame1::usage = "fa cose"
	getEsempioTipoFrazione::usage = "Funzione che restituisce l'esempio 1"

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
			coloreDenominatore = Red;(*colore per il denominatore*)
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


getGame1[] := (
	initMousePos = {-1, -1};
	dimPianoX = 20;
	dimPianoY = 12;
	 (*numero di lego inserite nel piano*)
	nLegos = 7;
	 (*dimensioni lego inserite*)

	legos = {{2, 8}, {2, 4}, {2, 3}, {2, 2}, {2, 1}, {1, 1}, {1, 1}};
	 (*posizioni delle lego sul piano*)

	legoPositions = {{1, 0}, {3, 0}, {3, 4}, {5, 0}, {3, 7}, {9, 0}, {8,
	    2}};
	(*colori delle lego*)(*TODO fissa il colore per dimensione*)

	legoColors = {Blue, Green, Red , Yellow, Cyan, Brown, Orange};
	(*si sta spostando una lego?*)
	moving = False;
	 (*identificativo della lego che viene spostata*)
	movingLego = -1;
	(*lista di oggetti da disegnare*)

	obj = getRectangles[legoPositions, legos];
	Pane[
 DynamicModule[
  {},
  EventHandler[
   legosPrint[Dynamic@obj],
   {
    (*evento mouse cliccato,
    controlla se \[EGrave] presente un blocchetto lego sotto il click*)

    "MouseDown" :> (
      (*recupero coordinate mouse*)

      initMousePos = MousePosition["Graphics"];
      xMouse = initMousePos[[1]];
      yMouse = initMousePos[[2]];
      For[i = 1, i <= nLegos, i++,
       If[
        xMouse >= legoPositions[[i, 1]] &&
         xMouse <= (legos[[i, 1]] + legoPositions[[i, 1]]) &&

          yMouse >= legoPositions[[i, 2]] &&
         yMouse <= (legos[[i, 2]] + legoPositions[[i, 2]]),
        moving = True;
        movingLego = i
        ]
       ]
      ),
    (*evento mouse rilasciato, ferma il movimento*)

    "MouseUp" :> (
      moving = False;
      movingLego = -1;
      ),
    (*evento mouse trascinato, sposta il blocchetto selezionato*)

    "MouseDragged" :> (
      (*recupero coordinate mouse*)

      initMousePos = MousePosition["Graphics"];
      xMouse = initMousePos[[1]];
      yMouse = initMousePos[[2]];
      (*TODO non permettere uscita dal piano*)

      If[xMouse >= 0 && xMouse <= dimPianoX && yMouse >= 0 &&
        yMouse <= dimPianoY,
       If[moving,
        legoPositions[[movingLego, 1]] = Floor[xMouse];
        legoPositions[[movingLego, 2]] = Floor[yMouse];
        obj = getRectangles[legoPositions, legos];
        ]
       ]
      )
    }, PassEventsDown -> True
   ]
  ]
 , {800, 600}, Alignment -> Center])


(*funzione che restituisce le informazioni per disegnare*)

getRectangles[legoPositions_, legos_] := (
  objTmp = {};
  (*scorro lego per lego e recupero le info per disegnare i \
rettangoli*)
  For[i = 1, i <= nLegos, i++,
   angoloInferiore = Part[legoPositions, i];
   dimensioneLego = Part[legos, i];
   angoloSuperiore = angoloInferiore + dimensioneLego;
   testo = dimensioneLego[[1]]*dimensioneLego[[2]]/16;
   AppendTo[
    objTmp, {legoColors[[i]], EdgeForm[Directive[Thick, Black]],
     Rectangle[angoloInferiore, angoloSuperiore],
     Text[Style[testo, Black, Bold],
      angoloInferiore + {dimensioneLego[[1]]/2,
        dimensioneLego[[2]]/2}]}];
   ];
  (*ritorno la lista di rettangoli*)
  objTmp
  )

(*Funzione che disegna i rettangoli*)
	legosPrint[obj_] := (
  Graphics[{obj}, PlotRange -> {{0, dimPianoX}, {0, dimPianoY}},
   ImageSize -> 800, Background -> White,
   GridLines -> {Range[0, dimPianoX, 1], Range[0, dimPianoY, 1]}]
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
