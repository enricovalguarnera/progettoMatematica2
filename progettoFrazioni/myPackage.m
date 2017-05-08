(* ::Package:: *)

(**)

ClearAll["ProgettoFrazioni`*"];

(*info su di noi e sulla versione di Mathematica, versione del nostro package*)

BeginPackage["ProgettoFrazioni`"]

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
