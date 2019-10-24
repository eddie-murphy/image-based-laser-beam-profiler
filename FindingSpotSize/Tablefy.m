(* Wolfram Language Package *)

BeginPackage["Tablefy`"]
(* Exported symbols added here with SymbolName::usage *)  
Tablefy::usage="Tablefy[Fazaio output list,option] will append the current headers to each column abd will output as grid by default and will not if option is gridno"
Begin["`Private`"] (* Begin Private Context *) 
Tablefy[fazaiooutputlist_,option_:"gridyes"]:=Module[{fitandzlist=fazaiooutputlist,tableofvalues,gridopt=option},tableofvalues=Prepend[fitandzlist, {"height spot size", "height spot size error", 
  "width spot size", "width spot size error", "zpos"}];If[gridopt==="gridno",tableofvalues,Grid[tableofvalues,Frame->All]]
]
End[] (* End Private Context *)

EndPackage[]