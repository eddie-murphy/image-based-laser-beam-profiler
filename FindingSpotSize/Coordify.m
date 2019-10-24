(* Wolfram Language Package *)

BeginPackage["Coordify`"]
(* Exported symbols added here with SymbolName::usage *)  
Coordify::usage="Coordify[imdata] adds height and width coordinates to each pixels. Output is: {{height,width,intensity},.....}"
Begin["`Private`"] (* Begin Private Context *) 
Coordify[imdata_]:=Module[{decimateddata=imdata},Flatten[MapIndexed[{#2[[1]], #2[[2]], #1} &, decimateddata, {2}], 1]]
End[] (* End Private Context *)

EndPackage[]