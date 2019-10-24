(* Wolfram Language Package *)

BeginPackage["GuessSpotSize`"]
(* Exported symbols added here with SymbolName::usage *)  
GuessSpotSize::usage="GuessSpotSize[imdata] gives in list format {wheight,wwidth} the spot size guesses."
Begin["`Private`"] (* Begin Private Context *) 
GuessSpotSize[imdata_]:=Module[{decimateddata=imdata,max,centercoord,heightcentercoord,widthcentercoord,decimatedheight,decimatedwidth,intbackground,targetintensity,intvaluesinwidthdirection,halftotalwidth,intvaluesinheightdirection,halftotalheight,wwidthguess,wheightguess},
max = Max[decimateddata];decimatedheight = Length[decimateddata[[All, 1]]];decimatedwidth = Length[decimateddata[[1, All]]];centercoord = 
 FirstPosition[decimateddata, max, {decimatedheight/2, decimatedwidth/2}, {2}];heightcentercoord = centercoord[[1]];widthcentercoord = centercoord[[2]];intbackground = 
 Mean[{decimateddata[[1, 1]], decimateddata[[1, decimatedheight]], 
   decimateddata[[1, decimatedwidth]], 
   decimateddata[[decimatedheight, decimatedwidth]]}];targetintensity = max/E^2//N;intvaluesinwidthdirection = 
 Reverse[decimateddata[[heightcentercoord, 1 ;; widthcentercoord]]]//N;halftotalwidth = decimatedwidth/2.0;intvaluesinheightdirection = 
  Reverse[decimateddata[[1 ;; heightcentercoord, widthcentercoord]]]//N;halftotalheight = decimatedheight/2.0;wwidthguess = 
 FirstPosition[intvaluesinwidthdirection, _?(# < (targetintensity) &),
    halftotalwidth,2][[1]];wheightguess = 
 FirstPosition[
   intvaluesinheightdirection, _?(# < (targetintensity) &), 
   halftotalheight,2][[1]];{wheightguess,wwidthguess}
]
End[] (* End Private Context *)

EndPackage[]
(*I bumped decimated dimensions up a little so I could use them in positioning*)