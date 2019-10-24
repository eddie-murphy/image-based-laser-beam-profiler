(* Wolfram Language Package *)

BeginPackage["SpotSizeGuesserRevised`"]
(* Exported symbols added here with SymbolName::usage *)  
GuessSpotSizeRevised::usage="GuessSpotSizeRevised[imdata] gives in list format {max,decimatedheight,decimatedwidth,intbackground,heightcentercoord,widthcentercoord,wheightguess,wwidthguess}."
Begin["`Private`"] (* Begin Private Context *) 
GuessSpotSizeRevised[imdata_]:=Module[{decimateddata=imdata,max,heightcentercoord,widthcentercoord,decimatedheight,decimatedwidth,intbackground,targetintensity,listy,heightlisty,widthlisty,intvaluesinwidthdirectionright,intvaluesinwidthdirectionleft,intvaluesinheightdirectionup,intvaluesinheightdirectiondown,halftotalwidth,halftotalheight,wwidthguessleft,wwidthguessright,wheightguessup,wheightguessdown,wwidthguess,wheightguess},max = Max[decimateddata];

decimatedheight = Length[decimateddata[[All, 1]]];

decimatedwidth = Length[decimateddata[[1, All]]];

intbackground = 
  Mean[{decimateddata[[1, 1]], decimateddata[[1, decimatedheight]], 
    decimateddata[[1, decimatedwidth]], 
    decimateddata[[decimatedheight, decimatedwidth]]}];

listy = Position[
   decimateddata, _?( (intbackground + (max/
           E^2) <= # <= (max)) &), {2}];

heightlisty = listy[[All, 1]];

widthlisty = listy[[All, 2]];

heightcentercoord = heightlisty // Mean // Round;

widthcentercoord = widthlisty // Mean // Round;

targetintensity = max/E^2//N;

intvaluesinwidthdirectionright = 
  decimateddata[[heightcentercoord, widthcentercoord ;;]]//N;

intvaluesinwidthdirectionleft = 
  Reverse[decimateddata[[heightcentercoord, ;; widthcentercoord]]]//N;

halftotalwidth = decimatedwidth/2.0;

halftotalheight = decimatedheight/2.0;

intvaluesinheightdirectionup = 
  Reverse[decimateddata[[;; heightcentercoord, widthcentercoord]]]//N;

intvaluesinheightdirectiondown = 
  decimateddata[[heightcentercoord ;;, widthcentercoord]]//N;

wwidthguessleft = 
  FirstPosition[
    intvaluesinwidthdirectionleft, _?(# < (targetintensity) &), 
    halftotalwidth,2][[1]];

wwidthguessright = 
  FirstPosition[
    intvaluesinwidthdirectionright, _?(# < (targetintensity) &), 
    halftotalwidth,2][[1]];

wheightguessup = 
  FirstPosition[
    intvaluesinheightdirectionup, _?(# < (targetintensity) &), 
    halftotalheight,2][[1]];

wheightguessdown = 
  FirstPosition[
    intvaluesinheightdirectiondown, _?(# < (targetintensity) &), 
    halftotalheight,2][[1]];

wwidthguess = Mean[{wwidthguessleft, wwidthguessright}];

wheightguess = Mean[{wheightguessup, wheightguessdown}];{max,decimatedheight,decimatedwidth,intbackground,heightcentercoord,widthcentercoord,wheightguess,wwidthguess}
]
End[] (* End Private Context *)

EndPackage[]