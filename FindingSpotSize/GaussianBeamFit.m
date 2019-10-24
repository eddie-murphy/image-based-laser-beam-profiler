(* Wolfram Language Package *)

BeginPackage["GaussianBeamFit`"]
Needs["FindingSpotSize`Coordify`"](*it will say it was not created but it still links it to this. it wasn't created because it already exists from loading init which gets loaded when loading the whole folder. If I used Get this message would not happen because Get loads it each time. *)
(* Exported symbols added here with SymbolName::usage *)  
GaussianBeamFit::usage="GaussianBeamFit[imdata] will fit a 3D gaussian to a 2D image. Internal code does all the guess work."
Begin["`Private`"] (* Begin Private Context *) 
GaussianBeamFit[imdata_]:=Module[{decimateddata=imdata,coordata,height,width,max,centercoord,heightcentercoord,widthcentercoord,decimatedheight,decimatedwidth,intbackground,targetintensity,intvaluesinwidthdirection,halftotalwidth,intvaluesinheightdirection,halftotalheight,wwidthguess,wheightguess},coordata=Coordify[decimateddata];height = Length[decimateddata[[All, 1]]];width = Length[decimateddata[[1, All]]];max = Max[decimateddata];
centercoord = 
 FirstPosition[decimateddata, max, {height/2, width/2}, {2}];heightcentercoord = centercoord[[1]];widthcentercoord = centercoord[[2]];decimatedheight = Length[decimateddata[[All, 1]]];decimatedwidth = Length[decimateddata[[1, All]]];intbackground = 
 Mean[{decimateddata[[1, 1]], decimateddata[[1, decimatedheight]], 
   decimateddata[[1, decimatedwidth]], 
   decimateddata[[decimatedheight, decimatedwidth]]}];targetintensity = max/E^2//N;intvaluesinwidthdirection = 
  Reverse[decimateddata[[heightcentercoord, 1 ;; widthcentercoord]]]//N;halftotalwidth = decimatedwidth/2.0;intvaluesinheightdirection = 
  Reverse[decimateddata[[1 ;; heightcentercoord, widthcentercoord]]]//N;halftotalheight = decimatedheight/2.0;wwidthguess = 
 FirstPosition[intvaluesinwidthdirection, _?(# < (targetintensity) &),
    halftotalwidth][[1]];wheightguess = 
 FirstPosition[
   intvaluesinheightdirection, _?(# < (targetintensity) &), 
   halftotalheight][[1]];NonlinearModelFit[coordata, {maximumintensity * 
    E^(-(widthpos - widthcenter)^2/
     wspotsize^2 - (heightpos - heightcenter)^2/hspotsize^2) + 
   backgroundintensity, {halftotalwidth >= wspotsize > 0, 
   halftotalheight >= hspotsize > 0, 255.0 >= maximumintensity > 0, 
   decimatedwidth >= widthcenter > 0 , 
   decimatedheight >= heightcenter > 0, 
   255.0 > backgroundintensity > 0}}, {{wspotsize, 
   wwidthguess*1.0}, {hspotsize, wheightguess*1.0}, {maximumintensity,
    max*1.0}, {widthcenter, widthcentercoord*1.0}, {heightcenter, 
   heightcentercoord*1.0}, {backgroundintensity, 
   intbackground*1.0}}, {heightpos, widthpos}(*, WorkingPrecision -> 10*)]
]
End[] (* End Private Context *)

EndPackage[]