(* Wolfram Language Package *)

BeginPackage["GaussianBeamFitRevised`"]
Needs["FindingSpotSize`Coordify`"]
Needs["FindingSpotSize`SpotSizeGuesserRevised`"]
(* Exported symbols added here with SymbolName::usage *)  
GaussianBeamFitRevised::usage="GaussianBeamFit[imdata] will fit a 3D gaussian to a 2D image. Internal code does all the guess work."
Begin["`Private`"] (* Begin Private Context *) 
GaussianBeamFitRevised[imdata_]:=Module[{decimateddata=imdata,coordata,decimatedwidth,decimatedheight,guessresults,max,intbackground,heightcentercoord,widthcentercoord,wheightguess,wwidthguess},coordata=Coordify[decimateddata];guessresults=SpotSizeGuesserRevised[decimateddata];max=guessresults[[1]];decimatedheight=guessresults[[2]];decimatedwidth=guessresults[[3]];intbackground=guessresults[[4]];heightcentercoord=guessresults[[5]];widthcentercoord=guessresults[[6]];wheightguess=guessresults[[7]];wwidthguess=guessresults[[8]];NonlinearModelFit[coordata, {maximumintensity * 
    E^(-(widthpos - widthcenter)^2/
     wspotsize^2 - (heightpos - heightcenter)^2/hspotsize^2) + 
   backgroundintensity, {halftotalwidth >= wspotsize > 0, 
   halftotalheight >= hspotsize > 0, max >= maximumintensity > 0, 
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
(*{max,decimatedheight,decimatedwidth,intbackground,heightcentercoord,widthcentercoord,wheightguess,wwidthguess}*)
halftotalwidth = decimatedwidth/2.0