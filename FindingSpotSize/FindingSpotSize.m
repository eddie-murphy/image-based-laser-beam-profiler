(* ::Package:: *)

(* Wolfram Language Package *)

(* Created by the Wolfram Workbench Jun 19, 2017 *)

BeginPackage["FindingSpotSize`"]
Needs["FindingSpotSize`GaussianBeamFit`"]
Needs["FindingSpotSize`TotalDecimation`"]
(* Exported symbols added here with SymbolName::usage *) 

FindSpotSize::usage="FindSpotSize[imdata] finds spot sizes in mm with errors of image data in the form {hspotmm,hspoterrormm,wspotmm,wspoterrormm}"
Begin["`Private`"]
(* Implementation of the package *)

(*I need to make a guessing thing to guess both wh and ww...then I have a basic if statment with cut off of 63 and use max of both wh and ww...then I decide if I decimate or autocrop...and then I need to output it as a list which shouldn't be anything more than having a list with symbols as the last command. then need to convert to mm*)
FindSpotSize[imdata_] := 
 Module[{data = imdata, newdata, fit, fitparameters, wspot, hspot, 
   wspoterror, hspoterror, wspotmm, hspotmm, wspoterrormm, 
   hspoterrormm, decimatedresults, mult}, 
  decimatedresults = TotallyDecimate[data]; 
  newdata = decimatedresults[[1]]; mult = decimatedresults[[2]]; 
  fit = GaussianBeamFit[newdata];
  fitparameters = fit["ParameterTableEntries"]; 
  wspot = fitparameters[[1, 1]]; wspoterror = fitparameters[[1, 2]]; 
  hspot = fitparameters[[2, 1]]; hspoterror = fitparameters[[2, 2]];
  wspotmm = wspot*mult; hspotmm = hspot*mult; 
  wspoterrormm = wspoterror*mult; 
  hspoterrormm = hspoterror*mult; {hspotmm, hspoterrormm, wspotmm, 
   wspoterrormm}]
End[]

EndPackage[]

