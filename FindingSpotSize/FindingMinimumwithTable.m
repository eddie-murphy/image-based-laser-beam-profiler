(* Wolfram Language Package *)

BeginPackage["FindBeamMinimumwT`"]
Needs["FindingSpotSize`FitAndZAIO`"]
Needs["FindingSpotSize`Tablefy`"]
(* Exported symbols added here with SymbolName::usage *)  
FindBeamMinimumwT::usage="FindBeamMinimum[folder,wavelength in mm] will find the beam waist and the beam waist's position for a folder of images and a textfile. Will return {whfit,wwfit}"
Begin["`Private`"] (* Begin Private Context *) 
FindBeamMinimumwT[folderpath_,wvlength_]:=Module[{folder=folderpath,fitandzlist,hserror,wserror,column1,column3,column5,whlist,wwlist,whguess,zposhguess,wwguess,zposwguess,lambda=wvlength,whfit,wwfit,tableresults},
fitandzlist = Fazaio[folder];tableresults=Tablefy[fitandzlist,"gridno"];hserror = fitandzlist[[All, 2]];wserror = fitandzlist[[All, 4]];column1 = Transpose[List[fitandzlist[[All, 1]]]];column3 = Transpose[List[fitandzlist[[All, 3]]]];column5 = Transpose[List[fitandzlist[[All, 5]]]];
whlist = Join[column5, column1, 2];wwlist = Join[column5, column3, 2];whguess = Min[whlist[[All, 2]]];zposhguess = 
 column5[[(FirstPosition[whlist[[All, 2]], whguess][[1]])]][[1]];wwguess = Min[wwlist[[All, 2]]];zposwguess = 
 column5[[(FirstPosition[wwlist[[All, 2]], wwguess][[1]])]][[1]];whfit = Module[{beamwaist, zpos, zbeamwaist, wavelength = lambda, 
   beamwaistguess = whguess, zposguess = zposhguess}, 
  NonlinearModelFit[
   whlist, {beamwaist*Sqrt[
     1 + (zpos - 
         zbeamwaist)^2 (wavelength/(\[Pi] beamwaist^2))^2], \
{beamwaist > 0, \[Infinity] > zpos > -\[Infinity]}}, {{beamwaist, 
     beamwaistguess}, {zbeamwaist, zposguess}}, {zpos}, 
   Weights -> 1/hserror^2, VarianceEstimatorFunction -> (1 &)]];wwfit = Module[{beamwaist, zpos, zbeamwaist, wavelength = lambda, 
   beamwaistguess = wwguess, zposguess = zposwguess}, 
  NonlinearModelFit[
   wwlist, {beamwaist*Sqrt[
     1 + (zpos - 
         zbeamwaist)^2 (wavelength/(\[Pi] beamwaist^2))^2], \
{beamwaist > 0, \[Infinity] > zpos > -\[Infinity]}}, {{beamwaist, 
     beamwaistguess}, {zbeamwaist, zposguess}}, {zpos}, 
   Weights -> 1/wserror^2, VarianceEstimatorFunction -> (1 &)]];{whfit,wwfit,tableresults}
]
End[] (* End Private Context *)

EndPackage[]