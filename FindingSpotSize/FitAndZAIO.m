(* Wolfram Language Package *)

BeginPackage["FitAndZAIO`"]
Needs["FindingSpotSize`FindingSpotSizeforFolder`"]
Needs["FindingSpotSize`ZLister`"]
(* Exported symbols added here with SymbolName::usage *)  
Fazaio::usage="Fazio[folderpath] will fit all the images (.bmp) and join a z position list (from .txt) to the fit results."
Begin["`Private`"] (* Begin Private Context *) 
Fazaio[folder_]:=Module[{path=folder,fitparameterslist,zposlist},fitparameterslist=FindSpotSizeForFolder[path];zposlist=ZList[path];Join[fitparameterslist,Transpose[zposlist],2]
]
End[] (* End Private Context *)

EndPackage[]