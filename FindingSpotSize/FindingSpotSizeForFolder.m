(* Wolfram Language Package *)

BeginPackage["FindingSpotSizeforFolder`"]
Needs["FindingSpotSize`FindingSpotSize`"]
(* Exported symbols added here with SymbolName::usage *)  
FindSpotSizeForFolder::usage="FindSpotSizeForFolder[path to folder,filetype] will perform fitting and conversion to mm on each item in the folder and create a nested list structure (ie a table) of the values."
Begin["`Private`"] (* Begin Private Context *) 
FindSpotSizeForFolder[pathtofolder_,filetype_:"*.bmp"]:=Module[{path=pathtofolder,type=filetype,files,i=0,newfiles},files = FileNames[
  type, {path}];newfiles=files[[Ordering@
   PadRight@
    StringSplit[files, x : DigitCharacter .. :> FromDigits@x]]];Monitor[(i++; 
    Quiet[FindSpotSize[Import[#, "Data"][[All, All, 1]]]]) & /@ newfiles,
  Row[{ProgressIndicator[i, {0, Length[newfiles]}], i}, 
  "Total Images, Working On Image:" Length[newfiles]]](*(FindSpotSize[Import[#, "Data"][[All, All, 1]]]) & /@ files*)
  
  ]
End[] (* End Private Context *)

EndPackage[]