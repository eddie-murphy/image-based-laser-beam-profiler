(* Wolfram Language Package *)

BeginPackage["ZLister`"]
(* Exported symbols added here with SymbolName::usage *)  
ZList::usage="ZList[folder,filetype] will pick out all the text (.txt by default) files in a folder and make a list of the positions in them. Hopefully you only have one text file per folder. File must be delimited by spaces for name and position and seperate line for each image entry. "
Begin["`Private`"] (* Begin Private Context *) 
ZList[folderpath_,filetype_:"*.txt"]:=Module[{path=folderpath,type=filetype,textfile},textfile = 
 FileNames[
  type, {path}];ToExpression[((Partition[Import[#, "words"], 2]) & /@ textfile)[[All, All, 2]]]
  ]
End[] (* End Private Context *)

EndPackage[]