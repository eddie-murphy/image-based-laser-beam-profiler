(* Wolfram Language Package *)

BeginPackage["Decimator`"]
(* Exported symbols added here with SymbolName::usage *)  
Decimate::usage="Decimate[imdata,n] decimates an array (imdata) representing a grayscale image data. Divides both dimensions by the same number, 2^n, where n is optional and has default of 3." 
Begin["`Private`"] (* Begin Private Context *) 
Decimate[imdata_,power_:3]:=Module[{twoisraisedby=power,data=imdata,height,width},height=Length[data[[All, 1]]];width = Length[data[[1, All]]];ArrayResample[data, {(1/(2^twoisraisedby))*height, (1/(2^twoisraisedby))*width}, Resampling -> "Nearest"]]
End[] (* End Private Context *)

EndPackage[]