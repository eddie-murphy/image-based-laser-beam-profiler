(* Wolfram Language Package *)

BeginPackage["TotalDecimation`"]
Needs["FindingSpotSize`AutoCroppingSuperRevised`"]
Needs["FindingSpotSize`SpotSizeGuesser`"]
Needs["FindingSpotSize`CroppingDimensions`"]
Needs["FindingSpotSize`Decimator`"]
(* Exported symbols added here with SymbolName::usage *)  
TotallyDecimate::usage="TotallyDecimate[imdata] will do the most effecient and effective decimation possible and will return {data,pixel multiplier to mm value}"
Begin["`Private`"] (* Begin Private Context *) 
TotallyDecimate[imdata_,thenumber_:157]:=Module[{newdata=imdata,winq,number=thenumber},winq = Max[GuessSpotSize[newdata]];Piecewise[{{{AutoCropSuperRevised[newdata, 
     CroppingDimensions[newdata]], 0.0052}, 
   winq <= number}, {{Decimate[newdata, 1], 0.0104}, 
   winq > number}}]]
End[] (* End Private Context *)

EndPackage[]