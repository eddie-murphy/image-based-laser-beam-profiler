(* Wolfram Language Package *)

BeginPackage["CroppingDimensions`"]
(* Exported symbols added here with SymbolName::usage *)  
CroppingDimensions::usage="CroppingDimensions[imdata] returns {new minimal height, new minimal width}"
Begin["`Private`"] (* Begin Private Context *) 
CroppingDimensions[imdata_, add_: 50] := 
 Module[{newdata = imdata, addition = add, max, height, width, 
   centercoord, heightcentercoord, widthcentercoord, 
   intvaluesinwidthdirectionright, intvaluesinheightdirectiondown, 
   intvaluesinheightdirectionup, intvaluesinwidthdirectionleft, 
   backgroundlistdown, backgroundlistup, backgroundlistleft, 
   backgroundlistright, backgroundhorizontallength, 
   backgroundverticallength, newverticaldimension, 
   newhorizontaldimension}, max = Max[newdata];
  height = Length[newdata[[All, 1]]];
  
  width = Length[newdata[[1, All]]];
  centercoord = 
   FirstPosition[newdata, max, {height/2, width/2}, {2}];
  heightcentercoord = centercoord[[1]];
  widthcentercoord = centercoord[[2]];
  intvaluesinwidthdirectionright = 
   newdata[[heightcentercoord, widthcentercoord ;;]]//N;
  
  intvaluesinwidthdirectionleft = 
   Reverse[newdata[[heightcentercoord, ;; widthcentercoord]]]//N;
  
  intvaluesinheightdirectionup = 
   Reverse[newdata[[;; heightcentercoord, widthcentercoord]]]//N;
  
  intvaluesinheightdirectiondown = 
   newdata[[heightcentercoord ;;, widthcentercoord]]//N;
  backgroundlistleft = 
   Position[intvaluesinwidthdirectionright, _?((# > 1) &), 1];
  backgroundlistright = 
   backgroundlistleft = 
    Position[intvaluesinwidthdirectionright, _?((# > 1) &), 1];
  backgroundhorizontallength = 
   Max[{Length[backgroundlistleft], Length[backgroundlistright]}];
  backgroundlistup = 
   Position[intvaluesinheightdirectionup, _?((# > 1) &), 1];
  backgroundlistdown = 
   Position[intvaluesinheightdirectiondown, _?((# > 1) &), 1];
  backgroundverticallength = 
   Max[{Length[backgroundlistup], Length[backgroundlistdown]}];
  newverticaldimension = 
   Round[(backgroundhorizontallength + addition)*2, 2];
  newhorizontaldimension = 
   Round[(backgroundhorizontallength + addition)*2, 
    2]; {newverticaldimension, newhorizontaldimension}]
End[] (* End Private Context *)

EndPackage[]