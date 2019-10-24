(* Wolfram Language Package *)

BeginPackage["AutoCroppingSuperRevised`"]
(* Exported symbols added here with SymbolName::usage *)  
AutoCropSuperRevised::usage="AutoCropSuperRevised[imdata,list of dimensions] will crop to the given dimensions."
Begin["`Private`"] (* Begin Private Context *) 
AutoCropSuperRevised[imdata_, listofdim_] := 
 Module[{data = imdata, max, list = listofdim, desiredheight, 
   desiredwidth, height, width, centercoord, heightcentercoord, 
   widthcentercoord, alloweddistancetotopedge, 
   allowedistancetobottomedge, alloweddistancetoleftedge, 
   alloweddistancetorightedge, wanteddistancetotopedge, 
   wanteddistancetobottomedge, wanteddistancetoleftedge, 
   wanteddistancetorightedge, extradistancetotopedge, 
   extradistancetobottomedge, extradistancetoleftedge, 
   extradistancetorightedge, leftpointindex, rightpointindex, 
   toppointindex, bottompointindex}, desiredheight = list[[1]]; 
  desiredwidth = list[[2]]; max = Max[data]; 
  height = Length[data[[All, 1]]]; width = Length[data[[1, All]]]; 
  centercoord = FirstPosition[data, max, {height/2, width/2}, {2}]; 
  heightcentercoord = centercoord[[1]]; 
  widthcentercoord = centercoord[[2]]; 
  alloweddistancetotopedge = 
   Length[data[[;; heightcentercoord, widthcentercoord]]] - 1;
  allowedistancetobottomedge = 
   Length[data[[heightcentercoord ;;, widthcentercoord]]] - 1; 
  alloweddistancetoleftedge = 
   Length[data[[heightcentercoord, ;; widthcentercoord]]] - 1; 
  alloweddistancetorightedge = 
   Length[data[[heightcentercoord, widthcentercoord ;;]]] - 1;
  wanteddistancetotopedge = desiredheight/2 - 1; 
  wanteddistancetobottomedge = desiredheight/2; 
  wanteddistancetoleftedge = desiredwidth/2 - 1; 
  wanteddistancetorightedge = desiredwidth/2; 
  extradistancetotopedge = 
   alloweddistancetotopedge - wanteddistancetotopedge; 
  extradistancetobottomedge = 
   allowedistancetobottomedge - wanteddistancetobottomedge; 
  extradistancetoleftedge = 
   alloweddistancetoleftedge - wanteddistancetoleftedge;
  extradistancetorightedge = 
   alloweddistancetorightedge - wanteddistancetorightedge; 
  Which[extradistancetorightedge > 0, 
   Which[extradistancetoleftedge > 0, 
    leftpointindex = wanteddistancetoleftedge;
    rightpointindex = wanteddistancetorightedge, 
    extradistancetoleftedge < 0, 
    leftpointindex = 
     wanteddistancetoleftedge + extradistancetoleftedge;
    rightpointindex = 
     wanteddistancetorightedge - 
      extradistancetoleftedge],(*I assume that one of the edges is on \
the screen before crop so there won't be a coded case for \
extradistancetorightedge<0 and extradistancetoleftedge<0*)
   extradistancetorightedge < 0, 
   rightpointindex = 
    wanteddistancetorightedge + extradistancetorightedge;
   leftpointindex = 
    wanteddistancetoleftedge - extradistancetorightedge]; 
  Which[extradistancetotopedge > 0, 
   Which[extradistancetobottomedge > 0, 
    bottompointindex = wanteddistancetobottomedge;
    toppointindex = wanteddistancetotopedge, 
    extradistancetobottomedge < 0, 
    bottompointindex = 
     wanteddistancetobottomedge + extradistancetobottomedge;
    toppointindex = 
     wanteddistancetotopedge - 
      extradistancetobottomedge],(*I assume that one of the edges is \
on the screen before crop so there won't be a coded case for \
extradistancetotopedge<0 and extradistancetobottomedge<0*)
   extradistancetotopedge < 0, 
   toppointindex = wanteddistancetotopedge + extradistancetotopedge;
   bottompointindex = 
    wanteddistancetobottomedge - extradistancetotopedge]; 
  data[[heightcentercoord - toppointindex ;; 
     heightcentercoord + bottompointindex, 
    widthcentercoord - leftpointindex ;; 
     widthcentercoord + rightpointindex]]]
End[] (* End Private Context *)

EndPackage[]