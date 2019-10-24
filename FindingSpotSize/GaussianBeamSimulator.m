(* Wolfram Language Package *)

BeginPackage["GaussianBeamSimulator`"]
(* Exported symbols added here with SymbolName::usage *)  
GaussianBeam::usage="GaussianBeam[minimizefactor,widthcenter,heightcenter,widthspot,heightspot,intmax,intback] returns a perfect gaussian beam with original dimension: width: 1280 and height: 1024."
Begin["`Private`"] (* Begin Private Context *) 
GaussianBeam[mf_,wc_,hc_,ws_,hs_,im_,inb_]:=Module[{height=1024*2^-(mf),width=1280*2^-(mf),widthcentercoord=wc,heightcentercoord=hc,wwidth=ws,wheight=hs,intmax=im,intbackground=inb},Array[(intmax E^(-(#2 - widthcentercoord)^2/
       wwidth^2 - (#1 - heightcentercoord)^2/wheight^2) + 
     intbackground) &, {height, width}]]
End[] (* End Private Context *)

EndPackage[]