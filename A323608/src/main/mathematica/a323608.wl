(* ::Package:: *)

(* ::Input:: *)
(*lt[x_,y_]:=Module[{c,d,xx,yy,u,v},{c,d}=IntegerExponent[#,2]&/@{x,y};*)
(*xx=x/2^c;*)
(*yy=y/2^d;*)
(*u=If[xx==1,\[Infinity],c];*)
(*v=If[yy==1,\[Infinity],d];*)
(*If[u!=v,u<v,If[u==\[Infinity],c>d,xx<yy]]]*)
(*row[n_]:=Sort[Range[n],lt]*)
(*a[n_]:=First[FirstPosition[row[n],n]]*)
(*Table[a[n],{n,1,120}]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*A={1,1,1,2,2,3,3,5,4,6,5,8,6,9,7,12,8,12,9,15,10,15,11,19,12,18,13,22,14,21,15,27,16,24,17,29,18,27,19,34,20,30,21,36,22,33,23,42,24,36,25,43,26,39,27,49,28,42,29,50,30,45,31,58,32,48,33,57,34,51,35,64,36,54,37,64,38,57,39,73,40,60,41,71,42,63,43,79,44,66,45,78,46,69,47,89,48,72,49,85,50,75,51,94,52,78,53,92,54,81,55,104,56,84,57,99,58,87,59,109}*)


(* ::Input:: *)
(*Table[A[[4*k+1]],{k,0,Floor[Length[A]/4]-1}]*)


(* ::Input:: *)
(*Table[A[[4*k+2]],{k,0,Floor[Length[A]/4]-1}]*)


(* ::Input:: *)
(*Table[A[[4*k+3]],{k,0,Floor[Length[A]/4]-1}]*)


(* ::Input:: *)
(*ListPlot[Table[A[[4*k+4]]-2*k,{k,0,Floor[Length[A]/4]-1}]]*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*B=Table[A[[4*k+4]]-3*k-1,{k,0,Floor[Length[A]/4]-1}]*)


(* ::Input:: *)
(*Table[A[[k]]-B[[k]],{k,1,25}]*)


(* ::Input:: *)
(*F[0]:=1*)
(*F[1]:=1*)
(*F[2]:=1*)
(*F[3]:=1*)
(*F[n_]:=Module[{s=1+Mod[n,4],k=Floor[n/4]},{3,2,3,2}[[s]]*k+{F[k]-2,0,0,1}[[s]]]*)


(* ::Input:: *)
(*Table[F[n],{n,1,20}]*)


(* ::Input:: *)
(*Table[a[n],{n,1,20}]*)


(* ::Input:: *)
(*Table[a[n]-F[n],{n,1,80}]*)


(* ::Input:: *)
(**)
