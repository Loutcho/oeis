(* ::Package:: *)

(* ::Input:: *)
(*G[{x_,y_}]:={x+y,x*y}*)


(* ::Input:: *)
(*MemA=<||>;MemB=<||>;*)


(* ::Input:: *)
(*GIteratedA[{x_,y_},n_]:=If[KeyExistsQ[MemA,n],MemA[n],Module[{GG},Print[n];GG=If[n==0,{x,y},Expand/@G[GIteratedA[{x,y},n-1]]];AssociateTo[MemA,n->GG];GG]]*)


(* ::Input:: *)
(*(*BUG \[Rule] GIteratedB[{x_,y_},n_]:=If[KeyExistsQ[MemB,n],MemB[n],Module[{GG},Print[n];GG=If[n\[Equal]0,{x,y},Expand/@GIteratedB[G[{x,y}],n-1]];AssociateTo[MemB,n\[Rule]GG];GG]]*)*)


(* ::Input:: *)
(*GIteratedB[{x_,y_},n_]:=Module[{GG},GG=If[n==0,{x,y},Expand/@GIteratedB[G[{x,y}],n-1]];GG]*)


(* ::Input:: *)
(*GIteratedB[{x,y},4]*)


(* ::Input:: *)
(*GIteratedA[{x,y},4]*)


(* ::Input:: *)
(*PreA[n_]:=CoefficientList[GIteratedA[{x,y},n][[2]],{x,y}]*)


(* ::Input:: *)
(*PreB[n_]:=CoefficientList[GIteratedB[{x,y},n][[2]],{x,y}]*)


(* ::Input:: *)
(*A[n_]:=Length[Select[Flatten[PreA[n]],(#!=0&)]]*)


(* ::Input:: *)
(*B[n_]:=Length[Select[Flatten[PreB[n]],(#!=0&)]]*)


(* ::Input:: *)
(*Table[A[n],{n,1,12}]*)


(* ::Input:: *)
(*Table[B[n],{n,1,9}]*)


(* ::Input:: *)
(*Keys[MemA]*)


(* ::Input:: *)
(*(*Keys[MemB]*)*)


(* ::Input:: *)
(*Table[ArrayPlot[PreA[n],ColorFunction->"TemperatureMap"],{n,1,Max[Keys [MemA]]}]*)


(* ::Input:: *)
(*PreA[6]//MatrixForm*)


(* ::Input:: *)
(*GIteratedA[{x,y},3][[2]]*)
