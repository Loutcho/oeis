(* ::Package:: *)

(* ::Input:: *)
(*P1=<|1->2|>*)


(* ::Input:: *)
(*Q1=<|2->1|>*)


(* ::Input:: *)
(*T[P_,i_]:=If[KeyExistsQ[P,i],P[i],0]*)


(* ::Input:: *)
(*Deg[P_]:=Max[Keys[P]]*)


(* ::Input:: *)
(*F1[P_,Q_]:=Association[MapIndexed[(First[#2]-1)->#1&,Module[{L=Max[Deg[P],Deg[Q]]},Max/@Table[{T[P,i],T[Q,i]},{i,0,L}]]]]*)


(* ::Input:: *)
(*P2=F1[P1,Q1]*)


(* ::Input:: *)
(*Q2=<|3->2|>*)


(* ::Input:: *)
(*FFF2[P_,Q_,i_,j_]:=Module[{p=T[P,j],q=T[Q,i-j]},If[p!=0&&q!=0,p+q,0]]*)


(* ::Input:: *)
(*FF2[P_,Q_,i_]:=Table[FFF2[P,Q,i,j],{j,0,Deg[P]+Deg[Q]}]*)


(* ::Input:: *)
(*F2[P_,Q_,i_]:=Max[FF2[P,Q,i]]-1*)


(* ::Input:: *)
(*F2[P1,Q1,2]*)
