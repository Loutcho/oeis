(* ::Package:: *)

(* ::Input:: *)
(*A[n_]:=Length[ConnectedComponents[AdjacencyGraph[Map[Boole[#!=1]&,Array[GCD,{n,n}],{2}]]]]*)


(* ::Input:: *)
(*Table[A[n],{n,1,107}]*)
