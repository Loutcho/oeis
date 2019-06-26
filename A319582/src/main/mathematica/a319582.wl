(* ::Package:: *)

(* ::Input:: *)
(*F[n_]:=If[n==1,{},FactorInteger[n]]*)


(* ::Input:: *)
(*V[p_]:=If[KeyExistsQ[#,p],#[p],0]&*)


(* ::Input:: *)
(*PreT[n_,k_]:=Module[{fn=F[n],fk=F[k],p,an=<||>,ak=<||>,w},*)
(*p=Union[First/@fn,First/@fk];*)
(*(an[#[[1]]]=#[[2]])&/@fn;*)
(*(ak[#[[1]]]=#[[2]])&/@fk;*)
(*w=({V[#][an],V[#][ak]})&/@p;*)
(*Select[w,(#[[1]]>=#[[2]]>0)&]*)
(*]*)


(* ::Input:: *)
(*T[n_,k_]:=2^Length[PreT[n,k]]*)


(* ::Input:: *)
(*A004736[n_]:=Binomial[Floor[3/2+Sqrt[2*n]],2]-n+1*)


(* ::Input:: *)
(*A002260[n_]:=n-Binomial[Floor[1/2+Sqrt[2*n]],2]*)


(* ::Input:: *)
(*a[n_]:=T[A004736[n],A002260[n]]*)


(* ::Input:: *)
(*Table[a[n],{n,1,90}]*)


(* ::Input:: *)
(*U[n_,k_]:=Times@@((#[[1]])&/@PreT[n,k])*)


(* ::Input:: *)
(*Table[T[n,k],{n,1,12},{k,1,12}]//MatrixForm*)


(* ::Input:: *)
(*B[n_]:=BarChart[Table[2^T[n,k],{k,1,n*n}],AspectRatio->Automatic]*)


(* ::Input:: *)
(*Verifk[k_]:=(If[PreT[#+k,k]==PreT[#,k],True,Print[#];False])&/@Range[20]//DeleteDuplicates*)


(* ::Input:: *)
(*T[60,50]*)


(* ::Input:: *)
(*T[12,12]*)
