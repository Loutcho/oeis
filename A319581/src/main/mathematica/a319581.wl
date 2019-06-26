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
(*T[n_,k_]:=Length[PreT[n,k]]*)


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
(*Table[U[n,k],{n,1,15},{k,1,15}]//MatrixForm*)


(* ::Input:: *)
(*Table[T[n,k],{n,1,12},{k,1,12}]//MatrixForm*)


(* ::Input:: *)
(*B[n_]:=BarChart[Table[2^T[n,k],{k,1,n*n}],AspectRatio->Automatic]*)


(* ::Input:: *)
(*B[12]*)


(* ::Input:: *)
(*Table[T[n,k],{n,1,200},{k,1,200}]//MatrixPlot*)


(* ::Input:: *)
(*Verifk[k_]:=(If[PreT[#+k,k]==PreT[#,k],True,Print[#];False])&/@Range[20]//DeleteDuplicates*)


(* ::Input:: *)
(*Verifn[n_]:=(PreT[n,#+n^2]==PreT[n,#])&/@Range[10000]//DeleteDuplicates*)


(* ::Input:: *)
(*{PreT[12,60],PreT[12,204]}*)


(* ::Input:: *)
(*For[k=1,k<=20,k++,Print[{k,Verifk[k]}]]*)


(* ::Input:: *)
(*For[n=21,n<=40,n++,Print[{n,Verifn[n]}]]*)


(* ::Input:: *)
(*TT[n_,k_]:=2^(T[n,k]+T[k,n])-GCD[n,k]//N*)


(* ::Input:: *)
(*ListPlot3D[Table[TT[n,k],{n,1,15},{k,1,15}],Mesh->None,InterpolationOrder->0,ColorFunction->"SouthwestColors"]*)


(* ::Input:: *)
(*n=2^3*5*7^3*)


(* ::Input:: *)
(*k=2^2*5^3*7*)


(* ::Input:: *)
(*d=GCD[n,k]*)


(* ::Input:: *)
(*T[n,k]*)


(* ::Input:: *)
(*T[n/d,k]*)


(* ::Input:: *)
(*T[d,k]*)


(* ::Input:: *)
(*T[5^1*7^3,2^2]*)
