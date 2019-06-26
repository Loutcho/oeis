(* ::Package:: *)

(* ::Input:: *)
(*G[{x_,y_}]:={x+y,x*y}*)


(* ::Input:: *)
(*Mem=<||>;*)


(* ::Input:: *)
(*GIterated[{x_,y_},n_]:=If[KeyExistsQ[Mem,n],Mem[n],Module[{GG},Print[n];GG=If[n==0,{x,y},Expand/@G[GIterated[{x,y},n-1]]];AssociateTo[Mem,n->GG];GG]]*)


(* ::Input:: *)
(*PreA[n_]:=CoefficientList[GIterated[{x,y},n][[2]],{x,y}]*)


(* ::Input:: *)
(*A[n_]:=Length[Select[Flatten[PreA[n]],(#!=0&)]]*)


(* ::Input:: *)
(*Table[A[n],{n,1,12}]*)


(* ::Input:: *)
(*Keys[Mem]*)


(* ::Input:: *)
(*Table[ArrayPlot[PreA[n],ColorFunction->"TemperatureMap"],{n,1,Max[Keys [Mem]]}]*)


(* ::Input:: *)
(*MyGrid[n_]:={Grid[Transpose[Table[i,{j,-1,-1},{i,0,Fibonacci[n+1]}]],Frame->All,Background->Gray],Grid[PreA[n],ItemSize->All,Frame->All]};MyGrid[8]*)


(* ::Input:: *)
(*T[n_]:=n*(n+1)/2*)


(* ::Input:: *)
(*F[n_]:=Fibonacci[n]*)


(* ::Input:: *)
(*V[n_]:=Sum[F[k]*(Sum[(n-2-l)*F[l],{l,k+1,n-3}]),{k,1,n-4}]*)


(* ::Input:: *)
(*Table[V[n],{n,1,20}]*)


(* ::Input:: *)
(*W[n_]:=Sum[(n-2-l)*T[F[l]],{l,1,n-3}]*)


(* ::Input:: *)
(*Table[W[n],{n,1,20}]*)


(* ::Input:: *)
(*AA[n_]:=(F[n+1])^2-T[n-1]-T[F[n-1]]-2*V[n]-2W[n]*)


(* ::Input:: *)
(*(*FullSimplify[F[n],{Element[n,Integers],n>0}]*)*)


(* ::Input:: *)
(*Table[AA[n]-A[n],{n,1,Max[Keys[Mem]]}]*)


(* ::Input:: *)
(*Table[AA[n],{n,1,50}]*)


(* ::Input:: *)
(*fname=FileNameJoin[{$TemporaryDirectory,"testfile"}];s=OpenWrite[fname]*)


(* ::Input:: *)
(*For[n=1,n<=200,n++,Write[s,TextString[{n,AA[n]},ListFormat->{""," ",""}]]];Close[s]*)
