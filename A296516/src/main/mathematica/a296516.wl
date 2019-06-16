(* ::Package:: *)

(* ::Input:: *)
(*{*)
(* {\[Placeholder]},*)
(* {\[Placeholder]}*)
(*}*)


(* ::Input:: *)
(*G[{x_,y_}]:={x+y,x*y};*)


(* ::Input:: *)
(*Mem=<||>;*)


(* ::Input:: *)
(*GIterated[{x_,y_},n_]:=If[KeyExistsQ[Mem,n],Mem[n],Module[{GG},Print[n];GG=If[n==0,{x,y},Expand/@G[GIterated[{x,y},n-1]]];AssociateTo[Mem,n->GG];GG]]*)


(* ::Input:: *)
(*P[n_]:=GIterated[{x,y},n][[1]];*)


(* ::Input:: *)
(*Q[n_]:=GIterated[{x,y},n][[2]];*)


(* ::Input:: *)
(*PreP[n_]:=CoefficientList[P[n],{x,y}];*)


(* ::Input:: *)
(*PreQ[n_]:=CoefficientList[Q[n],{x,y}];*)


(* ::Input:: *)
(*A[n_]:=Length[Select[Flatten[PreP[n]],(#!=0&)]];Table[A[n],{n,1,12}]*)


(* ::Input:: *)
(*Table[Column[{n,MatrixPlot[PreP[n]],MatrixPlot[PreQ[n]]}],{n,1,Max[Keys [Mem]]}]*)


(* ::Input:: *)
(*MatrixForm[PreP[8]]*)


(* ::Input:: *)
(*{2,3,5,10,22,52,128,323,827,2136,5546,14446}==Table[(Fibonacci[2n-1]+Fibonacci[n+1])/2+1,{n,1,12}]*)


(* ::Input:: *)
(*B[n_]:=(Fibonacci[2n-1]+Fibonacci[n+1])/2+1*)


(* ::Input:: *)
(*46601103890691607634812607997085822/17800037772979229611459186746219670//N*)


(* ::Input:: *)
(*Table[B[n],{n,1,30}]*)


(* ::Input:: *)
(*(* B[n] \[Equal] A005207[n] + 1 \[Equal]> fait \[AGrave] signaler sur OEIS ! 31/03/2018 *)*)


(* ::Input:: *)
(*ListPlot[Table[Log[PreP[12][[n,n]]],{n,1,140}]]*)


(* ::Input:: *)
(*Plot[x*Sqrt[1-x^2],{x,0,1}](*ressemblance*)*)


(* ::Input:: *)
(*n=10;{ListLinePlot[Table[Log/@Diagonal[PreP[n],d],{d,0,Fibonacci[n]}]],ListLinePlot[Table[Log/@Diagonal[PreQ[n],d],{d,0,Fibonacci[n]}]]}*)


(* ::Input:: *)
(*n=10;ListLinePlot[Table[Log/@Diagonal[Reverse[PreP[n]],d],{d,1,Fibonacci[n]}]]*)


(* ::Input:: *)
(*AntiDiagLenP[n_]:=Module[{Diag,Len},*)
(*Diag=Table[Diagonal[Reverse[PreP[n]],d],{d,-Fibonacci[n],Fibonacci[n]}];*)
(*Len=Length/@(Select[#,(#!=0)&]&)/@Diag*)
(*]*)
(*AntiDiagLenQ[n_]:=Module[{Diag,Len},*)
(*Diag=Table[Diagonal[Reverse[PreQ[n]],d],{d,-Fibonacci[n+1],Fibonacci[n+1]}];*)
(*Len=Length/@(Select[#,(#!=0)&]&)/@Diag*)
(*]*)


(* ::Input:: *)
(*(*Largeurs d'antidiagonales, alternativement paires et impaires pour raison de sym\[EAcute]trie*)*)


(* ::Input:: *)
(*Table[Column[{BarChart[AntiDiagLenP[n]],BarChart[AntiDiagLenQ[n]]}],{n,1,7}]*)


(* ::Input:: *)
(*{BarChart[Differences[AntiDiagLenP[10]]],BarChart[Differences[AntiDiagLenQ[9]]]}*)


(* ::Input:: *)
(**)


(* ::Input:: *)
(*n=8;AntiDiagLenP[n+1]-AntiDiagLenQ[n]*)


(* ::Input:: *)
(*Differences[AntiDiagLenP[2]]*)


(* ::Input:: *)
(*Differences[AntiDiagLenQ[8]]*)


(* ::Input:: *)
(*Table[Column[{n,Fibonacci[n]}],{n,1,10}]*)


(* ::Input:: *)
(*Table[Length[Select[AntiDiagLenQ[n],(#>0&)]],{n,1,10}]*)


(* ::Input:: *)
(*AntiDiagLenP[8]*)


(* ::Input:: *)
(*ListPlot[AntiDiagLenQ[8]]*)


(* ::Input:: *)
(*MapIndexed[p,AntiDiagLenP[8]]*)
