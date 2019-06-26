(* ::Package:: *)

(* ::Input:: *)
(*a[n_]:=Module[*)
(*{x=0,y=0,p=0,q=n,L={}},*)
(*While[q>0,*)
(*If[p!=0,L=Append[L,n-p*q]];*)
(*If[y<0,x=y+q;q--];*)
(*If[y>0,p++;x=y-p;];*)
(*If[y==0,*)
(*p++;*)
(*x=0;*)
(*L=Append[L,0];*)
(*q--;*)
(*];*)
(*y=x+p-q;*)
(*];*)
(*L*)
(*]*)


(* ::Input:: *)
(*a[6]*)


(* ::Input:: *)
(*n=6;ArrayPad[#,n-(Length[#]+1)/2]&/@(a/@Range[1,n])*)


(* ::Input:: *)
(*n=24;ArrayPlot[ArrayPad[#,n-(Length[#]+1)/2]&/@(a/@Range[1,n]),ColorRules->{0->Blue}]*)


(* ::Input:: *)
(**)
(*B[n_,t_]:=(n-#*t-#^2&)/@Range[Floor[(-t-Sqrt[t^ 2+4n])/2],Ceiling[(-t+Sqrt[t^ 2+4n])/2]]*)
(*B[10,4]*)
(*A[n_,t_]:=Min[(n-#*t-#^2&)/@Range[Ceiling[(-t-Sqrt[t^ 2+4n])/2],Floor[(-t+Sqrt[t^ 2+4n])/2]]*)


(* ::Input:: *)
(*A[10,4]*)


(* ::Input:: *)
(*x=24;DiscretePlot3D[A[n, t], {n, 1, x}, {t, -(x-1), (x-1)}, ExtentSize -> Full]*)


(* ::Input:: *)
(*A288969[n_,t_]:=Module[{x},*)
(*x=Floor[(-t+Sqrt[t^2+4n])/2];*)
(*n-x(t+x)*)
(*]*)


(* ::Input:: *)
(*A288969[20,4]*)


(* ::Input:: *)
(*x=24;DiscretePlot3D[A288969[n, t], {n, 1, x}, {t, -(x-1), (x-1)}, ExtentSize -> Full]*)


(* ::Input:: *)
(*Recurrence[n_,t_]:=If[n<=t,n,Recurrence[n-(t+1),t+2]]*)
(**)


(* ::Input:: *)
(*Grid[Table[Recurrence[n,t],{n,0,25},{t,0,24}],Frame->All]*)


(* ::Input:: *)
(*FEven[x_]:=x^ 2*)


(* ::Input:: *)
(*InvFEven[x_]:=Sqrt[x]*)


(* ::Input:: *)
(*GEven[n_]:=n-FEven[Floor[InvFEven[n]]]*)


(* ::Input:: *)
(*FOdd[x_]:=x*(x+1)*)


(* ::Input:: *)
(*InvFOdd[x_]:=(Sqrt[1+4x]-1)/2*)


(* ::Input:: *)
(*GOdd[n_]:=n-FOdd[Floor[InvFOdd[n]]]*)


(* ::Input:: *)
(*G[n_,t_]:=Module[*)
(*{e,k,x},*)
(*e=EvenQ[t];*)
(*k=If[e,t/2,(t-1)/2];*)
(*x=n+If[e,FEven[k],FOdd[k]];*)
(*If[e,GEven[x],GOdd[x]]*)
(*]*)


(* ::Input:: *)
(*Table[GEven[n],{n,0,25}] (*A053186*)*)


(* ::Input:: *)
(*Table[GOdd[n],{n,0,80}]*)


(* ::Input:: *)
(*x=24;DiscretePlot3D[G[n, t], {n, 1, x}, {t, -(x-1), (x-1)}, ExtentSize -> Full]*)


(* ::Input:: *)
(*F[n_,t_]:=Module[{x},x=Floor[(-t+Sqrt[t^2+4n])/2];n-x(t+x)];*)
(*T[n_,t_]:=F[n-1,t]-F[n,t]+1;ARow[n_]:=Table[T[n,t],{t,-(n-1),+(n-1)}];Table[ARow[n],{n,1,20}]*)
