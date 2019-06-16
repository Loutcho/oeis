(* ::Package:: *)

(* ::Input:: *)
(*ListPlot[Module[{n=16},(Mod[n,#]&)/@Range[n+2]]]*)


(* ::Input:: *)
(*a[n_]:=Differences[(Mod[n,#]&)/@Range[n+2]];a[51]*)


(* ::Input:: *)
(*a[n_]:=Module[{d=Differences[(Mod[n,#]&)/@Range[n+2]],r=1,k},For[k=2,k<=Length[d],k++,If[d[[k]]!=d[[k-1]],r++]];r];a/@Range[0,68]*)


(* ::Input:: *)
(*f[n_]:=N[a[n]/Sqrt[n],10]*)


(* ::Input:: *)
(*g[n_]:=N[a[n]-Sqrt[8*n],10]*)


(* ::Input:: *)
(*Column[{#,a[#]}]&/@Range[0,100]*)


(* ::Input:: *)
(*ListPlot[Table[f[n],{n,1,1000}]]*)


(* ::Input:: *)
(*f[10000000]*)


(* ::Input:: *)
(*f[20000000]*)


(* ::Input:: *)
(*f[40000000]*)


(* ::Input:: *)
(*f[50000000]*)


(* ::Input:: *)
(*ListPlot[Table[g[n],{n,1,1000}]]*)


(* ::Input:: *)
(*a[n_]:=Module[{m1=Mod[n,1],m2=Mod[n,2],m3,d21,d32,k,r=1},d21=m2-m1;For[k=3,k<=n+2,k++,m3=Mod[n,k];d32=m3-m2;If[d32!=d21,r++];m1=m2;m2=m3;d21=d32];r]*)


(* ::Input:: *)
(*a[1000000]*)


(* ::Input:: *)
(*N[19995/Sqrt[50000000],15]*)


(* ::Input:: *)
(*N[25287/Sqrt[80000000],15]*)


(* ::Input:: *)
(*N[35771/Sqrt[160000000],15]*)


(* ::Input:: *)
(*N[50589/Sqrt[320000000],15]*)


(* ::Input:: *)
(*N[Sqrt[8],15]*)


(* ::Input:: *)
(*(*PARI: a(n)=m1=n%1;m2=n%2;d21=m2-m1;r=1;for(k=3,n+2,if(k%1000000\[Equal]0,print1(k,"% "));m3=n%k;d32=m3-m2;if(d32\[NotEqual]d21,r++);m1=m2;m2=m3;d21=d32);r *)*)
(*(* a(10000000000) = 282834 *)*)


(* ::Input:: *)
(*N[282834/Sqrt[10000000000],15]*)


(* ::Input:: *)
(*2.82834`15.^2*)
