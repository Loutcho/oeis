(* ::Package:: *)

(* ::Input:: *)
(*iMax[k_,n_]:=PrimePi[k^2-2*n+1]*)
(*f[k_,n_]:=IntegerPartitions[k^2-1,{n},Table[Prime[i],{i,1,iMax[k,n]}]]*)
(*a[n_]:=Module[{k=1},While[f[k,n]=={},k++];k]*)
(*Table[a[n],{n,0,100}]*)
(*s=OpenWrite["b101776.txt"];n=0;*)
(*While[n<=200,*)
(*WriteString[s,n," ",a[n],"\n"];*)
(*n++];Close[s]*)



