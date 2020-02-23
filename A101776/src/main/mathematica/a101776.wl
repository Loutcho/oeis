(* ::Package:: *)

(* ::Input:: *)
(*iMax[k_,n_]:=PrimePi[k^2-2*n+1]*)
(*s[n_]:=Ceiling[Sqrt[2*n+1]]*)
(*f[k_,n_]:=IntegerPartitions[k^2-1,{n},Table[Prime[i],{i,1,iMax[k,n]}]]*)
(*a[n_]:=Module[{k=s[n]},While[f[k,n]=={},k++];k](* conjectured: a[n]:=Ceiling[Sqrt[2*n+1]] *)*)
(*b[n_]:=f[a[n],n]*)
(*c[n_]:=Length[b[n]]*)
(*Table[c[n],{n,0,60}]*)
(*ListPlot[Table[c[n],{n,0,400}]]*)
(*Module[{n=0,pos=0,lg},While[pos<=400,lg=2*Floor[n/2]+1;Print["reversed ",Reverse[Table[c[k],{k,pos,pos+lg-1}]]];pos+=lg;n++]]*)
(**)



