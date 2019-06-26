(* ::Package:: *)

(* ::Input:: *)
(*lt[x_,y_]:=Module[*)
(*{c,d,xx,yy,u,v},*)
(*{c,d}=IntegerExponent[#,2]& /@{x,y};*)
(*xx=x/2^c;*)
(*yy=y/2^d;*)
(*u=If[xx==1,\[Infinity],c];*)
(*v=If[yy==1,\[Infinity],d];*)
(*If[u!=v,u<v,If[u==\[Infinity],c>d,xx<yy]]]*)


(* ::Input:: *)
(*row[n_]:=Sort[Range[n],lt]*)


(* ::Input:: *)
(*a[n_]:=First[FirstPosition[row[n],n]]*)


(* ::Input:: *)
(*Table[Column[{n,a[n]}],{n,1,150}]*)


(* ::Input:: *)
(*Table[a[64*n+32]-(63*n+26),{n,1,10}]*)


(* ::Input:: *)
(*Table[a[128*n+64]-(127*n+57),{n,1,4}]*)


(* ::Input:: *)
(*Table[a[256*n+128]-(255*n+120),{n,1,4}]*)


(* ::Input:: *)
(*Table[a[512*n+256]-(511*n+247),{n,1,4}]*)


(* ::Input:: *)
(*eulerian[n_]:=2^n-1-n*)


(* ::Input:: *)
(*A[n_]:=Module[{c,xx,k},c=IntegerExponent[n,2];xx=n/2^c;k=(xx-1)/2;(2^(c+1)-1)*k+eulerian[c]+Boole[xx==1]]*)


(* ::Input:: *)
(*B[n_]:=Module[{v},v=IntegerExponent[n,2];(1-1/2^(v+1))*n-1/2-v+Boole[n==2^v]]*)


(* ::Input:: *)
(*Table[B[n],{n,1,100}]*)


(* ::Input:: *)
(*Table[B[n],{n,1,10000}]//ListPlot*)


(* ::Input:: *)
(*Table[(11^n-1)/2,{n,1,10}]*)


(* ::Input:: *)
(*Table[a[9^n]//N,{n,1,4}]*)


(* ::Input:: *)
(*Table[lt[n,k],{n,1,8},{k,1,8}]*)


(* ::Input:: *)
(*row/@Range[20]//{MatrixForm[#],MatrixPlot[#]}&*)


(* ::Input:: *)
(*row/@Range[13]//Flatten*)


(* ::Input:: *)
(*MatrixPlot[row/@Range[250],PixelConstrained->3]*)
