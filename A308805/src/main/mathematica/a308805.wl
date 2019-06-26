(* ::Package:: *)

(* ::Input:: *)
(*F[n_,d_]:=2n(n-1)+d-n/d*)


(* ::Input:: *)
(*G[n_]:=F[n,#]&/@Divisors[n]*)


(* ::Input:: *)
(*A[n_]:=Flatten[G/@Range[n]] *)


(* ::Input:: *)
(*A[20]// ListPlot*)


(* ::Input:: *)
(*Table[2*x*y*(x*y-1)-x+y,{x,1,10},{y,1,10}]//MatrixForm*)


(* ::Input:: *)
(*Table[F[n,1],{n,1,25}]*)


(* ::Input:: *)
(*Table[(n-1)*(2*n-1),{n,1,25}](* A014105[n-1] *)*)


(* ::Input:: *)
(*Table[F[n,n],{n,1,25}]*)


(* ::Input:: *)
(*Table[(n-1)*(2*n+1),{n,1,25}] (* A014106[n-1] *)*)


(* ::Input:: *)
(*f[{x_,y_}]:=2*x*y*(x*y-1)-x+y*)


(* ::Input:: *)
(*p[{x_,y_}]:=((x+y)^2+3*x+y)/2 (* Cantor polynomial *)*)


(* ::Input:: *)
(*d[{x_,y_}]:={(x+1)*(y-1),(x-1)*(y+1)} (* Divisor plot *)*)


(* ::Input:: *)
(*Simplify[p[d[{x,y}]]-f[{x,y}]] (* f = p o d *)*)


(* ::Input:: *)
(*Flatten[G/@Range[10!,10!+10]] //ListPlot*)
