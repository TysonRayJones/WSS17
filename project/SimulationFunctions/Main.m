(* ::Package:: *)

Package["SimulationFunctions`"]


(* symbol exports *)

(* PackageExport[YOUR SYMBOL HERE] *)



(* function exports *)

(* PackageExport[YOUR FUNCTION HERE] *)


(* public functions *)

(*
PlotWavefunction::usage = 
	"PlotWavefunction[psi, domain] plots a 1D discrete/continuous, list/functional/interpolated/symbolic wavefunction"
*)


(* function definitions *)

schrodEqu[V_, r__, t_] :=
	 D[\[Psi][r, t], t] ==  - Laplacian[\[Psi][r, t], {r}] / 2 + \[Psi][r, t] V[r]



	NDSolveValue[
		{
			schrodEqu[1/2 #^2&, x, t],
			\[Psi][x, 0] == Exp[-x^2] HermiteH[1, x],
			\[Psi][-5, t] == 0,
			\[Psi][5, t] == 0
		},
		\[Psi], 
		{x, -5, 5},
		{t, 0, 1}
	]


Laplacian[x^3 + x, {x}]
