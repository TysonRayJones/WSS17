(* ::Package:: *)

Package["SimulationFunctions`"]


(* symbol exports *)

(* PackageExport[YOUR SYMBOL HERE] *)



(* function exports *)

PackageExport[evolveFunctionalWavefunction]


(* public functions *)

evolveFunctionalWavefunction::usage = "evolveFunctionalWavefunction[psi, potential, domains, duration] = evolves"

(*
PlotWavefunction::usage = 
	"PlotWavefunction[psi, domain] plots a 1D discrete/continuous, list/functional/interpolated/symbolic wavefunction"
*)


(* function definitions *)

getSchrodEqu[V_, r__, t_] :=
	 I D[\[Psi][r, t], t] ==  - Laplacian[\[Psi][r, t], {r}] / 2 + \[Psi][r, t] V[r]

getEvolveInitBoundaryEqus[psi_, V_, r__, t_] :=
	{
		getSchrodEqu[V, r, t],
		\[Psi][r, 0] == psi[r],
		DirichletCondition[\[Psi][r,t] == 0, True]
	}



(* r__: dummyvarx, dummyvary *)
(* domains__: {dummaryvarx, -5, 5}, {dummyvary, -5, 5} *)

evolveFunctionalWavefunction[psi_, potential_, r__Symbol, domains__List, duration:(_Real|_Integer)] :=
	
	NDSolveValue[
		getEvolveInitBoundaryEqus[psi, potential, r, t],
		\[Psi], {t, 0, duration}, 
		domains
	]
	
