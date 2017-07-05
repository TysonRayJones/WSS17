(* ::Package:: *)

Package["SimulationFunctions`"]


(* symbol exports *)

(* PackageExport[YOUR SYMBOL HERE] *)



(* function exports *)

PackageExport[EvolveWavefunction]


(* public functions *)

EvolveWavefunction::usage = "EvolveWavefunction[psi, potential, domains, duration] evolves a given initial wavefunction in a given potential over duration"


(* function definitions *)

getSchrodEqu[V_, r__, t_] :=
	 I D[\[Psi][r, t], t] ==  - Laplacian[\[Psi][r, t], {r}] / 2 + \[Psi][r, t] V[r]
	 
getBoundaryEqus[vars_List, doms_List, t_] :=
	With[
		{subs = 
			Partition[Flatten[
				Table[
					{vars[[i]] -> doms[[i]][[1]], vars[[i]] -> doms[[i]][[2]]}, 
					{i, Length[vars]}
				]
			], 1]},
		Sequence @@ Map[Apply[\[Psi], #] == 0 &, Join[vars, {t}] /. subs]
	]

getEvolveInitBoundaryEqus[psi_, V_, r__Symbol, doms__List, t_] :=
	{
		getSchrodEqu[V, r, t],
		getBoundaryEqus[{r}, {doms}, t],
		\[Psi][r, 0] == psi[r]
	}
	
EvolveWavefunction[psi_, potential_, domains__List, duration:(_Real|_Integer)] :=
	
	Which[
		Head[psi] === Function || Head[psi] === InterpolatingFunction,
		evolveFunctionalWavefunction[psi, potential, Sequence @@ {x,y,z}[[;;Length[{domains}]]], domains, duration],
		
		Head[psi] === List,
		None,
		
		True,
		None
	]
	

(* r__: dummyvarx, dummyvary *)
(* domains__: {dummaryvarx, -5, 5}, {dummyvary, -5, 5} *)

evolveFunctionalWavefunction[psi_, potential_, r__Symbol, domains__List, duration:(_Real|_Integer)] :=
	
	NDSolveValue[
		getEvolveInitBoundaryEqus[psi, potential, r, domains, t],
		\[Psi], 
		Sequence @@ MapThread[ {#1, Sequence @@ #2} &, {{r}, {domains}}],
		{t, 0, duration}
	]
	
