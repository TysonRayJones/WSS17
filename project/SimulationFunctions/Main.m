(* ::Package:: *)

Package["SimulationFunctions`"]



(* package imports *)

PackageImport["PlotFunctions`"]  (* needed for ShowEvolution *)


(* symbol exports *)

PackageExport[PlotDomain]
PackageExport[TimeSteps]


(* function exports *)

PackageExport[NormaliseWavefunction]
PackageExport[EvolveWavefunction]
PackageExport[ShowRawEvolution]
PackageExport[ShowCachedEvolution]


(* public functions *)

NormaliseWavefunction::usage = 
	"NormaliseWavefunction[psi, xdomain] numerically normalises a 1D pure/interpolating function wavefunction"
	
NormaliseWavefunction::usage = 
	"NormaliseWavefunction[psi, xdomain, ydomain] numerically normalises a 2D pure/interpolating function wavefunction"

EvolveWavefunction::usage = 
	"EvolveWavefunction[psi, potential, domains, duration] evolves a given 1D or 2D initial wavefunction in a given potential over duration"

ShowRawEvolution::usage = 
	"ShowRawEvolution[psi, potential, domains, duration] simulates and plots a given 1D or 2D wavefunction's evolution over duration"
	
ShowCachedEvolution::usage = 
	"ShowCachedEvolution[psi, potential, domains, duration] simulates, caches and efficiently plots a given 1D or 2D wavefunction's evolution over duration"


(* function definitions *)





(* disable initial-cond and boundary-cond disagreement alerts *)
Off[NDSolveValue::ibcinc]




(* times and prints an expression's evaluation *)
TimeEigFlag = False;
SetAttributes[EchoTiming, HoldFirst]
EchoTiming[expr_, label_:"Timing"] := 
	If[
		TimeEigFlag,
		Last @ Echo[AbsoluteTiming @ expr, label <> " ", First],
		expr
	]
	
	


NormaliseWavefunction[psi:(_Function|_InterpolatingFunction), {xL_:-\[Infinity], xR_:\[Infinity]}] :=
	Function[{x}, psi[x]/Sqrt[NIntegrate[Abs[psi[r]]^2, {r, xL, xR}, Method -> {Automatic, "SymbolicProcessing" -> 0}]]]
	
	
NormaliseWavefunction[psi:(_Function|_InterpolatingFunction), {xL_:-\[Infinity], xR_:\[Infinity]}, {yL_:-\[Infinity], yR_:\[Infinity]}] :=
	Function[{x, y}, psi[x, y]/Sqrt[NIntegrate[Abs[psi[r1, r2]]^2, {r1, xL, xR}, {r2, yL, yR}, Method -> {Automatic, "SymbolicProcessing" -> 0}]]]


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
		getEvolveInitBoundaryEqus[
			NormaliseWavefunction[psi, domains], 
			potential, 
			r, 
			domains, 
			t
		],
		\[Psi], 
		Sequence @@ MapThread[ {#1, Sequence @@ #2} &, {{r}, {domains}}],
		{t, 0, duration}
	]
	

	

Options[ShowRawEvolution] := {
	PlotDomain -> None,
	TimeSteps -> False
}


ShowRawEvolution[psi_, potential_, domains__List, duration:(_Real|_Integer), options:OptionsPattern[]] :=

	DynamicModule[
		{wavef, plotdoms},
		
		(* re-instate timing and printing (if true) *)
		TimeEigFlag = OptionValue[{ShowRawEvolution, PlotWavefunction}, {options}, "TimeSteps"];
		
		(* simulate wavefunction *)
		wavef = EchoTiming[
			EvolveWavefunction[psi, potential, domains, duration],
			"simulating"
		];
	
		(* decide on using simulation or plot domain *)
		plotdoms = OptionValue[{ShowRawEvolution, PlotWavefunction}, {options}, PlotDomain];
		plotdoms = If[
			plotdoms === None,
			{domains}, 
			{plotdoms}
		];
		
		(* CONSULT OPTIONS; CACHE RESULTS! *)

		EchoTiming[
			Legended[
				Manipulate[
					PlotWavefunction[
						If[
							Length[{domains}] === 2,
							(wavef[#1, #2, t] &),
							(wavef[#1, t] &)
						],
						Sequence @@ Partition[Flatten[plotdoms],2],
						Potential -> potential,
						ShowBar -> False,
						Sequence[FilterRules[{options}, Options[PlotWavefunction]]]
					],
					
					(* avoid t=0 state; ambiguous phases *)
					{{t, duration/100, "time"}, 0, duration}
				],
				ColorBar[]
			],
			"animating"
		]
	]
	

Options[ShowCachedEvolution] := {
	PlotDomain -> None,
	TimeSteps -> False
}
	
(* CACHED VERSION *)
ShowCachedEvolution[psi_, potential_, domains__List, duration:(_Real|_Integer), options:OptionsPattern[]] :=

	DynamicModule[
		{wavef, plotdoms, frames, va, vp, vv},
		
		(* re-instate timing and printing (if true) *)
		TimeEigFlag = OptionValue[{ShowCachedEvolution, PlotWavefunction}, {options}, "TimeSteps"];
		
		(* simulate wavefunction *)
		wavef = EchoTiming[
			EvolveWavefunction[psi, potential, domains, duration],
			"simulating"
		];
	
		(* decide on using simulation or plot domain *)
		plotdoms = OptionValue[{ShowCachedEvolution, PlotWavefunction}, {options}, PlotDomain];
		plotdoms = If[
			plotdoms === None,
			{domains}, 
			{plotdoms}
		];
		
		(* enable mid-animation rotation of graph by dynamic view params *)
		va = 60 Degree;
		vp = {1, 1, 1};
		vv = {0, 0, 2.0};

		(* cache frames *)
		frames = 
			EchoTiming[
				Table[
					PlotWavefunction[
						If[
							Length[{domains}] === 2,
							(wavef[#1, #2, t] &),
							(wavef[#1, t] &)
						],
						Sequence @@ Partition[Flatten[plotdoms],2],
						Potential -> potential,
						ShowBar -> False,
						
						(* PointsActivePassive \[Rule] {50, 50}, *)
						ViewAngle -> Dynamic@va,
						ViewPoint -> Dynamic@vp,
						ViewVertical -> Dynamic@vv,
						
						Sequence[FilterRules[{options}, Options[PlotWavefunction]]]
					],
					{t, 0, duration, duration/10}   (* time increments *)
				],
				"caching frames"
			];
			
		(* animate frames *)
		EchoTiming[
			Legended[			
				ListAnimate[
					frames, 
					10,      (* fps *)
					Initialization :> (vp = {1,1,1}; vv = {0,0, 2.0};)
				],
				ColorBar[]
			], 
			"animating"
		]
	]
		
		
