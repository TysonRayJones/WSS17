(* ::Package:: *)

BeginPackage["plotFunctions`"]


plotWavefunction::usage = 
	"plotWavefunction[psi, domain] plots a discrete/continuous, list/functional/interpolated/symbolic wavefunction"
	

colorBar::usage =
	"colorBar[title] returns a colorbar which can be legended outside plots. Avoid embedding!"


Begin["`Private`"]


colorBar[title_:"Arg[\[Psi]]"] :=
	BarLegend[
		{"Rainbow", {-\[Pi], \[Pi]}}, 
		LegendLabel -> title,
		"Ticks" -> N[{-\[Pi]+.01, -\[Pi]/2, 0, \[Pi]/2, \[Pi]-.01}],
		"TickLabels" -> {"-\[Pi]", "-\[Pi]/2", "0", "\[Pi]/2", "\[Pi]"}
	]


plotWavefunction[psi_, domain_, args___] :=

	Which[		
		Head[psi] === InterpolatingFunction || Head[psi] === Function,
		plotContinuousWavefunction[psi, domain, args],
		
		Head[psi] === List,
		plotDiscreteWavefunction[psi, domain, args],
		
		True,
		plotSymbolicWavefunction[psi, domain, args]
	]
	

Options[plotContinuousWavefunction] = {
	"plotRange" -> {0,1}, 
	"showBar" -> True,
	"labels" -> {"x", "Abs[\[Psi][x]\!\(\*SuperscriptBox[\(]\), \(2\)]\)", "Arg[\[Psi][x]]"},
	"potential" -> 0
}	

plotContinuousWavefunction[psi_, domain_, OptionsPattern[]] :=
		
	Legended[
		Show[
			ReplaceAll[
				Plot[
				
					(* plot probability density *)
					Abs[psi[x]]^2, 
					{x, domain[[1]], domain[[-1]]},   (* will hide grid error *)
					PlotRange -> OptionValue["plotRange"],
					AxesLabel -> OptionValue["labels"][[{1,2}]],
					
					(* fill colour based on complex phase *)
					ColorFunction -> (ColorData["Rainbow"][Rescale[Arg[psi[#]], {-\[Pi], \[Pi]}]]&),
					ColorFunctionScaling -> False,
					Filling -> Axis
				],
				
				(* allow independent outline and filling colour *)
				Line[pts_, _] :> {Black, Line[pts]}
			],
			Plot[
				processPotential[OptionValue["potential"]][x],
				{x, domain[[1]], domain[[-1]]},
				PlotStyle -> {Thick, Red}
			]
		],
			
		(* show colorbar only when static plotting (else it's super laggy) *)
		If[
			OptionValue["showBar"], 
			colorBar[OptionValue["labels"][[3]]], 
			None
		]
	]
			
			
plotDiscreteWavefunction[psi_, domain_, args___] :=
	
	(* interpolate and plot as continuous *)
	plotContinuousWavefunction[
		ListInterpolation[
			psi, 
			{{domain[[1]], domain[[-1]]}}
		],
		
		(* continuous plot will consult only end-points *)
		domain,    
		args 
	]
	

plotSymbolicWavefunction[psi_, {x_, xL_, xR_}, args___] :=
	
	(* convert to a pure function and plot as continuous *)
	plotContinuousWavefunction[
	
		(*
		(psi /. domain[[1]] \[Rule] #)&,
		*)
		Function @@ {x, psi},
		
		(* assume first domain element is independent var... *)
		(*
		domain[[2;;]],
		*)
		{xL, xR},
		
		(* ...and remove it from potential, replacing with pure function *)
		(*
		args /. {
			(potential \[Rule] symb_) \[RuleDelayed] 
			(potential \[Rule] (Evaluate[symb /. domain[[1]] \[RuleDelayed] #]&))
		}
		*)
		(*
		{args} /. {
			(potential \[Rule] symb_) \[RuleDelayed] (potential \[Rule] Function @@ {x, symb})
		}
		*)
		Sequence @@ Echo[ReplaceAll[
			Echo@{args},
			("potential" -> symb_) :> ("potential" -> Function @@ {x, symb})
		]]
	]
	

(*
plotSymbolicWavefunction[psi_, {x_, xL_, xR_}, args___] :=
	(* convert to a pure function and plot as continuous *)
	plotContinuousWavefunction[
		Function @@ {x, psi},
		{xL, xR},
		{args} /. {
			(potential \[Rule] symb_) \[RuleDelayed] (potential \[Rule] Function @@ {x, symb})
		}
	]
*)
	
processPotential[potential_] :=
	potential
	

End[]
EndPackage[]



