(* ::Package:: *)

Package["PlotFunctions`"]


(* symbol exports *)

PackageExport[Labels]
PackageExport[ShowBar]
PackageExport[Potential]
PackageExport[PotentialFilling]
PackageExport[PotentialTransform]
(* PlotRange not exported *)


(* function exports *)

PackageExport[PlotWavefunction]
PackageExport[ColorBar]


(* public functions *)

PlotWavefunction::usage = 
	"PlotWavefunction[psi, domain] plots a discrete/continuous, list/functional/interpolated/symbolic wavefunction"

ColorBar::usage =
	"ColorBar[title] returns a colorbar which can be legended outside plots. Avoid embedding!"


(* function definitions *)

ColorBar[title_:"Arg[\[Psi]]"] :=
	BarLegend[
		{"Rainbow", {-\[Pi], \[Pi]}}, 
		LegendLabel -> title,
		"Ticks" -> N[{-\[Pi]+.01, -\[Pi]/2, 0, \[Pi]/2, \[Pi]-.01}],
		"TickLabels" -> {"-\[Pi]", "-\[Pi]/2", "0", "\[Pi]/2", "\[Pi]"}
	]
	
	
Options[PlotWavefunction] = {

	(* exported *)
	ShowBar -> True,
	Labels -> {"x", "Abs[\[Psi][x]\!\(\*SuperscriptBox[\(]\), \(2\)]\)", "Arg[\[Psi][x]]"},
	Potential -> None,
	PotentialFilling -> True,
	PotentialTransform -> (#&),
	
	(* not exported *)
	PlotRange -> {0,1} 
}


PlotWavefunction[psi_, domain_, OptionsPattern[]] :=

	(* convert OptionValues to assocation *)
	With[
		{options = 
			<|ReplaceAll[
				Options[PlotWavefunction],
				(option_ -> _) :> (option -> OptionValue[option])
			]|>
		},
		
		(* branch based on psi type *)
		Which[		
			Head[psi] === InterpolatingFunction || Head[psi] === Function,
			plotContinuousWavefunction[psi, domain, options],
			
			Head[psi] === List,
			plotDiscreteWavefunction[psi, domain, options],
			
			True,
			plotSymbolicWavefunction[psi, domain, options]
		]
	]
	

	

plotContinuousWavefunction[psi_, {xL_, ___, xR_}, options_] :=

	(* add elements to plot, one by one *)
	Module[
		{plot}, 
		
		(* plot probability density *)
		plot = ReplaceAll[
			Plot[
				Abs[psi[x]]^2, 
				{x, xL, xR},   
				PlotRange -> options[PlotRange],
				AxesLabel -> options[Labels][[{1,2}]],
				
				(* fill colour based on complex phase *)
				ColorFunction -> (ColorData["Rainbow"][Rescale[Arg[psi[#]], {-\[Pi], \[Pi]}]]&),
				ColorFunctionScaling -> False,
				Filling -> Axis
			],		
			(* allow independent outline and filling colour *)
			Line[pts_, _] :> {Black, Line[pts]}
		];
		
		(* optionally show potential *)
		plot = If[
			options[Potential] === None,
			plot,
			
			(* ensure potential is a function, and optionally transform *)
			Show[
				plot,
				With[
					{processedPotential = 
						processPotential[
							options[Potential], 
							options[PotentialTransform],
							{xL, xR}
						]
					},
				
					(* plot transformed potential *)
					Plot[
						processedPotential[x],
						{x, xL, xR},
						PlotRange -> options[PlotRange],
						Exclusions -> None,
						Filling -> If[options[PotentialFilling], Axis, None],
						PlotStyle -> {Thick, Red}
					]
				]
			]
		];
		
		(* optionally show colorbar (should only be used in static plot) *)
		plot = If[
			options[ShowBar],
			Legended[
				plot,
				ColorBar[options[Labels][[3]]]
			],
			plot
		];
		
		(* return *)
		plot
	]
				
			
plotDiscreteWavefunction[psi_, {xL_, ___, xR_}, options_] :=
	
	(* interpolate and plot as continuous *)
	plotContinuousWavefunction[
		ListInterpolation[
			psi, 
			{{xL, xR}}
		],
		{xL, xR},    
		options
	]
	


plotSymbolicWavefunction[psi_, {xL_, xR_}, options_] :=

	(* not passing a variable is valid if psi is a constant *)
	plotSymbolicWavefunction[psi, {dummaryvar, xL, xR}, options]


plotSymbolicWavefunction[psi_, {x_, xL_, xR_}, options_] :=
	
	(* convert to a pure function and plot as continuous *)
	plotContinuousWavefunction[
		Function @@ {x, psi},
		{xL, xR},
		
		(* convert symbolic potential to pure func by unwrapping assoc, updating rule, conv to assoc *)
		<|ReplaceAll[
			Normal[options],
			(Potential -> symb:Except[_List|_Function|_InterpolatingFunction]) :> 
			(Potential -> Function @@ {x, symb})
		]|>
		
	]
	
	
processPotential[potential_, transform_, domain_] :=
	
	(* potential and psi format can be uncorrelated (unless symbolic) *)
	Which[		
	
		(* transform existing functions *)
		Head[potential] === InterpolatingFunction || Head[potential] === Function,
		Composition[transform, potential],
		
		(* interpolate list to function then transform *)
		Head[potential] === List,
		Composition[
			transform, 
			ListInterpolation[
				potential,
				{domain}
			]
		]
	]
	
