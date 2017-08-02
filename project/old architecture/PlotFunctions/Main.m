(* ::Package:: *)

Package["PlotFunctions`"]


(* symbol exports *)

PackageExport[Labels]
PackageExport[ShowBar]
PackageExport[Potential]
PackageExport[PotentialFilling]
PackageExport[PotentialTransform]
PackageExport[PointsActivePassive]

(* PlotRange not exported *)

(* ViewAngle not exported *)
(* ViewPoint not exported *)
(* ViewVertical not exported *)


(* function exports *)

PackageExport[PlotWavefunction]
PackageExport[ColorBar]


(* public functions *)

PlotWavefunction::usage = 
	"PlotWavefunction[psi, domain] plots a 1D discrete/continuous, list/functional/interpolated/symbolic wavefunction"
	
PlotWavefunction::usage = 
	"PlotWavefunction[psi, xdomain, ydomain] plots a 2D discrete/continuous, list/functional/interpolated/symbolic wavefunction"

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
	Labels -> {"x", "y", "Abs[\[Psi][x]\!\(\*SuperscriptBox[\(]\), \(2\)]\)", "Arg[\[Psi][x]]"},
	Potential -> None,
	PotentialFilling -> True,
	PotentialTransform -> (#&),
	PointsActivePassive -> {10, 50},
	
	(* not exported *)
	PlotRange -> {0,1},
	
	ViewAngle -> 60 Degree,    (* only relevant for Plot3D *)
	ViewPoint -> {1, 1, 1},
	ViewVertical -> {0, 0, 1},
	ImageSize -> Large
}




(* --------------- 1D plots --------------- *)


PlotWavefunction[psi_, domain_, OptionsPattern[]] :=

	Module[
		{options},
		
		(* convert OptionValues to assocation *)
		options = 
			<|ReplaceAll[
				Options[PlotWavefunction],
				(option_ -> _) :> (option -> OptionValue[option])
			]|>;
			
		(* adjust default labels for 1D plot *)
		options[Labels] =
			If[
				OptionValue[Labels] === Lookup[Options[PlotWavefunction], Labels],
				OptionValue[Labels][[{1, 3, 4}]],
				OptionValue[Labels]
			];
		
		(* branch based on psi type *)
		Which[		
			Head[psi] === InterpolatingFunction || Head[psi] === Function,
			plotFunctionalWavefunction[psi, domain, options],
			
			Head[psi] === List,
			plotListWavefunction[psi, domain, options],
			
			True,
			plotSymbolicWavefunction[psi, domain, options]
		]
	]
	

	

plotFunctionalWavefunction[psi_, {xL_, ___, xR_}, options_] :=

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
						]                            (* this should really be done in first-gen functions!!! *)
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
				
			
plotListWavefunction[psi_, {xL_, ___, xR_}, options_] :=
	
	(* interpolate and plot as functional *)
	plotFunctionalWavefunction[
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
	
	(* convert to a pure function and plot as functional *)
	plotFunctionalWavefunction[
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
	




(* --------------- 2D plots --------------- *)


PlotWavefunction[psi_, xDomain_, yDomain_, OptionsPattern[]] :=
	
	With[
	
		(* convert OptionValues to assocation *)
		{options = 
			<|ReplaceAll[
				Options[PlotWavefunction],
				(option_ -> _) :> (option -> OptionValue[option])
			]|>},
		
		(* branch based on psi type *)
		Which[
			Head[psi] === InterpolatingFunction || Head[psi] === Function,
			plotFunctionalWavefunction[psi, xDomain, yDomain, options],
			
			(* TODO *)
			Head[psi] === List,
			None,  
			
			(* TODO *)
			True,
			None
		]
	]
		
(* PASS WAVEFUNCTION AND POTENTIAL BEFORE CALLING THIS *)

(* options is an Association of every possible optional param (between PlotPotential and PlotWavefnuction) *)

plotFunctionalWavefunction[psi_, {xL_, xR_}, {yL_, yR_}, options_] :=

	Module[
		{plot},
	
		(* plot probability density *)
		plot =
			Plot3D[
				Abs[psi[x, y]]^2, {x, xL, xR}, {y, yL, yR},
				PlotRange-> options[PlotRange],
				AxesLabel-> options[Labels][[;;3]],
				
				(* colour based on complex phase *)
				Mesh -> None,
				Exclusions -> None,
				ColorFunction-> Function[{x, y}, ColorData["Rainbow"][Rescale[Arg[psi[x,y]], {-\[Pi], \[Pi]}]]],
				ColorFunctionScaling-> False,
				
				(* set resolution for static and dynamic plotting *)
				PlotPoints -> Apply[ControlActive, options[PointsActivePassive]],
				
				(* explicitly set angle vars for consistency when ListAnimating cache *)
				ViewAngle -> options[ViewAngle],
				ViewPoint -> options[ViewPoint],
				ViewVertical -> options[ViewVertical],
				
				ImageSize -> options[ImageSize]
			];
			
		(* optionally plot potential... *)
		plot = If[
			options[Potential] === None,
			plot,
			Show[
				plot,
				plotFunctionalPotential[
					options[Potential], 
					{xL, xR}, {yL, yR}, 
					options
				]
			]	
		];
		
		(* optionally show colorbar (should only be used in static plots) *)
		plot = If[
			options[ShowBar],
			Legended[
				plot,
				ColorBar[options[Labels][[4]]]
			],
			plot
		];
		
		(* return *)
		plot
	]
	
	
(* only recognises exlucisvely PlotWavefunction optional params. Params for PlotPotential will be adjusted prior claling *) 
	
plotFunctionalPotential[potential_, {xL_, xR_}, {yL_, yR_}, options_] :=
	Plot3D[
		Composition[options[PotentialTransform], potential][x, y],
		{x, xL, xR}, {y, yL, yR},
		ClippingStyle -> None,
		Exclusions -> None,
		
		PlotRange -> options[PlotRange],
		PlotStyle -> {{Opacity[0.3], Red}},
		Mesh -> 5,
		MeshStyle -> {{Red}},
		
		PlotPoints -> Apply[ControlActive, options[PointsActivePassive]]
	]


