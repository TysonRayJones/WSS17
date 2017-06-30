(* ::Package:: *)

Package["PlotFunctions`"]

(* PlotRange not exported *)
PackageExport[ShowBar]
PackageExport[Labels]
PackageExport[Potential]

PackageExport[PlotWavefunction]
PackageExport[ColorBar]

PlotWavefunction::usage = 
	"PlotWavefunction[psi, domain] plots a discrete/continuous, list/functional/interpolated/symbolic wavefunction"

ColorBar::usage =
	"ColorBar[title] returns a colorbar which can be legended outside plots. Avoid embedding!"


ColorBar[title_:"Arg[\[Psi]]"] :=
	BarLegend[
		{"Rainbow", {-\[Pi], \[Pi]}}, 
		LegendLabel -> title,
		"Ticks" -> N[{-\[Pi]+.01, -\[Pi]/2, 0, \[Pi]/2, \[Pi]-.01}],
		"TickLabels" -> {"-\[Pi]", "-\[Pi]/2", "0", "\[Pi]/2", "\[Pi]"}
	]


PlotWavefunction[psi_, domain_, args___] :=

	Which[		
		Head[psi] === InterpolatingFunction || Head[psi] === Function,
		plotContinuousWavefunction[psi, domain, args],
		
		Head[psi] === List,
		plotDiscreteWavefunction[psi, domain, args],
		
		True,
		plotSymbolicWavefunction[psi, domain, args]
	]
	

Options[plotContinuousWavefunction] = {
	"PlotRange" -> {0,1},   (* not exported *)
	"ShowBar" -> True,
	"Labels" -> {"x", "Abs[\[Psi][x]\!\(\*SuperscriptBox[\(]\), \(2\)]\)", "Arg[\[Psi][x]]"},
	"Potential" -> 0
}	

plotContinuousWavefunction[psi_, domain_, OptionsPattern[]] :=
		
	Legended[
		Show[
			ReplaceAll[
				Plot[
				
					(* plot probability density *)
					Abs[psi[x]]^2, 
					{x, domain[[1]], domain[[-1]]},   (* will hide grid error *)
					PlotRange -> OptionValue["PlotRange"],
					AxesLabel -> OptionValue["Labels"][[{1,2}]],
					
					(* fill colour based on complex phase *)
					ColorFunction -> (ColorData["Rainbow"][Rescale[Arg[psi[#]], {-\[Pi], \[Pi]}]]&),
					ColorFunctionScaling -> False,
					Filling -> Axis
				],
				
				(* allow independent outline and filling colour *)
				Line[pts_, _] :> {Black, Line[pts]}
			],
			Plot[
				processPotential[OptionValue["Potential"]][x],
				{x, domain[[1]], domain[[-1]]},
				PlotStyle -> {Thick, Red}
			]
		],
			
		(* show colorbar only when static plotting (else it's super laggy) *)
		If[
			OptionValue["ShowBar"], 
			ColorBar[OptionValue["Labels"][[3]]], 
			None
		]
	]
			
			
plotDiscreteWavefunction[psi_, {xL_, ___, xR_}, args___] :=
	
	(* interpolate and plot as continuous *)
	plotContinuousWavefunction[
		ListInterpolation[
			psi, 
			{{xL, xR}}
		],
		{xL, xR},    
		args 
	]
	

plotSymbolicWavefunction[psi_, {x_, xL_, xR_}, args___] :=
	
	(* convert to a pure function and plot as continuous *)
	plotContinuousWavefunction[
		Function @@ {x, psi},
		{xL, xR},
		
		(* convert symbolic potential to pure *)
		Sequence @@ ReplaceAll[
			Echo@{args},
			(Potential -> symb_) :> (Potential -> Function @@ {x, symb})
		]
	]
	
	
processPotential[potential_] :=
	potential
