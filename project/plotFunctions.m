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
	
	
plotContinuousWavefunction[psi_, domain_, range_:{0,1}, labels_:{"x", "Abs[\[Psi][x]\!\(\*SuperscriptBox[\(]\), \(2\)]\)", "Arg[\[Psi][x]]/2\[Pi]"}, showbar_:True] :=
	
	(* allow independent outline and filling colour *)
	ReplaceAll[
		Plot[
		
			(* plot probability density *)
			Abs[psi[x]]^2, 
			{x, domain[[1]], domain[[-1]]},   (* will hide grid error *)
			PlotRange -> range,
			AxesLabel -> labels[[{1,2}]],
			
			(* fill colour based on complex phase *)
			ColorFunction -> (ColorData["Rainbow"][Rescale[Arg[psi[#]], {-\[Pi], \[Pi]}]]&),
			ColorFunctionScaling -> False,
			Filling -> Axis,
			
			(* color bar is laggy; only to be used statically. bug forces normalisation here *)
			PlotLegends -> If[
				showbar,
				BarLegend["Rainbow", LegendLabel -> labels[[3]]],
				None
			]
		],
		Line[pts_, _] :> {Black, Line[pts]}
	]
			
			
plotDiscreteWavefunction[psi_, domain_, args___] :=
	
	(* interpolate and plot as continuous *)
	plotContinuousWavefunction[
		ListInterpolation[
			psi, 
			{{domain[[1]], domain[[-1]]}}
		],
		domain,    (* continuous plot will consult end-points *)
		args
	]
	

plotSymbolicWavefunction[psi_, domain_, args___] :=
	
	(* convert to a pure function and plot as continuous *)
	plotContinuousWavefunction[
		(psi /. domain[[1]] -> #)&,
		
		(* assume first domain element is independent var *)
		domain[[2;;]],
		args
	]
	

End[]
EndPackage[]



