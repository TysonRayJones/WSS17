(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	- function to plot a symbolic, analytic wavefunction (has a time dependence), 
		in 1D space and 2D space
	- same ^ but with function wavefunctions
	- function to actually calc evolution of a static wavefunction
	- options should offer pre-caching
*)





(* SYMBOL EXPORTS *)





(* FUNCTION EXPORTS *)

PackageExport[PlotEvolution]





(* FUNCTION USAGE MESSAGES *)





(* FUNCTION ERROR MESSAGES *)





(* PRIVATE SYMBOL IMPORTS *)

realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

func1DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func1DQ;
func2DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func2DQ;

convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;
convertToEndPoints = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToEndPoints;

plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;

extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;
extractOptionsFromDefaults = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptionsFromDefaults

defaultProbDensityLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultProbDensityLabel;
defaultXLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultXLabel;
defaultYLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultYLabel;

plot1DWavefunction = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DWavefunction
plot1DPotential = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DPotential;
addColorBar = WavefunctionSolver`PlotFunctions`PackagePrivate`addColorBar;




(* CONSTANTS *)

defaultTimeLabel = "time";






(* PRIVATE FUNCTION DEFINITIONS *)

(* extend convertToFunction to handle a third (time) dimension *)
convertToFunction[
	symbolic_,
	{symb1_Symbol, _?realNumQ, _?realNumQ},
	{symb2_Symbol, _?realNumQ, _?realNumQ},
	{symb3_Symbol, _?realNumQ, _?realNumQ}
] :=
	Function @@ {{symb1, symb2, symb3}, symbolic}



animate1DEvolution[
	probpotPlot:_Hold,
	{t_Symbol, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[Animate]
] :=
	Animate[
		ReleaseHold[probpotPlot], 
		{{t, t0, t}, t0, t1},
		
		(* apply over-riding user-given Animate options *)
		options,
		
		(* otherwise apply this styling *)
		Paneled -> False
	]
		
	

combine1DProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:None
] :=
	probPlot
	
combine1DProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:_Graphics
] :=
	Hold @ Show[
		ReleaseHold @ probPlot,
		potentialPlot
	]



plot1DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 2D: [x, t] *)
	potential:(_Function|_InterpolatingFunction|None),  (* 1D: [x] *)
	xDomain:{x_Symbol, xL_?realNumQ, xR_?realNumQ},
	tDomain:{t_Symbol, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions1D, Animate} // Flatten]
] :=
	Module[
		{probabilityPlot, potentialPlot, rawAnimation, cbarAnimation},
		
		(* prepare (held) probability without sources of slow animation (we'll restore externally if desired *)
		probabilityPlot = Hold @ PlotWavefunction[
			wavef[x, t], xDomain,  Potential -> None, ShowColorBar -> False,
			Evaluate @ extractOptions[options, plotOptionFunctions1D]			
		];
		
		(* plot static potential with automatic range (it'll use Show range); may return none *)
		potentialPlot = plot1DPotential[
			potential, {xL, xR}, Automatic, 
			Evaluate @ extractOptions[options, plotOptionFunctions1D]
		];
		
		(* merge probability and potential (into a held Show), then animate *)
		rawAnimation = animate1DEvolution[
			combine1DProbPotentialPlots[probabilityPlot, potentialPlot],
			tDomain, 
			Evaluate @ extractOptions[options, Animate]
		];
		
		(* add an external colorbar *)
		cbarAnimation = addColorBar[
			rawAnimation,
			OptionValue[ColorScheme],
			OptionValue[ShowColorBar],
			Evaluate @ extractOptionsFromDefaults[options, {PlotWavefunction, plot1DWavefunction}, BarLegend]
		];
		
		cbarAnimation
	]





(* PUBLIC FUNCTION DEFINITIONS *)

Options[PlotEvolution] = {};

PlotEvolution[
	wavef_,     (* time-dependent, symbolic, one-dimensional*)
	xDomain:{x_Symbol, _?realNumQ, _?realNumQ},
	tDomain:{t_Symbol, _?realNumQ, _?realNumQ},
	options:OptionsPattern[{PlotEvolution, Animate, plotOptionFunctions1D} // Flatten]
] := 
	plot1DEvolution[
		convertToFunction[wavef, xDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain],   (* throws an alert on bad options (once) *)
		xDomain,
		tDomain,
		
		(* apply over-riding user-given plot-options *)
		Evaluate @ extractOptions[options, {Animate, plotOptionFunctions1D}]
	]

	

