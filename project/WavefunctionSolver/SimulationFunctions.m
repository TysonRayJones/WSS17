(* ::Package:: *)

Package["WavefunctionSolver`"]

(*
	TODO:
	- implement AutoNormalise
	- implement Verbose or TimeSteps or ShowProgress or w/e
	- extend time-dependent plotting to lists and matrices and what not
	- function to actually calc evolution of a static wavefunction
	  in 1D and 2D
	- options should offer pre-caching
	
	NOTES:
	- ControlActive upperbound in PlotPoints affects render-time of the lowerbound; WTH? 
	   Occurs for both Animate and Manipuulate
*)





(* SYMBOL EXPORTS *)

PackageExport[NumberOfFrames]





(* FUNCTION EXPORTS *)

PackageExport[PlotEvolution]





(* FUNCTION USAGE MESSAGES *)

PlotEvolution::usage = "PlotEvolution[wavefunction[x, t], {x, xL, xR}, {t, t0, t1}] animates the evolution of a symbolic time-dependent wavefunction";





(* FUNCTION ERROR MESSAGES *)





(* PRIVATE SYMBOL IMPORTS *)

(* patterns for accepting domains *)
realNumQ = WavefunctionSolver`PlotFunctions`PackagePrivate`realNumQ;
domainQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainQ;
domainSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainSymbQ;
domainOptSymbQ = WavefunctionSolver`PlotFunctions`PackagePrivate`domainOptSymbQ;

(* testers for functions *)
is2DFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`is2DFunction;
func1DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func1DQ;
func2DQ = WavefunctionSolver`EigenFunctions`PackagePrivate`func2DQ;

(* for manipulating accepted inputs into functional form *)
convertToFunction = WavefunctionSolver`PlotFunctions`PackagePrivate`convertToFunction;

(* functions for filtering options *)
extractOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptions;
addDefaultOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`addDefaultOptions;
extractOptionsFromDefaults = WavefunctionSolver`PlotFunctions`PackagePrivate`extractOptionsFromDefaults;

(* to ensure we use the same default axes labels as PlotFunctions, when we're forced to pass them *)
defaultProbDensityLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultProbDensityLabel;
defaultXLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultXLabel;
defaultYLabel = WavefunctionSolver`PlotFunctions`PackagePrivate`defaultYLabel;

(* for accessing default plot options *)
plotOptionFunctions1D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions1D;
plotOptionFunctions2D = WavefunctionSolver`PlotFunctions`PackagePrivate`optionFunctions2D;
plot1DWavefunction = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DWavefunction;
plot2DWavefunction = WavefunctionSolver`PlotFunctions`PackagePrivate`plot2DWavefunction;

(* for plotting potentials and colorbars *)
plot1DPotential = WavefunctionSolver`PlotFunctions`PackagePrivate`plot1DPotential;
plot2DPotential = WavefunctionSolver`PlotFunctions`PackagePrivate`plot2DPotential;
addColorBar = WavefunctionSolver`PlotFunctions`PackagePrivate`addColorBar;

(* for validating the Potential in options passed to PlotEvolution *)
isValid1DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid1DPotentialOptions;
isValid2DPotentialOptions = WavefunctionSolver`PlotFunctions`PackagePrivate`isValid2DPotentialOptions;





(* CONSTANTS *)

defaultTimeLabel = "time";





(* PRIVATE FUNCTION DEFINITIONS *)

is3DFunction[function:(_Function|_InterpolatingFunction)] :=
	Quiet @ Not[Check[func[dummyval1, dummyval2, dummyval3], None] === None]



(* 1D *)
convertToCurriedFunction[
	function_?func2DQ,
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ     (* time *)
] :=
	Function @@ {t, Function @@ {x, function[x, t]}}

convertToCurriedFunction[
	symbolic_,
	xDomain:_?domainSymbQ,                    (* space *)
	{tSymb_Symbol, _?realNumQ, _?realNumQ}    (* time *)
] :=
	Function @@ {tSymb, convertToFunction @@ {symbolic, xDomain}}
	
(* 2D *)
convertToCurriedFunction[
	function_?func2DQ,
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ,    (* space *)
	_?domainOptSymbQ     (* time *)
] :=
	Function @@ {t, Function @@ {{x, y}, function[x, y, t]}}

convertToCurriedFunction[
	symbolic_,
	xDomain:_?domainSymbQ,                    (* space *)
	yDomain:_?domainSymbQ,                    (* space *)  
	{tSymb_Symbol, _?realNumQ, _?realNumQ}   (* time *)
] :=
	Function @@ {tSymb, convertToFunction @@ {symbolic, xDomain, yDomain}}



(* 
	Directly animates a held PlotWavefunction combined with a potential plot.
	Can produce a plot at any time-stamp
*)
animateEvolution[
	probPotPlot:_Hold,
	{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:0,
	options:OptionsPattern[Animate]
] :=
	Animate[
		ReleaseHold[probPotPlot], 
		{{tSymb, t0, tLabel}, t0, t1},
		
		(* apply over-riding user-given Animate options *)
		options,
		
		(* otherwise apply this styling *)
		Paneled -> False
	]

(*
	collects a number of frames from a held PlotWavefunction combined with a
	potential plot, and produces a ListAnimation
*)
animateEvolution[
	probPotPlot:_Hold,
	{tSymb_Symbol, tLabel:(_Symbol|_String), t0_?realNumQ, t1_?realNumQ},
	numFrames:_Integer?Positive,
	options:OptionsPattern[ListAnimate]
] :=
	DynamicModule[
		{frames},
		
		frames = Table[
			ReleaseHold[probPotPlot],
			{tSymb, t0, t1, (t1-t0)/(numFrames-1)}   (*** hmm we can't have single frame any more? **)
		];

		ListAnimate[
			frames, 
			
			(* apply over-riding user-given Animate options *)
			options,
		
			(* otherwise apply this styling *)    (* MMA BUG; THIS OVER-RIDES GRRR *)
			Paneled -> False,
			
			(* initialise dynamic view-angle, to enable mid-animation rotation *)
			Initialization :> (		
				viewA = 60 Degree; viewP = {1, 1, 1}; viewV = {0, 0, 2.0};
			)
		]
	]



(*
    combines a held wavefunction plot and a potential plot
    (which may be None) in a Show
*)
combineProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:None
] :=
	probPlot
	
combineProbPotentialPlots[
	probPlot:_Hold,
	potentialPlot:(_Graphics|_Graphics3D)
] :=
	Hold @ Show[
		ReleaseHold @ probPlot,
		potentialPlot
	]



plot1DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 2D: [x][t] (curried) *)
	potential:(_Function|_InterpolatingFunction|None),  (* 1D: [x] *)
	{xLabel:(_Symbol|_String):defaultXLabel,    xL_?realNumQ, xR_?realNumQ},
	{tLabel:(_Symbol|_String):defaultTimeLabel, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions1D, Animate, ListAnimate, PlotEvolution} // Flatten]
] :=
	Module[
		{probabilityPlot, potentialPlot, rawAnimation, cbarAnimation},
		
		(* prepare (held) probability without sources of slow animation (we'll restore externally if desired *)
		probabilityPlot = Hold @ PlotWavefunction[
			 (* evals to a 1D-in-space function *)
			wavef[tDummyVar],  
			{xL, xR},  
			(* kill features which ruin performance (restore externally if user desires) *)
			Potential -> None, ShowColorBar -> False,
			(* pass user-given over-riding options *)
			Evaluate @ extractOptions[options, plotOptionFunctions1D],	
			(* otherwise update the axes labels *)
			AxesLabel -> {xLabel, defaultProbDensityLabel}
		];
		
		(* plot static potential with automatic range (it'll use Show range); may return none *)
		potentialPlot = plot1DPotential[
			potential, {xL, xR}, Automatic, 
			Evaluate @ extractOptions[options, plotOptionFunctions1D]
		];
		
		(* merge probability and potential (into a held Show), then animate *)
		rawAnimation = animateEvolution[
			combineProbPotentialPlots[probabilityPlot, potentialPlot],
			{tDummyVar, tLabel, t0, t1}, 
			
			(* decide whether to directly animate, or cache *)
			OptionValue[NumberOfFrames],
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



plot2DEvolution[
	wavef:(_Function|_InterpolatingFunction),           (* 3D: [x, y][t] (curried) *)
	potential:(_Function|_InterpolatingFunction|None),  (* 2D: [x, y] *)
	{xLabel:(_Symbol|_String):defaultXLabel,    xL_?realNumQ, xR_?realNumQ},
	{yLabel:(_Symbol|_String):defaultYLabel,    yL_?realNumQ, yR_?realNumQ},
	{tLabel:(_Symbol|_String):defaultTimeLabel, t0_?realNumQ, t1_?realNumQ},
	options:OptionsPattern[{plotOptionFunctions2D, Animate, ListAnimate, PlotEvolution} // Flatten]
] :=
	Module[
		{probabilityPlot, potentialPlot, rawAnimation, cbarAnimation},
		
		(* prepare (held) probability without sources of slow animation (we'll restore externally if desired *)
		probabilityPlot = Hold @ PlotWavefunction[
			 (* evals to a 2D-in-space function *)
			wavef[tInternalVar],  
			{xL, xR}, {yL, yR},
			(* kill features which ruin performance (restore externally if user desires) *)
			Potential -> None, 
			ShowColorBar -> False,
			(* pass user-given over-riding options *)
			Evaluate @ extractOptions[options, plotOptionFunctions2D],	
			(* otherwise update the axes labels *)
			AxesLabel -> {xLabel, yLabel, defaultProbDensityLabel},
			(* Give dynamic view params (to enable mid-cached animation rotation *)
			ViewAngle :> Dynamic@viewA, ViewPoint :> Dynamic@viewP, ViewVertical :> Dynamic@viewV
		];
		
		(* plot static potential with automatic range (it'll use Show range); may return none *)
		potentialPlot = plot2DPotential[
			potential, {xL, xR}, {yL, yR}, Automatic, 
			Evaluate @ extractOptions[options, plotOptionFunctions2D]
		];
		
		(* merge probability and potential (into a held Show), then animate *)
		rawAnimation = animateEvolution[
			combineProbPotentialPlots[probabilityPlot, potentialPlot],
			{tInternalVar, tLabel, t0, t1}, 
			
			(* decide whether to directly animate, or cache *)
			OptionValue[NumberOfFrames],
			Evaluate @ extractOptions[options, Animate]
		];
		
		(* add an external colorbar *)
		cbarAnimation = addColorBar[
			rawAnimation,
			OptionValue[ColorScheme],
			OptionValue[ShowColorBar],
			Evaluate @ extractOptionsFromDefaults[options, {PlotWavefunction, plot2DWavefunction}, BarLegend]
		];
		
		cbarAnimation
	]



isValid1DTimeDependentWavef[symb:Except[(_Function|_InterpolatingFunction)], _?domainSymbQ, _?domainSymbQ] := 
	True
	
isValid1DTimeDependentWavef[function:(_Function|_InterpolatingFunction), _?domainOptSymbQ, _?domainOptSymbQ] := 
	is2DFunction[function]
	
isValid1DTimeDependentWavef[_, _?domainOptSymbQ, _?domainOptSymbQ] :=
	False
	
	

isValid2DTimeDependentWavef[symb:Except[(_Function|_InterpolatingFunction)], Repeated[_?domainSymbQ, 3]] := 
	True
	
isValid1DTimeDependentWavef[function:(_Function|_InterpolatingFunction), Repeated[_?domainOptSymbQ, 3]] := 
	is3DFunction[function]
	
isValid2DTimeDependentWavef[_, Repeated[_?domainOptSymbQ, 3]] :=
	False





(* PUBLIC FUNCTION DEFINITIONS *)

(*
	PUT INFO ABOUT PLOTEVOLUTOIN HERE 
*)
Options[PlotEvolution] = {

	NumberOfFrames -> 0,

	AutoNormalize -> False   (* TODO *)
};

(* 1D *)

(*
	passing time-dependent
*)
PlotEvolution[
	wavef_,     (* time-dependent, symbolic, one space dimension *)
	xDomain_?domainOptSymbQ,
	tDomain_?domainOptSymbQ, 
	options:OptionsPattern[{PlotEvolution, Animate, plotOptionFunctions1D} // Flatten]
] /; (
	isValid1DTimeDependentWavef[wavef, xDomain, tDomain] &&
	isValid1DPotentialOptions[options, xDomain]
) := 
	plot1DEvolution[
		convertToCurriedFunction[wavef, xDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain],   (* throws an alert on bad options (once) *)
		xDomain, tDomain,
		Evaluate @ extractOptions[options, {PlotEvolution, Animate, ListAnimate, plotOptionFunctions1D}]
	]

(*
	passing initial-state
*)



(* 2D *)

(*
	passing time-dependent
*)
PlotEvolution[
	wavef_,     (* time-dependent, symbolic, two space dimensions *)
	xDomain_?domainOptSymbQ,
	yDomain_?domainOptSymbQ,
	tDomain_?domainOptSymbQ, 
	options:OptionsPattern[{PlotEvolution, Animate, ListAnimate, plotOptionFunctions2D} // Flatten]
] /; (
	isValid2DTimeDependentWavef[wavef, xDomain, yDomain, tDomain] &&
	isValid2DPotentialOptions[options, xDomain, yDomain]
) := 
	plot2DEvolution[
		convertToCurriedFunction[wavef, xDomain, yDomain, tDomain],
		convertToFunction[OptionValue[Potential], xDomain, yDomain],   (* throws an alert on bad options (once) *)
		xDomain, yDomain, tDomain,
		Evaluate @ extractOptions[options, {PlotEvolution, Animate, plotOptionFunctions2D}]
	]
