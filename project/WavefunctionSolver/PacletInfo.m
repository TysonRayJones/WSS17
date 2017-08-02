(* ::Package:: *)

Paclet[
	Name -> "WavefunctionSolver",
	Version -> "0.0.1",
	MathematicaVersion -> "11+",
	Description -> "Functions for finding stationary, evolving and plotting wavefunctions",
	Creator -> "Tyson Jones <tyson.jones@monash.edu>",
	Loading -> Automatic,
	Extensions -> {
		{
			"Kernel", 
			Context -> {"WavefunctionSolver`"}, 
			Symbols -> {}
		},
		{
			"Resource", 
			Root -> ".", 
			Resources -> {"Assets"}
		}
	}
]
