(* ::Package:: *)

BeginPackage["PlotFunctions`"]
EndPackage[]

(* All loading of the paclet's Wolfram Language code should go through this file. *)

(* Developer maintains this list of symbols.
   autoloadSymbols must agree with the symbols listed in the Kernel extension in the PacletInfo.m file.
*)

Map[
    (Unprotect[#];ClearAll[#]) &, {
        "PlotFunctions`*",
        "PlotFunctions`*`*",
        "PlotFunctions`*`*`*"
    }
]

PacletManager`Package`loadWolframLanguageCode[
    "PlotFunctions", 
    "PlotFunctions`", 
    DirectoryName[$InputFileName], 
    "Main.m",
    "AutoUpdate" -> True,
    "ForceMX" -> False, 
    "Lock" -> False,
    "AutoloadSymbols" -> {},
    "HiddenImports" -> {}
]
