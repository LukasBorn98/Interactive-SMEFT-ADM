#!/usr/bin/env wolframscript
(* Rebuilds everything: cached data, the notebook and the PNG snapshots.
   Run from the repository root:   wolframscript -file make_all.wls *)
root = DirectoryName[$InputFileName];
If[root === "", root = Directory[]];
SetDirectory[root];
Get["SMEFTADMExplorer.m"];

data = BuildADMData["Input files/SMEFT_beta_functions.m", "ADMData.wxf"];

(* notebook that loads SMEFTADMExplorer.m and ADMData.wxf from its own folder *)
MakeExplorerNotebook["SMEFT_ADM_Explorer.nb", "EmbedData" -> False, "StartOperator" -> "cH"];

(* snapshots for the README *)
If[! DirectoryQ["snapshots"], CreateDirectory["snapshots"]];
views = {
  {"cH_generated_by_direct", "cH", "Incoming", "Direct"},
  {"cuH_generated_by_direct_56_neighbours", "cuH", "Incoming", "Direct"},
  {"cHWB_runs_into_one_loop_squared", "cHWB", "Outgoing", "OneLoopSquared"}};
UsingFrontEnd[
 Do[
  Module[{g = ExplorerGraphData[data, v[[2]], v[[3]], v[[4]], "ShowSelfRunning" -> True]},
   Export[FileNameJoin["snapshots", v[[1]] <> ".png"],
    SMEFTADMExplorer`Private`explorerGraphics[data, g, "Radial", "ShowSelf" -> True], ImageResolution -> 90]], {v, views}];
 (* side panel with a one-loop and a two-loop entry *)
 Export[FileNameJoin["snapshots", "entry_panel_cHq2_to_cHq1.png"],
  Rasterize[Panel[SMEFTADMExplorer`Private`entryPanel[data, ExplorerGraphData[data, "cHq1", "Incoming", "Direct"], {"cHq2", "cHq1", 1}, False], ImageSize -> 440], ImageResolution -> 100]]];
Print["done"];
