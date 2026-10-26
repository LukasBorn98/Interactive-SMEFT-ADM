#!/usr/bin/env wolframscript
(* Acceptance tests for the SMEFT ADM Explorer (roadmap Phase 7, the parts that can run headlessly).
   Run from the repository root:   wolframscript -file tests/run_tests.wls
   Needs ADMData.wxf (run BuildADMData first). Exit code 1 if any check fails. *)

root = DirectoryName[$InputFileName, 2];
If[root === "", root = Directory[]];
Get[FileNameJoin[{root, "SMEFTADMExplorer.m"}]];
rawFile = FileNameJoin[{root, "Input files", "SMEFT_beta_functions.m"}];
dataFile = FileNameJoin[{root, "ADMData.wxf"}];
tmp = CreateDirectory[];

failures = 0;
check[name_String, ok_, detail_ : ""] := (
  Print[If[TrueQ[ok], "[ OK ] ", "[FAIL] "], name, If[detail =!= "", "   " <> ToString[detail], ""]];
  If[! TrueQ[ok], failures++]);
P[name_String] := Symbol["SMEFTADMExplorer`Private`" <> name];

(* ---------------------------------------------------------------- loading *)
tLoad = First[AbsoluteTiming[d = SMEFTADMExplorer`LoadADMData[dataFile]]];
check["cache loads, < 2 s", AssociationQ[d] && tLoad < 2., Round[tLoad, 0.01]];
ops = d["Operators"];
check["59 operators", Length[ops] === 59, Length[ops]];
check["exactness check of the build passed", d["Checks", "ExactnessOK"]];
check["129 non-linear (Weinberg^2) terms excluded, none without WC", d["Excluded", "NonLinearTerms"] === 129 && d["Excluded", "PureSMTerms"] === 0];

(* ---------------------------------------------------------------- independent reading of the raw file *)
raw = Block[{$Context = "TestRaw`", $ContextPath = {"System`"}}, Get[rawFile]];
nm[s_] := SymbolName[s];
rawEdges = <|1 -> {}, 2 -> {}|>;
Do[
 With[{k = First[Select[Keys[raw], Head[#] =!= Power && SymbolName[Head[#]] === "Coupling" && nm[#[[1]]] === ci &]]},
  Do[
   Module[{wcs = Union[Cases[t, c_ /; MatchQ[Head[c], _Symbol] && SymbolName[Head[c]] === "Coupling" && MemberQ[ops, nm[c[[1]]]] :> nm[c[[1]]], {0, Infinity}]], L = Exponent[t, Symbol["TestRaw`hbar"]]},
    If[Length[wcs] === 1 && wcs[[1]] =!= ci && MemberQ[{1, 2}, L], AppendTo[rawEdges[L], {wcs[[1]], ci}]]],
   {t, If[Head[raw[k]] === Plus, List @@ raw[k], {raw[k]}]}]],
 {ci, ops}];
rawEdges = Map[Union, rawEdges];
check["edge sets equal an independent read of the raw file (1L)", rawEdges[1] === Sort[Keys[Select[d["Edges"], #["L1"] &]]], {Length[rawEdges[1]]}];
check["edge sets equal an independent read of the raw file (2L)", rawEdges[2] === Sort[Keys[Select[d["Edges"], #["L2"] &]]], {Length[rawEdges[2]]}];

(* ---------------------------------------------------------------- graph logic vs raw data, all operators *)
out1 = GroupBy[rawEdges[1], First -> Last]; in1 = GroupBy[rawEdges[1], Last -> First];
out12 = GroupBy[Union[rawEdges[1], rawEdges[2]], First -> Last]; in12 = GroupBy[Union[rawEdges[1], rawEdges[2]], Last -> First];
nbr[a_Association, op_] := Sort[Lookup[a, op, {}]];
graphOK = True; modeColorOK = True; oneLoopGreen = 0; newOK = True; nCfg = 0;
Do[
 Do[
  Module[{g1, g12, g2, expected1, expected12, A, second, exp2edges, newExp},
   g1 = ExplorerGraphData[d, op, dir, "OneLoop"];
   g12 = ExplorerGraphData[d, op, dir, "Direct"];
   g2 = ExplorerGraphData[d, op, dir, "OneLoopSquared"];
   nCfg += 3;
   expected1 = If[dir === "Outgoing", {op, #} & /@ nbr[out1, op], {#, op} & /@ nbr[in1, op]];
   expected12 = If[dir === "Outgoing", {op, #} & /@ nbr[out12, op], {#, op} & /@ nbr[in12, op]];
   graphOK = graphOK && Sort[{#["From"], #["To"]} & /@ g1["Edges"]] === Sort[expected1] && Sort[{#["From"], #["To"]} & /@ g12["Edges"]] === Sort[expected12];
   oneLoopGreen += Count[g1["Edges"], e_ /; e["Color"] === "Green"];
   modeColorOK = modeColorOK && AllTrue[g12["Edges"], (#["Color"] === "Red") === #["L1"] && (#["Color"] === "Green") === (! #["L1"] && #["L2"]) &];
   (* one-loop squared: two hops of one-loop edges; the trivial chain op -> X -> op is dropped *)
   A = If[dir === "Outgoing", nbr[out1, op], nbr[in1, op]];
   second = Flatten[Table[If[dir === "Outgoing", {a, #} & /@ nbr[out1, a], {#, a} & /@ nbr[in1, a]], {a, A}], 1];
   second = DeleteCases[second, e_ /; If[dir === "Outgoing", e[[2]], e[[1]]] === op];
   exp2edges = Union[expected1, second];
   graphOK = graphOK && Sort[{#["From"], #["To"]} & /@ g2["Edges"]] === Sort[exp2edges];
   newExp = Complement[Union[If[dir === "Outgoing", #[[2]] & /@ second, #[[1]] & /@ second]], A, {op}];
   newOK = newOK && Sort[g2["NewEndpoints"]] === Sort[newExp] && Intersection[g2["NewEndpoints"], A] === {}],
  {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["edge lists = direct queries on the raw data (3 modes x 2 directions x 59 operators = " <> ToString[nCfg] <> " graphs)", graphOK];
check["colouring agrees with the stored L1/L2 flags (red = one-loop, green = two-loop only)", modeColorOK];
check["1-loop mode never shows a green edge", oneLoopGreen === 0];
check["highlighted chain endpoints are absent among the direct one-loop neighbours", newOK];

(* chain mode is a tree: every second-leg operator is a separate copy attached to exactly one intermediate operator *)
treeOK = True; nLeaves = 0; maxLeaves = 0;
Do[
 Do[
  Module[{g = ExplorerGraphData[d, op, dir, "OneLoopSquared"], h2, leafEnd, leafIDs, firstIDs},
   h2 = Select[g["Edges"], #["Hop"] === 2 &];
   leafEnd = If[dir === "Outgoing", #["ToID"], #["FromID"]] & /@ h2;
   leafIDs = Select[g["Vertices"], g["Depth"][#] === 2 &];
   firstIDs = Select[g["Vertices"], g["Depth"][#] === 1 &];
   nLeaves += Length[leafIDs]; maxLeaves = Max[maxLeaves, Length[leafIDs]];
   treeOK = treeOK && g["Tree"] === True
     && Length[g["Edges"]] === Length[g["Vertices"]] - 1                      (* connected acyclic graph: a tree *)
     && Sort[leafEnd] === Sort[leafIDs]                                       (* one edge per copy, no shared copies *)
     && AllTrue[h2, g["Parent"][If[dir === "Outgoing", #["ToID"], #["FromID"]]] === If[dir === "Outgoing", #["From"], #["To"]] &]
     && AllTrue[leafIDs, g["NodeOp"][#] === First[StringSplit[#, "@"]] &]
     && Length[Select[g["Edges"], #["Hop"] === 1 &]] === Length[firstIDs]],
  {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["chain graphs are trees with one copy per (intermediate, endpoint) pair (max " <> ToString[maxLeaves] <> " copies)", treeOK];

(* chain mode: open branches. Nothing open = first ring only; a branch shows exactly the second leg of its intermediate operator *)
branchOK = True; nBranchCfg = 0;
SeedRandom[11];
Do[
 Do[
  Module[{full = ExplorerGraphData[d, op, dir, "OneLoopSquared"], none, ring1, some, one, nearOp},
   ring1 = full["FirstRing"];
   none = ExplorerGraphData[d, op, dir, "OneLoopSquared", "OpenBranches" -> {}];
   some = If[ring1 === {}, {}, RandomSample[ring1, Min[2, Length[ring1]]]];
   one = ExplorerGraphData[d, op, dir, "OneLoopSquared", "OpenBranches" -> some];
   nearOp[e_] := If[dir === "Outgoing", e["From"], e["To"]];
   nBranchCfg += 2;
   branchOK = branchOK
     && Length[none["Edges"]] === Length[ring1] && Length[none["Vertices"]] === 1 + Length[ring1] && AllTrue[none["Edges"], #["Hop"] === 1 &]
     && none["NewEndpoints"] === full["NewEndpoints"] && none["BranchCount"] === full["BranchCount"]
     && Sort[Select[one["Edges"], #["Hop"] === 2 &]] === Sort[Select[full["Edges"], #["Hop"] === 2 && MemberQ[some, nearOp[#]] &]]
     && Total[Lookup[full["BranchCount"], ring1, 0]] === Count[full["Edges"], e_ /; e["Hop"] === 2]
     && Total[Lookup[full["BranchCount"], some, 0]] === Count[one["Edges"], e_ /; e["Hop"] === 2]],
  {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["open branches: first ring only / chosen branches / all (" <> ToString[nBranchCfg] <> " graphs)", branchOK];
(* click state machine of the first-ring chips *)
Module[{tb = SMEFTADMExplorer`Private`toggleBranch, st, hid = {}},
 st = tb[d, "cHWB", "Outgoing", hid, False, None, {}, "cHW"];
 check["click on a first-ring chip opens its branch", st === {False, {"cHWB", "Outgoing"}, {"cHW"}}, st];
 st = tb[d, "cHWB", "Outgoing", hid, st[[1]], st[[2]], st[[3]], "cHB"];
 st = tb[d, "cHWB", "Outgoing", hid, st[[1]], st[[2]], st[[3]], "cHW"];
 check["a second click closes it again; other branches stay open", st[[3]] === {"cHB"}, st];
 st = tb[d, "cHWB", "Incoming", hid, False, {"cHWB", "Outgoing"}, {"cHB"}, "cHW"];
 check["open branches are forgotten when the direction changes", st[[3]] === {"cHW"}, st];
 st = tb[d, "cHWB", "Outgoing", hid, True, None, {}, "cHW"];
 check["from 'both rings', a click closes just that branch", Length[st[[3]]] === Length[ExplorerGraphData[d, "cHWB", "Outgoing", "OneLoop"]["Vertices"]] - 2 && ! MemberQ[st[[3]], "cHW"], Length[st[[3]]]]];

(* ---------------------------------------------------------------- layouts: no collisions, no overlaps, timing *)
overlaps[cl_] := Module[{pos = cl["Layout"]["Pos"], sz = cl["Routing"]["Sizes"], o = Keys[cl["Layout"]["Pos"]], c = 0},
  Do[If[i < j, With[{a = pos[o[[i]]], b = pos[o[[j]]], sa = sz[o[[i]]], sb = sz[o[[j]]]},
     If[Abs[a[[1]] - b[[1]]] < (sa[[1]] + sb[[1]])/2 + 2 && Abs[a[[2]] - b[[2]]] < (sa[[2]] + sb[[2]])/2 + 2, c++]]], {i, Length[o]}, {j, Length[o]}]; c];
viol = 0; ovl = 0; times = {}; renderMsgs = 0; nRender = 0;
Do[
 Do[
  Do[
   Module[{g = ExplorerGraphData[d, op, dir, mode, "ShowSelfRunning" -> True], cl, t, r},
    t = First[AbsoluteTiming[cl = P["computeLayout"][d, g, "Radial"]]];
    AppendTo[times, {mode, t}];
    viol += cl["Routing"]["Violations"]; ovl += overlaps[cl];
    r = Quiet[Check[P["renderGraphics"][d, g, "Radial", "ShowSelf" -> True], $Failed, All::stop], All::stop];
    nRender++; If[r === $Failed || Head[r] =!= Association, renderMsgs++]],
   {mode, {"OneLoop", "Direct", "OneLoopSquared"}}], {dir, {"Outgoing", "Incoming"}}], {op, ops}];
(* chain views with nothing open and with a few open branches *)
Do[
 Do[
  Module[{ring1 = ExplorerGraphData[d, op, dir, "OneLoopSquared"]["FirstRing"], g, cl, r},
   Do[
    g = ExplorerGraphData[d, op, dir, "OneLoopSquared", "ShowSelfRunning" -> True, "OpenBranches" -> If[open === "few", Take[ring1, UpTo[3]], {}]];
    cl = P["computeLayout"][d, g, "Radial"];
    viol += cl["Routing"]["Violations"]; ovl += overlaps[cl];
    r = Quiet[Check[P["renderGraphics"][d, g, "Radial", "ShowSelf" -> True], $Failed, All::stop], All::stop];
    nRender++; If[r === $Failed || Head[r] =!= Association, renderMsgs++],
    {open, {"none", "few"}}]],
  {dir, {"Outgoing", "Incoming"}}], {op, ops}];
check["every operator renders in all modes/directions without messages (" <> ToString[nRender] <> " views)", renderMsgs === 0, renderMsgs];
check["no arrow touches a node it does not start or end at (radial layout)", viol === 0, viol];
check["no two nodes overlap (radial layout)", ovl === 0, ovl];
Print["       layout time per view [s]: max one-loop ", Round[Max[Select[times, #[[1]] === "OneLoop" &][[All, 2]]], 0.01],
  ", max direct ", Round[Max[Select[times, #[[1]] === "Direct" &][[All, 2]]], 0.01],
  ", max chains ", Round[Max[Select[times, #[[1]] === "OneLoopSquared" &][[All, 2]]], 0.01],
  ", median chains ", Round[Median[Select[times, #[[1]] === "OneLoopSquared" &][[All, 2]]], 0.01]];
check["switching is fast: single-hop views < 0.5 s", Max[Select[times, #[[1]] =!= "OneLoopSquared" &][[All, 2]]] < 0.5];
(* spring layout on a few of the biggest graphs *)
springOK = True;
Do[Module[{g = ExplorerGraphData[d, op, dir, mode], cl}, cl = P["computeLayout"][d, g, "Spring"]; springOK = springOK && overlaps[cl] === 0],
 {op, {"cuH", "cdB", "cH"}}, {dir, {"Outgoing", "Incoming"}}, {mode, {"Direct", "OneLoopSquared"}}];
check["spring layout: no node overlaps on the largest graphs", springOK];
(* layouts are deterministic *)
g = ExplorerGraphData[d, "cHq1", "Outgoing", "Direct"];
a1 = P["springLayout"][g, P["getPrep"][d]]; a2 = P["springLayout"][g, P["getPrep"][d]];
check["layouts are deterministic", a1 === a2];

(* ---------------------------------------------------------------- spot checks of 5 edges against the raw beta functions *)
spot = {{"cHWB", "cHW", 1}, {"cHq2", "cHq1", 1}, {"cuW", "cuB", 1}, {"cdH", "cHD1", 2}, {"cquqd2", "cqq2", 2}};
spotOK = True;
Do[
 Module[{k = First[Select[Keys[raw], Head[#] =!= Power && SymbolName[Head[#]] === "Coupling" && nm[#[[1]]] === s[[2]] &]], terms, cnt},
  terms = If[Head[raw[k]] === Plus, List @@ raw[k], {raw[k]}];
  cnt = Count[terms, t_ /; Exponent[t, Symbol["TestRaw`hbar"]] === s[[3]] && ! FreeQ[t, c_ /; MatchQ[Head[c], _Symbol] && SymbolName[Head[c]] === "Coupling" && nm[c[[1]]] === s[[1]]]];
  Print["       raw beta of ", s[[2]], " has ", cnt, " loop-", s[[3]], " terms containing ", s[[1]], "; cache: ", d["Edges", {s[[1]], s[[2]]}, "NTerms" <> ToString[s[[3]]]]];
  spotOK = spotOK && cnt === d["Edges", {s[[1]], s[[2]]}, "NTerms" <> ToString[s[[3]]]]],
 {s, spot}];
check["5 spot-checked edges agree with the raw beta functions (term counts)", spotOK];

(* ---------------------------------------------------------------- notation: LaTeX compiles *)
allEdges = Keys[d["Edges"]];
SeedRandom[7];
sample = Join[RandomSample[allEdges, 60], {{"cquqd2", "cqq2"}, {"cuH", "cuH"}}];
texs = Flatten[Table[If[e[[1]] === e[[2]], {d["SelfRunning", e[[1]], "TeX1"], d["SelfRunning", e[[1]], "TeX2"]}, {d["Edges", e, "TeX1"], d["Edges", e, "TeX2"]}], {e, sample}]];
texs = Select[texs, StringQ];
doc = "\\documentclass[10pt]{article}\n\\usepackage{amsmath,geometry}\n\\geometry{a4paper,landscape,margin=0.5cm}\n\\begin{document}\n" <>
   StringRiffle["\\[ " <> # <> " \\]" & /@ texs, "\n"] <> "\n\\end{document}\n";
Export[FileNameJoin[{tmp, "t.tex"}], doc, "Text"];
If[Run["which pdflatex > /dev/null 2>&1"] === 0,
 Module[{rc = Run["cd " <> tmp <> " && pdflatex -interaction=nonstopmode -halt-on-error t.tex > t.log 2>&1"]},
  check["LaTeX of " <> ToString[Length[texs]] <> " sampled entries compiles with pdflatex", rc === 0 && FileExistsQ[FileNameJoin[{tmp, "t.pdf"}]]]],
 Print["[SKIP] pdflatex not available"]];
(* the TeX is also reproducible from Matchete-notation input *)
inp = d["Edges", {"cHWB", "cHW"}, "Input1"];
parsed = Block[{$Context = "SMEFTADMRaw`", $ContextPath = {"System`", "SMEFTADMRaw`"}}, ToExpression[inp, InputForm, HoldComplete]];
check["InputForm strings parse back to expressions (and give the same TeX)",
 ToTeXString[ReleaseHold[parsed]] === d["Edges", {"cHWB", "cHW"}, "TeX1"]];

(* ---------------------------------------------------------------- largest entry does not freeze the panel *)
big = Last[SortBy[Normal[d["Edges"]], #[[2, "NTerms2"]] &]];
g = ExplorerGraphData[d, big[[1, 2]], "Incoming", "Direct"];
tPanel = First[AbsoluteTiming[P["entryPanel"][d, g, {big[[1, 1]], big[[1, 2]], 1}, True]]];
check["largest entry (" <> ToString[big[[2, "NTerms2"]]] <> " terms, full expression) builds in < 1 s", tPanel < 1., Round[tPanel, 0.01]];

(* ---------------------------------------------------------------- grouped entries *)
(* The grouped (collected) display re-expands to the original entry: the sign pulled out of an all-negative group and the signs
   between groups must reproduce every term's sign. Checked on a pure sign/term count level and on the bracket structure. *)
Module[{ed = d["Edges", {"cHW", "cH"}], boxes, groups, negGroups, flat},
 boxes = P["collectedFlowBoxes"][ed["Terms2"], 2];
 groups = GatherBy[P["sortedTerms"][ed["Terms2"]], Cases[#["Factors"], f_ /; MemberQ[{"Gauge", "Quartic"}, f["Type"]], {1}] &];
 negGroups = Count[groups, g_ /; AllTrue[g, P["coeffParts"][#["Coeff"]][[1]] < 0 &]];
 check["grouped entry: one outer bracket after the loop factor, square bracket per group (" <> ToString[Length[groups]] <> " groups)",
  Length[boxes] === Length[groups] && Count[boxes, "[", Infinity] === Length[groups] && Count[boxes, "]", Infinity] === Length[groups]
   && Count[boxes, "(", Infinity] === Count[boxes, ")", Infinity] && Count[boxes[[-1]], ")", Infinity] >= 1, {Length[boxes], Count[boxes, "[", Infinity]}];
 check["grouped entry: minus signs of all-negative groups are pulled out front (" <> ToString[negGroups] <> " groups)",
  Count[boxes, "-", Infinity] >= negGroups, Count[boxes, "-", Infinity]]];

(* ---------------------------------------------------------------- lazy tooltips *)
(* Tooltips are built on hover (Dynamic labels): they must evaluate to a tooltip for every edge and node of the largest chain view *)
Module[{r, labs, vals, size},
 SMEFTADMExplorer`Private`$explorerData = d;
 r = P["renderGraphics"][d, ExplorerGraphData[d, "cuH", "Incoming", "OneLoopSquared"], "Radial"];
 labs = Cases[r["Primitives"], Tooltip[_, lab_Dynamic] :> lab, Infinity];
 vals = First /@ labs;
 size = LeafCount[r["Primitives"]];
 check["lazy tooltips: " <> ToString[Length[labs]] <> " Dynamic labels (703 expected for the largest chain view), all evaluate to a tooltip",
  Length[labs] === 351 + 352 && AllTrue[vals, MatchQ[#, _Framed] &], Length[labs]];
 check["largest chain view: graphics expression is small (" <> ToString[size] <> " leaves < 120000)", size < 120000]];

(* ---------------------------------------------------------------- no stray Delimiter symbols in the UI *)
(* Delimiter is a menu symbol; inside a Column/Grid it would be printed as the word "Delimiter" *)
Module[{ui = SMEFTADMExplorer`SMEFTADMExplorer[d, "cH"], panel},
 panel = P["entryPanel"][d, ExplorerGraphData[d, "cHWB", "Incoming", "Direct"], {"cHW", "cHWB", 1}, False];
 check["the UI contains no Delimiter symbol", FreeQ[ui, Delimiter] && FreeQ[panel, Delimiter]]];

(* ---------------------------------------------------------------- two-loop entries that are not cross-checked (Table 3) *)
(* The rules are transcribed from Table 3 / Sec. 3.2 of the paper (see section 7b of the package). *)
Module[{rules = P["$unverifiedRules"], cnt, flaggedEdges, allowedSrc, g, r, panelFlag, panelPlain, tipFlag, tipPlain, strs, rOne, rChain, rOff, selfFlag, nodeInfo, ov},
 SMEFTADMExplorer`Private`$explorerData = d;
 check["Table 3 rules: all operator IDs exist", AllTrue[Flatten[Join[#["Sources"], #["Targets"]] & /@ rules], MemberQ[ops, #] &]];
 check["Table 3 rules: dipoles = the 8 psi2XH operators, CP-odd X2H2 = H G~, H W~, H B~, H W~B, CP-odd X3 = G~, W~",
  Sort[P["$dipoleOps"]] === Sort[{"ceW", "ceB", "cuG", "cuW", "cuB", "cdG", "cdW", "cdB"}] && Sort[P["$cpOddX2H2Ops"]] === Sort[{"cHGt", "cHWt", "cHBt", "cHWtB"}] && Sort[P["$cpOddX3Ops"]] === Sort[{"cGt", "cWt"}]];
 cnt = Association[#["ID"] -> P["unverifiedCounts"][d, #] & /@ rules];
 check["unverified entries in the data (arrows, self-running): 6a 48+8, 6b-j 30, 6k 5, 7 48+8, 8e 12; Figs. 3b and 5 have no entry",
  cnt === <|"Fig5" -> {0, 0}, "Fig6a" -> {48, 8}, "Fig6b-j" -> {30, 0}, "Fig6k" -> {5, 0}, "Fig3b" -> {0, 0}, "Fig7" -> {48, 8}, "Fig8e" -> {12, 0}|>, cnt];
 flaggedEdges = Select[Keys[d["Edges"]], P["unverifiedRulesFor"] @@ # =!= {} &];
 allowedSrc = Union[P["$dipoleOps"], P["$cpOddX2H2Ops"]];
 check["flagged arrows: " <> ToString[Length[flaggedEdges]] <> " (dipoles -> dipoles/CP-odd X2H2/CP-odd X3, CP-odd X2H2 -> psi2H3); each has a two-loop part",
  Length[flaggedEdges] === 48 + 30 + 5 + 12 && SubsetQ[allowedSrc, flaggedEdges[[All, 1]]] && AllTrue[flaggedEdges, d["Edges", #, "L2"] &]];
 check["unflagged examples: Hq(x) -> Hq(||), C_H -> C_HD, the CP-even X2H2 -> psi2H3, dipole -> CP-even X2H2",
  AllTrue[{{"cHq2", "cHq1"}, {"cH", "cHD1"}, {"cHG", "cuH"}, {"cHWB", "ceH"}, {"cuG", "cHG"}, {"cuG", "cHW"}}, P["unverifiedRulesFor"] @@ # === {} &]];
 (* graph: marked arrows only in the Direct view *)
 g = ExplorerGraphData[d, "cuG", "Outgoing", "Direct", "ShowSelfRunning" -> True];
 r = P["renderGraphics"][d, g, "Radial", "ShowSelf" -> True];
 rOne = P["renderGraphics"][d, ExplorerGraphData[d, "cuG", "Outgoing", "OneLoop"], "Radial"];
 rChain = P["renderGraphics"][d, ExplorerGraphData[d, "cuG", "Outgoing", "OneLoopSquared"], "Radial"];
 rOff = P["renderGraphics"][d, g, "Radial", "ShowUnverified" -> False];
 check["Direct view of C_uG: " <> ToString[r["NUnverified"]] <> " marked arrows = flagged edges of the view; none in the 1-loop and chain views or when switched off",
  r["NUnverified"] === Count[g["Edges"], e_ /; P["unverifiedRulesFor"][e["From"], e["To"]] =!= {}] && r["NUnverified"] > 0 && rOne["NUnverified"] === 0 && rChain["NUnverified"] === 0 && rOff["NUnverified"] === 0, r["NUnverified"]];
 check["Direct view of C_uG: marked arrows carry an amber band and a warning triangle with a click handler each",
  Count[r["Primitives"], c_RGBColor /; c === P["$warnFill"], Infinity] >= r["NUnverified"] && Length[Cases[r["Primitives"], EventHandler[Tooltip[Inset[_Graphics, ___], _], _], Infinity]] === r["NUnverified"]];
 (* side panel *)
 strs[x_] := Cases[x, _String, Infinity];
 panelFlag = P["entryPanel"][d, g, {"cuG", "cHGt", 1}, False];
 panelPlain = P["entryPanel"][d, ExplorerGraphData[d, "cHq1", "Incoming", "Direct"], {"cHq2", "cHq1", 1}, False];
 check["entry panel of C_uG -> C_H G~: banner with the diagram class (Figs. 6b-6j) above the two-loop entry",
  AnyTrue[strs[panelFlag], StringContainsQ[#, "NOT CROSS-CHECKED"] &] && AnyTrue[strs[panelFlag], StringContainsQ[#, "Figs. 6b-6j"] &]];
 check["entry panel of C_Hq(x) -> C_Hq(||): no banner",
  NoneTrue[strs[panelPlain], StringContainsQ[#, "CROSS-CHECKED"] || StringContainsQ[#, "cross-checked"] &]];
 check["entry panel of a dipole -> dipole arrow names both diagram classes (Fig. 6a and Fig. 7)",
  Module[{s = strs[P["entryPanel"][d, ExplorerGraphData[d, "cuG", "Outgoing", "Direct"], {"cuG", "cuW", 1}, False]]},
   AnyTrue[s, StringContainsQ[#, "Fig. 6a"] &] && AnyTrue[s, StringContainsQ[#, "Fig. 7"] &]]];
 check["entry panel of CP-odd X2H2 -> psi2H3 (Fig. 8e) is flagged",
  AnyTrue[strs[P["entryPanel"][d, ExplorerGraphData[d, "cHGt", "Outgoing", "Direct"], {"cHGt", "cuH", 1}, False]], StringContainsQ[#, "Fig. 8e"] &]];
 (* tooltips *)
 tipFlag = P["edgeTooltip"][d, <|"From" -> "cuG", "To" -> "cHGt", "Hop" -> 1|>];
 tipPlain = P["edgeTooltip"][d, <|"From" -> "cHq2", "To" -> "cHq1", "Hop" -> 1|>];
 check["edge tooltips: warning only on flagged arrows",
  AnyTrue[strs[tipFlag], StringContainsQ[#, "not cross-checked"] &] && NoneTrue[strs[tipPlain], StringContainsQ[#, "not cross-checked"] &]];
 nodeInfo = P["nodeTooltipInfo"][g, "cuG"];
 check["center tooltip of a dipole flags the self-running; the one of C_H does not",
  nodeInfo["SelfUnverified"] === True && P["nodeTooltipInfo"][ExplorerGraphData[d, "cH", "Incoming", "Direct"], "cH"]["SelfUnverified"] === False
   && AnyTrue[strs[P["nodeTooltipFromInfo"][d, nodeInfo]], StringContainsQ[#, "self-running is not cross-checked"] &]];
 selfFlag = Cases[r["Primitives"], s_String /; StringContainsQ[s, "unchecked"], Infinity];
 check["self-running badge of a dipole centre carries the warning", selfFlag =!= {}];
 ov = P["unverifiedOverview"][d];
 check["overview of Table 3 lists all " <> ToString[Length[rules]] <> " rules", Count[ov, s_String /; StringStartsQ[s, "Fig"], Infinity] >= Length[rules]]];

(* ---------------------------------------------------------------- explicit flavour (FlavorADMData.wxf) *)
flavorFile = FileNameJoin[{root, "FlavorADMData.wxf"}];
admDir = FileNameJoin[{root, "Input files", "ADM"}];
fd = SMEFTADMExplorer`LoadFlavorADMData[flavorFile];
check["flavour data file loads", AssociationQ[fd]];
If[AssociationQ[fd],
 SMEFTADMExplorer`Private`$explorerData = d; SMEFTADMExplorer`Private`$flavorData = fd;
 Module[{rowsOf = fd["RowsOf"], colsOf = fd["ColsOf"], combos = P["$flavorCombos"], fam, famOK, overl = 0, violF = 0, msgsF = 0, nF = 0, clickF},
  check["flavour: all 59 operators have components (rows " <> ToString[Length[fd["Rows"]]] <> ", columns " <> ToString[Length[fd["Cols"]]] <> ")",
   Sort[Keys[Select[rowsOf, # =!= {} &]]] === Sort[ops] && Sort[Keys[Select[colsOf, # =!= {} &]]] === Sort[ops]];
  check["flavour: components per operator: 1 (no index), 9 or 6 (2 indices), 45/81/... (4 indices); C* columns only for complex components",
   AllTrue[ops, MemberQ[{1, 3, 6, 9, 21, 27, 36, 45, 57, 81, 126, 162}, Length[rowsOf[#]]] &] &&
    AllTrue[fd["Cols"], ! #["Conj"] || ! FreeQ[fd["Rows"][[All, {"Op", "Idx"}]], <|"Op" -> #["Op"], "Idx" -> #["Idx"]|>] &]];
  check["flavour: four approximations x two loop orders stored", AllTrue[combos, AssociationQ[fd["Matrices", #]] && Head[fd["Matrices", #, 1]] === SparseArray && Head[fd["Matrices", #, 2]] === SparseArray &]];
  check["flavour: exact top+gauge entries reproduce the stored top+gauge numbers (build check " <> ToString[fd["Meta", "TopGaugeCheck"]] <> ")", fd["Meta", "TopGaugeCheck"] < 10.^-10];
  (* the same check here, independently, for 300 random entries *)
  Module[{ok = True, inp = fd["Meta", "Inputs"]},
   Do[With[{ks = RandomSample[Keys[fd["TopGaugeExact"][h]], UpTo[150]]},
     ok = ok && AllTrue[ks, With[{ex = P["flavorExactValue"][fd["TopGaugeExact"][h][#], inp] N[1/(16 Pi^2)^h], num = fd["Matrices", "TopGauge", h][[Sequence @@ #]]},
          Abs[ex - num] <= 10.^-10 Abs[num]] &]], {h, 1, 2}];
   check["flavour: exact top+gauge polynomials evaluated at M_t = stored numbers (300 random entries)", ok]];
  (* every flavoured block belongs to an edge of the family graph (at the reference point; the cut-off can only remove elements) *)
  fam = Union[Flatten[Table[With[{m = fd["Matrices", c, h]}, {fd["Cols"][[#[[2]], "Op"]], fd["Rows"][[#[[1]], "Op"]], h} & /@ m["NonzeroPositions"]], {c, combos}, {h, 1, 2}], 2]];
  famOK = AllTrue[fam, If[#[[1]] === #[[2]], d["SelfRunning", #[[1]], "L" <> ToString[#[[3]]]], KeyExistsQ[d["Edges"], #[[1 ;; 2]]] && d["Edges", #[[1 ;; 2]], "L" <> ToString[#[[3]]]]] &];
  check["flavour: every nonzero flavoured block (all approximations) is an edge (or self-running) of the generic ADM at the same loop order", famOK,
   Select[fam, ! If[#[[1]] === #[[2]], d["SelfRunning", #[[1]], "L" <> ToString[#[[3]]]], KeyExistsQ[d["Edges"], #[[1 ;; 2]]] && d["Edges", #[[1 ;; 2]], "L" <> ToString[#[[3]]]]] &]];
  (* focus views: every operator, both directions, K = 40 *)
  Do[Module[{g = P["flavorFocusGraph"][fd, op, P["defaultFlavorIdx"][fd, op], dir, "FullUp", "Sum", 40, 10.^-14], cl, r},
     cl = P["computeLayout"][d, g, "Radial"];
     violF += cl["Routing"]["Violations"]; overl += overlaps[cl];
     r = Quiet[Check[P["renderGraphics"][d, g, "Radial", "ShowSelf" -> True], $Failed, All::stop], All::stop];
     nF++; If[r === $Failed || Head[r] =!= Association, msgsF++]], {op, ops}, {dir, {"Outgoing", "Incoming"}}];
  check["flavour focus views render without messages (" <> ToString[nF] <> " views, 40 partners)", msgsF === 0, msgsF];
  check["flavour focus views: no arrow touches a foreign node, no node overlaps", violF === 0 && overl === 0, {violF, overl}];
  (* weighted family graph and panels *)
  Module[{g = ExplorerGraphData[d, "cuH", "Incoming", "Direct"], gw, pm, panel1, panel2, det},
   gw = P["weightFamilyEdges"][g, fd, "FullUp", "Sum", 10.^-8];
   pm = fd["PairMax", "FullUp", "Sum"];
   check["weighted graph: thickness grows with the largest flavoured element, faded below the threshold",
    AllTrue[gw["Edges"], KeyExistsQ[#, "Thick"] &] &&
     With[{es = Select[gw["Edges"], ! #["Faded"] &]}, OrderedQ[SortBy[{#["FlavorMax"][[1]], #["Thick"]} & /@ es, First][[All, 2]]]] &&
     AllTrue[Select[gw["Edges"], #["Faded"] &], #["FlavorMax"][[1]] < 10.^-8 &]];
   check["weighted graph: largest element of a block = maximum over the block", With[{e = {"cHG", "cuH"}},
     pm[e][[1]] == Max[Abs[P["flavorBlockEntries"][fd, "FullUp", "cHG", "cuH"][[All, 5]]]]]];
   panel1 = P["flavorBlockPanel"][fd, "cHG", "cuH", "FullUp", "Sum", "", False, (Null &), {fd["RowsOf"]["cuH"][[-1]], fd["ColsOf"]["cHG"][[1]]}];
   panel2 = P["flavorBlockPanel"][fd, "cqu1", "cuH", "FullUp", "Sum", "33", False, (Null &), None];
   check["flavour block panels build (heat map, ranked table with filter, element detail with exact expression)",
    ! FreeQ[panel1, "Exact, top Yukawa + gauge approximation"] && FreeQ[{panel1, panel2}, $Failed] && FreeQ[{panel1, panel2}, Delimiter]];
   (* regression: the selection handler must survive being inlined into the cell handlers (a Function[{r, c}, ..] had its
      parameters replaced by numbers: "Function::flpar") *)
   Module[{rec = None, ok = True},
    Do[Module[{pan = P["flavorBlockPanel"][fd, pr[[1]], pr[[2]], "FullUp", "Sum", "", False, (rec = {#1, #2}) &, None], rl},
       rl = Cases[pan, HoldPattern[RuleDelayed["MouseClicked", rhs_]] :> Hold[rhs], Infinity];
       rec = None; ok = ok && rl =!= {} && Quiet[Check[ReleaseHold[First[rl]]; True, False]] && MatchQ[rec, {_Integer, _Integer}]],
     {pr, {{"cHG", "cuH"}, {"cqu1", "cuH"}}}];
    check["flavour panels: clicking a heat-map cell / list row selects the element without messages", ok]];
   det = P["flavorElementDetail"][fd, fd["RowsOf"]["cuH"][[-1]], fd["ColsOf"]["cHG"][[1]], "FullUp"];
   check["element detail: exact expression of C_HG -> C_uH^33 at one loop is 32 g_s^4 y_t", 
    P["exactExpr"][fd["TopGaugeExact"][1][{fd["RowsOf"]["cuH"][[-1]], fd["ColsOf"]["cHG"][[1]]}]] === 32 P["flavGS"]^4 P["flavYt"]]];
  (* clicks in the focus view recenter on the flavoured coefficient *)
  clickF = Module[{ui = SMEFTADMExplorer`SMEFTADMExplorer[d, "cuH"], S = Symbol["SMEFTADMExplorer`Private`" <> #] &, set, gr, rules, nodeRule},
    set[n_, v_] := ToExpression["SMEFTADMExplorer`Private`" <> n, InputForm, Function[sym, sym = v, HoldFirst]];
    ui[[1]]; (* initialise the DynamicModule variables *)
    set["mode", "Flavour"]; set["center", "cuH"]; set["direction", "Incoming"]; set["history", {}];
    gr = ReleaseHold[First[Select[Cases[ui, HoldPattern[Dynamic[a_, ___]] :> Hold[a], Infinity], MatchQ[#, Hold[_Module]] &]]];
    rules = Cases[gr, HoldPattern[RuleDelayed["MouseClicked", rhs_]] :> Hold[rhs], Infinity];
    nodeRule = SelectFirst[rules, ! FreeQ[#, s_String /; StringMatchQ[s, "cqu1[" ~~ __]] && FreeQ[#, {_String, _String, _Integer}] &, None];
    If[nodeRule === None, False, ReleaseHold[nodeRule]; S["center"] === "cqu1" && S["history"] === {"cuH"} && ListQ[S["fIdxOf"]["cqu1"]]]];
  check["focus view: clicking a flavoured coefficient recenters on it (operator and flavour component)", clickF];
  (* independent cross-check against the symbolic entries of the source (only if Input files/ADM is present) *)
  If[DirectoryQ[admDir],
   Module[{inp = fd["Meta", "Inputs"], ys, Y, val, ok = True, chainMat, nf = <|1 -> N[1/(16 Pi^2)], 2 -> N[1/(16 Pi^2)^2]|>, raw = "SMEFTFlavorRaw`", m, picks},
    P["loadFlavorADMSource"][admDir];
    ys = P["flavorYukawas"][inp, "FullUp"];
    Y = <|"Yu" -> ys[[1]], "Yd" -> ys[[2]], "Ye" -> ys[[3]]|>;
    chainMat[toks_] := Dot @@ (If[#[[2]], ConjugateTranspose[Y[#[[1]]]], Y[#[[1]]]] & /@ toks);
    val[ex_] := ex /. {h_Symbol[i_Integer, j_Integer] /; Context[h] === raw :> chainMat[Symbol[raw <> "$admChainTokens"][SymbolName[h]]][[i, j]]} /.
       {s_Symbol /; Context[s] === raw :> Switch[SymbolName[s], "g", inp["gL"], "gp", inp["gY"], "gs", inp["gs"], "lambda", inp["lambda"], "m2", 0.,
          _, Tr[chainMat[Symbol[raw <> "$admTraceTokens"][SymbolName[s]]]]]};
    Do[
     m = fd["Matrices", "FullUp", h];
     picks = RandomSample[m["NonzeroPositions"], 6];
     Do[With[{r = fd["Rows"][[pk[[1]]]], c = fd["Cols"][[pk[[2]]]]},
       With[{ex = Block[{$Context = raw, $ContextPath = {"System`"}}, Symbol[raw <> "ADMEntry"][h, r["Name"], c["Name"], c["Conj"]]]},
        ok = ok && Abs[nf[h] val[ex] - m[[Sequence @@ pk]]] <= 10.^-9 Abs[m[[Sequence @@ pk]]]]], {pk, picks}], {h, 1, 2}];
    check["flavour: 12 random stored elements equal the symbolic entries of Input files/ADM (ADMEntry) evaluated at M_t", ok];
    (* at a random point (complex Yukawas, no cut-off) the nonzero flavoured blocks are exactly the edges of the generic ADM *)
    Module[{rnd = (SeedRandom[4711]; RandomComplex[{-1 - I, 1 + I}, {3, 3, 3}]), pt, rowsG, colsG, nm = Symbol[raw <> "$admMeta"]["names"], cj = Symbol[raw <> "$admMeta"]["conj"], blocks, generic, mism = {}},
     pt = Symbol[raw <> "ADMPoint"][0.6, 0.35, 1.1, 0.3, 0., rnd[[1]], rnd[[2]], rnd[[3]]];
     rowsG = Lookup[AssociationThread[{nm, cj}\[Transpose] -> Range[Length[nm]]], Key[{#["Name"], 0}]] & /@ fd["Rows"];
     colsG = Lookup[AssociationThread[{nm, cj}\[Transpose] -> Range[Length[nm]]], Key[{#["Name"], Boole[#["Conj"]]}]] & /@ fd["Cols"];
     Do[
      blocks = Union[{fd["Cols"][[#[[2]], "Op"]], fd["Rows"][[#[[1]], "Op"]]} & /@ Select[Most[ArrayRules[Chop[Symbol[raw <> "ADMMatrix"][h, pt][[rowsG, colsG]], 10.^-12]]], True &][[All, 1]]];
      generic = Union[Keys[Select[d["Edges"], #["L" <> ToString[h]] &]], {#, #} & /@ Select[ops, d["SelfRunning", #, "L" <> ToString[h]] &]];
      If[blocks =!= generic, AppendTo[mism, {h, Complement[blocks, generic], Complement[generic, blocks]}]], {h, 1, 2}];
     check["flavour: at a random point the nonzero flavoured blocks are exactly the generic edges and self-running (both loop orders)", mism === {}, mism]]]]]];

(* ---------------------------------------------------------------- click handlers must be self-contained *)
(* Regression test: handlers that refer to Module-local symbols stop working in the front end (the
   temporary symbols are removed once only the front end holds the graphic). *)
Module[{ui, gr, rules, tmpSyms, ok1, ok2, edgeRule, nodeRule, S},
 ui = SMEFTADMExplorer`SMEFTADMExplorer[d, "cH"];
 ui[[1]];
 gr = ReleaseHold[First[Select[Cases[ui, HoldPattern[Dynamic[a_, ___]] :> Hold[a], Infinity], MatchQ[#, Hold[_Module]] &]]];
 rules = Cases[gr, HoldPattern[RuleDelayed["MouseClicked", rhs_]] :> Hold[rhs], Infinity];
 tmpSyms = Union[Cases[rules, s_Symbol /; StringMatchQ[SymbolName[s], __ ~~ "$" ~~ DigitCharacter ..] && Context[s] === "SMEFTADMExplorer`Private`", Infinity]];
 tmpSyms = Select[tmpSyms, ! MemberQ[{"op$", "key$"}, StringReplace[SymbolName[#], DigitCharacter .. ~~ EndOfString -> ""]] &];
 check["click handlers contain no Module-local temporary symbols (" <> ToString[Length[rules]] <> " handlers)", Length[rules] > 0 && tmpSyms === {}, tmpSyms];
 S[n_] := Symbol["SMEFTADMExplorer`Private`" <> n];
 edgeRule = First[Select[rules, ! FreeQ[#, {_String, _String, _Integer}] &]];
 ReleaseHold[edgeRule];
 check["clicking an arrow selects its edge", MatchQ[S["selected"], {_String, _String, _Integer}], S["selected"]];
 nodeRule = First[Select[rules, FreeQ[#, {_String, _String, _Integer}] && FreeQ[#, "cH"] &]];
 ReleaseHold[nodeRule];
 check["clicking an operator recenters the graph and records history", S["center"] =!= "cH" && S["history"] === {"cH"} && S["selected"] === None, S["center"]]];

(* ---------------------------------------------------------------- packaging *)
dd = FileNameJoin[{tmp, "ADMData.wxf"}]; CopyFile[dataFile, dd];
nbEmb = SMEFTADMExplorer`MakeExplorerNotebook[FileNameJoin[{tmp, "emb.nb"}], "EmbedData" -> True, "DataFile" -> dd];
nbLin = SMEFTADMExplorer`MakeExplorerNotebook[FileNameJoin[{tmp, "lnk.nb"}], "EmbedData" -> False, "DataFile" -> dd];
check["notebooks are generated (embedded " <> ToString[Round[FileByteCount[nbEmb]/10.^6, 0.01]] <> " MB, linked " <> ToString[Round[FileByteCount[nbLin]/10.^3, 1]] <> " kB)", FileExistsQ[nbEmb] && FileExistsQ[nbLin]];
(* run the loader of the embedded notebook in a fresh kernel *)
nbExpr = Import[nbEmb, "NB"];
loadSrc = First[Cases[nbExpr, Cell[s_String, "Input", ___] /; StringContainsQ[s, "Uncompress"] :> s, Infinity]];
Export[FileNameJoin[{tmp, "load.wl"}], loadSrc <> "\nPrint[Length[SMEFTADMExplorer`Private`$notebookData[\"Operators\"]]];", "Text"];
out = RunProcess[{"wolframscript", "-file", FileNameJoin[{tmp, "load.wl"}]}];
check["embedded loader works in a fresh kernel (no package file needed)", StringTrim[out["StandardOutput"]] === "59", StringTrim[out["StandardOutput"]]];

Print["\n", If[failures === 0, "ALL CHECKS PASSED", ToString[failures] <> " CHECK(S) FAILED"]];
DeleteDirectory[tmp, DeleteContents -> True];
Exit[If[failures === 0, 0, 1]];
