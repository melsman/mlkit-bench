if ((typeof(Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1)) == "undefined") {Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1 = {};
};
(function(){Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.eq_traversal$753 = function(v$755,v$756){switch (v$755) { case 0: {switch (v$756) { case 0: {return true;
 break; }default: {return false;
} };
 break; }case 1: {switch (v$756) { case 1: {return true;
 break; }default: {return false;
} };
 break; } };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.fromList$58 = function(v$61){if (v$61 == null) {return [[h31NAsryh9ddz8aYJmJgvIb$3basis$0Vector$1$3.fromList$228(null),0,0]];
} else {var v$79 = v$61;
var v$80 = v$79[0];
var v$81 = v$79[1];
var row1$68 = h6vNYOZIUi7ndbcMVSOZBfa$3basis$0Array$1$3.fromList$228(v$80);
var rowr$69 = dQvcrgdPh2Rb0tWb4ecVml$3basis$0List$1.map$330(function(v$1349){return h6vNYOZIUi7ndbcMVSOZBfa$3basis$0Array$1$3.fromList$228(v$1349);
},v$81);
var cols$70 = row1$68.length;
var vec$71;
var t$1352;
var fix$1350 = {};
fix$1350.$all = function(v$762){lab$all: while (true) {if (v$762 == null) {return true;
} else {var v$763 = v$762;
var v$764 = v$763[0];
var v$765 = v$763[1];
if (v$764.length == cols$70) {var t$1351 = v$765;
var v$762 = t$1351;
continue lab$all;
} else {return false;
};
};
};
};
var all$761 = fix$1350.$all;
t$1352 = (all$761(rowr$69));
if (t$1352) {vec$71 = (h31NAsryh9ddz8aYJmJgvIb$3basis$0Vector$1$3.fromList$228([row1$68,rowr$69]));
} else {throw CompilerInitial.exn$Size$51;
};
return [[vec$71,vec$71.length,cols$70]];
};
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.array$82 = function(v$90,v$91,v$92){var t$1360;
var t$1353;
var t$772;
if ((0 <= v$90)?(v$90 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$772 = (Array(v$90));
var fix$1354 = {};
fix$1354.$init = function(v$775,v$776){lab$init: while (true) {if (v$776 >= v$90) {return 0;
} else {var t$1357 = v$775;
var t$1356 = v$776;
var t$1355;
if ((0 <= v$91)?(v$91 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$1355 = (SmlPrims.wordTableInit(v$91,v$92));
(t$1357[t$1356] = t$1355,0);
var t$1358 = v$775;
var t$1359 = SmlPrims.chk_ovf_i32(v$776 + 1);
var v$775 = t$1358;
var v$776 = t$1359;
continue lab$init;
};
};
};
var init$773 = fix$1354.$init;
init$773(t$772,0);
t$1353 = t$772;
t$1360 = [t$1353,v$90,v$91];
return [t$1360];
};
var fix$1361 = {};
fix$1361.$tabulate = function(v$96,v$99){lab$tabulate: while (true) {switch (v$96) { case 0: {var v$118 = v$99[0];
var v$119 = v$99[1];
var v$120 = v$99[2];
var t$1372;
var t$1362;
var t$783;
if ((0 <= v$118)?(v$118 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$783 = (Array(v$118));
var fix$1363 = {};
fix$1363.$init = function(v$786,v$787){lab$init: while (true) {if (v$787 >= v$118) {return 0;
} else {var t$1369 = v$786;
var t$1368 = v$787;
var t$1364;
var t$1119;
if ((0 <= v$119)?(v$119 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$1119 = (Array(v$119));
var fix$1365 = {};
fix$1365.$init = function(v$1122,v$1123){lab$init: while (true) {if (v$1123 >= v$119) {return 0;
} else {(v$1122[v$1123] = (v$120([v$787,v$1123])),0);
var t$1366 = v$1122;
var t$1367 = SmlPrims.chk_ovf_i32(v$1123 + 1);
var v$1122 = t$1366;
var v$1123 = t$1367;
continue lab$init;
};
};
};
var init$1120 = fix$1365.$init;
init$1120(t$1119,0);
t$1364 = t$1119;
(t$1369[t$1368] = t$1364,0);
var t$1370 = v$786;
var t$1371 = SmlPrims.chk_ovf_i32(v$787 + 1);
var v$786 = t$1370;
var v$787 = t$1371;
continue lab$init;
};
};
};
var init$784 = fix$1363.$init;
init$784(t$783,0);
t$1362 = t$783;
t$1372 = [t$1362,v$118,v$119];
return [t$1372];
 break; }default: {var v$159 = v$99[0];
var v$160 = v$99[1];
var v$161 = v$99[2];
if ((v$159 > 0)?(v$160 > 0):false) {var f00$125 = v$161([0,0]);
var arr$126;
var t$799;
if ((0 <= v$159)?(v$159 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$799 = (Array(v$159));
var fix$1373 = {};
fix$1373.$init = function(v$802,v$803){lab$init: while (true) {if (v$803 >= v$159) {return 0;
} else {var t$1376 = v$802;
var t$1375 = v$803;
var t$1374;
if ((0 <= v$160)?(v$160 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$1374 = (SmlPrims.wordTableInit(v$160,f00$125));
(t$1376[t$1375] = t$1374,0);
var t$1377 = v$802;
var t$1378 = SmlPrims.chk_ovf_i32(v$803 + 1);
var v$802 = t$1377;
var v$803 = t$1378;
continue lab$init;
};
};
};
var init$800 = fix$1373.$init;
init$800(t$799,0);
arr$126 = t$799;
var v$808 = qRjCLbdhz7biMtuhsfIhkt$3basis$0VectorSlice$1$3.slice$217(arr$126,1,[1]);
var v$809 = v$808[0];
var v$810 = v$808[1];
var v$811 = v$808[2];
var stop$812 = SmlPrims.chk_ovf_i32(v$810 + v$811);
var fix$1379 = {};
fix$1379.$lr = function(j$814){lab$lr: while (true) {if (j$814 < stop$812) {var v$1258 = SmlPrims.chk_ovf_i32(j$814 - v$810);
var v$1259 = v$809[j$814];
var v$1129 = v$161([SmlPrims.chk_ovf_i32(v$1258 + 1),0]);
var v$1130 = v$1259.length;
if (0 < v$1130) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
(v$1259[0] = v$1129,0);
var t$1380 = SmlPrims.chk_ovf_i32(j$814 + 1);
var j$814 = t$1380;
continue lab$lr;
} else {return 0;
};
};
};
var lr$813 = fix$1379.$lr;
lr$813(v$810);
var fix$1381 = {};
fix$1381.$loop = function(c$139){lab$loop: while (true) {if (c$139 < v$160) {var stop$822 = arr$126.length;
var fix$1382 = {};
fix$1382.$lr = function(j$824){lab$lr: while (true) {if (j$824 < stop$822) {var v$1261 = arr$126[j$824];
var v$1134 = v$161([j$824,c$139]);
var v$1135 = v$1261.length;
if ((0 <= c$139)?(c$139 < v$1135):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
(v$1261[c$139] = v$1134,0);
var t$1383 = SmlPrims.chk_ovf_i32(j$824 + 1);
var j$824 = t$1383;
continue lab$lr;
} else {return 0;
};
};
};
var lr$823 = fix$1382.$lr;
lr$823(0);
var t$1384 = SmlPrims.chk_ovf_i32(c$139 + 1);
var c$139 = t$1384;
continue lab$loop;
} else {return 0;
};
};
};
var loop$136 = fix$1381.$loop;
loop$136(1);
return [[arr$126,v$159,v$160]];
} else {var t$1385 = 0;
var t$1386 = [v$159,v$160,v$161];
var v$96 = t$1385;
var v$99 = t$1386;
continue lab$tabulate;
};
} };
};
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.tabulate$93 = fix$1361.$tabulate;
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.dimensions$162 = function(v$165){v$165[0][0];
return [v$165[0][1],v$165[0][2]];
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.nRows$174 = function(v$177){v$177[0][0];
var v$184 = v$177[0][1];
v$177[0][2];
return v$184;
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.nCols$186 = function(v$189){v$189[0][0];
v$189[0][1];
return v$189[0][2];
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.sub$198 = function(v$209,v$1286,v$1287){var v$210 = v$209[0][0];
v$209[0][1];
v$209[0][2];
var v$1045;
var v$837 = v$210.length;
if ((0 <= v$1286)?(v$1286 < v$837):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1045 = v$210[v$1286];
var v$833 = v$1045.length;
if ((0 <= v$1287)?(v$1287 < v$833):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
return v$1045[v$1287];
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.update$215 = function(v$227,v$1288,v$1289,v$1290){var v$228 = v$227[0][0];
v$227[0][1];
v$227[0][2];
var v$1049;
var v$846 = v$228.length;
if ((0 <= v$1288)?(v$1288 < v$846):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1049 = v$228[v$1288];
var v$842 = v$1049.length;
if ((0 <= v$1289)?(v$1289 < v$842):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
return (v$1049[v$1289] = v$1290,0);
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.row$234 = function(v$242,v$1291){var v$243 = v$242[0][0];
var a$847;
var v$858 = v$243.length;
if ((0 <= v$1291)?(v$1291 < v$858):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
a$847 = v$243[v$1291];
var v$848 = a$847.length;
var fix$1387 = {};
fix$1387.$init = function(v$1306,v$1307){lab$init: while (true) {if (v$1307 >= v$848) {return v$1306;
} else {(v$1306[v$1307] = a$847[v$1307],0);
var t$1388 = v$1306;
var t$1389 = SmlPrims.chk_ovf_i32(v$1307 + 1);
var v$1306 = t$1388;
var v$1307 = t$1389;
continue lab$init;
};
};
};
var init$1305 = fix$1387.$init;
return init$1305(Array(v$848),0);
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.column$245 = function(v$266,v$1292){var v$267 = v$266[0][0];
var v$268 = v$266[0][1];
var v$269 = v$266[0][2];
if ((v$1292 < 0)?true:(v$1292 >= v$269)) {throw CompilerInitial.exn$Subscript$50;
} else {var t$862;
if ((0 <= v$268)?(v$268 <= 123456789):false) {0;
} else {throw CompilerInitial.exn$Size$51;
};
t$862 = (Array(v$268));
var fix$1390 = {};
fix$1390.$init = function(v$865,v$866){lab$init: while (true) {if (v$866 >= v$268) {return 0;
} else {var t$1393 = v$865;
var t$1392 = v$866;
var t$1391;
var v$1141;
var v$1142 = v$267.length;
if ((0 <= v$866)?(v$866 < v$1142):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1141 = v$267[v$866];
var v$1143 = v$1141.length;
if ((0 <= v$1292)?(v$1292 < v$1143):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1391 = v$1141[v$1292];
(t$1393[t$1392] = t$1391,0);
var t$1394 = v$865;
var t$1395 = SmlPrims.chk_ovf_i32(v$866 + 1);
var v$865 = t$1394;
var v$866 = t$1395;
continue lab$init;
};
};
};
var init$863 = fix$1390.$init;
init$863(t$862,0);
return t$862;
};
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.fold$271 = function(v$274,v$277,v$280,v$283){switch (v$274) { case 0: {var v$306 = v$283[0][0];
var n$878 = v$306.length;
var fix$1396 = {};
fix$1396.$lr = function(v$881,v$882){lab$lr: while (true) {if (v$881 < n$878) {var t$1401 = SmlPrims.chk_ovf_i32(v$881 + 1);
var t$1397;
var v$1262 = v$306[v$881];
var n$1147 = v$1262.length;
var fix$1398 = {};
fix$1398.$lr = function(v$1150,v$1151){lab$lr: while (true) {if (v$1150 < n$1147) {var t$1399 = SmlPrims.chk_ovf_i32(v$1150 + 1);
var t$1400 = v$277([v$1262[v$1150],v$1151]);
var v$1150 = t$1399;
var v$1151 = t$1400;
continue lab$lr;
} else {return v$1151;
};
};
};
var lr$1148 = fix$1398.$lr;
t$1397 = (lr$1148(0,v$882));
var t$1402 = t$1401;
var t$1403 = t$1397;
var v$881 = t$1402;
var v$882 = t$1403;
continue lab$lr;
} else {return v$882;
};
};
};
var lr$879 = fix$1396.$lr;
return lr$879(0,v$280);
 break; }default: {var v$335 = v$283[0][0];
var v$336 = v$283[0][1];
var v$337 = v$283[0][2];
var fix$1404 = {};
fix$1404.$cols = function(j$324,b$327){lab$cols: while (true) {if (j$324 >= v$337) {return b$327;
} else {var t$1414 = SmlPrims.chk_ovf_i32(j$324 + 1);
var t$1413;
var fix$1405 = {};
fix$1405.$rows = function(i$1154,b$1155){lab$rows: while (true) {if (i$1154 >= v$336) {return b$1155;
} else {var t$1410 = SmlPrims.chk_ovf_i32(i$1154 + 1);
var t$1409;
var t$1408 = v$277;
var t$1407;
var t$1406;
var v$1156;
var v$1161 = v$335.length;
if ((0 <= i$1154)?(i$1154 < v$1161):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1156 = v$335[i$1154];
var v$1165 = v$1156.length;
if ((0 <= j$324)?(j$324 < v$1165):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1406 = v$1156[j$324];
t$1407 = [t$1406,b$1155];
t$1409 = (t$1408(t$1407));
var t$1411 = t$1410;
var t$1412 = t$1409;
var i$1154 = t$1411;
var b$1155 = t$1412;
continue lab$rows;
};
};
};
var rows$1153 = fix$1405.$rows;
t$1413 = (rows$1153(0,b$327));
var t$1415 = t$1414;
var t$1416 = t$1413;
var j$324 = t$1415;
var b$327 = t$1416;
continue lab$cols;
};
};
};
var cols$321 = fix$1404.$cols;
return cols$321(0,v$280);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.foldi$383 = function(v$386,v$389,v$392,v$395){switch (v$386) { case 0: {var v$438 = v$395[0];
var v$439 = v$438[0][0];
v$438[0][1];
v$438[0][2];
var v$442 = v$395[4];
var v$443 = v$395[1];
var v$444 = v$395[3];
var v$445 = v$395[2];
var v$901 = qRjCLbdhz7biMtuhsfIhkt$3basis$0VectorSlice$1$3.slice$217(v$439,v$442,v$444);
var v$902 = v$901[0];
var v$903 = v$901[1];
var v$904 = v$901[2];
var stop$905 = SmlPrims.chk_ovf_i32(v$903 + v$904);
var fix$1417 = {};
fix$1417.$lr = function(j$907,res$908){lab$lr: while (true) {if (j$907 < stop$905) {var t$1424 = SmlPrims.chk_ovf_i32(j$907 + 1);
var t$1418;
var v$1264 = SmlPrims.chk_ovf_i32(j$907 - v$903);
var v$1265 = v$902[j$907];
var v$1174 = ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.slice$215(v$1265,v$443,v$445);
var v$1175 = v$1174[0];
var v$1176 = v$1174[1];
var v$1177 = v$1174[2];
var stop$1178 = SmlPrims.chk_ovf_i32(v$1176 + v$1177);
var fix$1419 = {};
fix$1419.$lr = function(j$1180,res$1181){lab$lr: while (true) {if (j$1180 < stop$1178) {var t$1421 = SmlPrims.chk_ovf_i32(j$1180 + 1);
var t$1420;
var v$1267 = SmlPrims.chk_ovf_i32(j$1180 - v$1176);
var v$1268 = v$1175[j$1180];
t$1420 = (v$389([SmlPrims.chk_ovf_i32(v$1264 + v$442),SmlPrims.chk_ovf_i32(v$1267 + v$443),v$1268,res$1181]));
var t$1422 = t$1421;
var t$1423 = t$1420;
var j$1180 = t$1422;
var res$1181 = t$1423;
continue lab$lr;
} else {return res$1181;
};
};
};
var lr$1179 = fix$1419.$lr;
t$1418 = (lr$1179(v$1176,res$908));
var t$1425 = t$1424;
var t$1426 = t$1418;
var j$907 = t$1425;
var res$908 = t$1426;
continue lab$lr;
} else {return res$908;
};
};
};
var lr$906 = fix$1417.$lr;
return lr$906(v$903,v$392);
 break; }default: {var v$476 = v$395[0];
var v$477 = v$476[0][0];
var v$478 = v$476[0][1];
var v$479 = v$476[0][2];
var v$480 = v$395[4];
var v$481 = v$395[1];
var v$482 = v$395[3];
var v$483 = v$395[2];
var stoprow$446;
switch (v$482[0]) { case 1: {if ((v$480 < 0)?true:(v$480 > v$478)) {throw CompilerInitial.exn$Subscript$50;
} else {stoprow$446 = v$478;
};
 break; }default: {var v$928 = v$482[1];
if ((v$480 < 0)?true:((v$928 < 0)?true:((SmlPrims.chk_ovf_i32(v$480 + v$928)) > v$478))) {throw CompilerInitial.exn$Subscript$50;
} else {stoprow$446 = (SmlPrims.chk_ovf_i32(v$480 + v$928));
};
} };
var stopcol$447;
switch (v$483[0]) { case 1: {if ((v$481 < 0)?true:(v$481 > v$479)) {throw CompilerInitial.exn$Subscript$50;
} else {stopcol$447 = v$479;
};
 break; }default: {var v$938 = v$483[1];
if ((v$481 < 0)?true:((v$938 < 0)?true:((SmlPrims.chk_ovf_i32(v$481 + v$938)) > v$479))) {throw CompilerInitial.exn$Subscript$50;
} else {stopcol$447 = (SmlPrims.chk_ovf_i32(v$481 + v$938));
};
} };
var fix$1427 = {};
fix$1427.$cols = function(j$465,b$468){lab$cols: while (true) {if (j$465 >= stopcol$447) {return b$468;
} else {var t$1439 = SmlPrims.chk_ovf_i32(j$465 + 1);
var t$1438;
var fix$1428 = {};
fix$1428.$rows = function(i$1188,b$1189){lab$rows: while (true) {if (i$1188 >= stoprow$446) {return b$1189;
} else {var t$1435 = SmlPrims.chk_ovf_i32(i$1188 + 1);
var t$1434;
var t$1433 = v$389;
var t$1432;
var t$1431 = i$1188;
var t$1430 = j$465;
var t$1429;
var v$1190;
var v$1195 = v$477.length;
if ((0 <= i$1188)?(i$1188 < v$1195):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1190 = v$477[i$1188];
var v$1199 = v$1190.length;
if ((0 <= j$465)?(j$465 < v$1199):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1429 = v$1190[j$465];
t$1432 = [t$1431,t$1430,t$1429,b$1189];
t$1434 = (t$1433(t$1432));
var t$1436 = t$1435;
var t$1437 = t$1434;
var i$1188 = t$1436;
var b$1189 = t$1437;
continue lab$rows;
};
};
};
var rows$1187 = fix$1428.$rows;
t$1438 = (rows$1187(v$480,b$468));
var t$1440 = t$1439;
var t$1441 = t$1438;
var j$465 = t$1440;
var b$468 = t$1441;
continue lab$cols;
};
};
};
var cols$462 = fix$1427.$cols;
return cols$462(v$481,v$392);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.app$484 = function(v$487,v$490,v$493){switch (v$487) { case 0: {var v$505 = v$493[0][0];
var n$949 = v$505.length;
var fix$1442 = {};
fix$1442.$lr = function(j$951){lab$lr: while (true) {if (j$951 < n$949) {var a$1200 = v$505[j$951];
var n$1201 = a$1200.length;
var fix$1443 = {};
fix$1443.$lr = function(j$1203){lab$lr: while (true) {if (j$1203 < n$1201) {v$490(a$1200[j$1203]);
var t$1444 = SmlPrims.chk_ovf_i32(j$1203 + 1);
var j$1203 = t$1444;
continue lab$lr;
} else {return 0;
};
};
};
var lr$1202 = fix$1443.$lr;
lr$1202(0);
var t$1445 = SmlPrims.chk_ovf_i32(j$951 + 1);
var j$951 = t$1445;
continue lab$lr;
} else {return 0;
};
};
};
var lr$950 = fix$1442.$lr;
return lr$950(0);
 break; }default: {var fold$1313 = function(v$1314,v$1315,v$1316){switch (v$1314) { case 0: {var v$1317 = v$1316[0][0];
var n$1318 = v$1317.length;
var fix$1446 = {};
fix$1446.$lr = function(v$1320,v$1321){lab$lr: while (true) {if (v$1320 < n$1318) {var t$1453 = SmlPrims.chk_ovf_i32(v$1320 + 1);
var t$1447;
var v$1322 = v$1317[v$1320];
var n$1323 = v$1322.length;
var fix$1448 = {};
fix$1448.$lr = function(v$1325,v$1326){lab$lr: while (true) {if (v$1325 < n$1323) {var t$1450 = SmlPrims.chk_ovf_i32(v$1325 + 1);
var t$1449;
var v$1345 = v$1322[v$1325];
t$1449 = (v$490(v$1345));
var t$1451 = t$1450;
var t$1452 = t$1449;
var v$1325 = t$1451;
var v$1326 = t$1452;
continue lab$lr;
} else {return v$1326;
};
};
};
var lr$1324 = fix$1448.$lr;
t$1447 = (lr$1324(0,v$1321));
var t$1454 = t$1453;
var t$1455 = t$1447;
var v$1320 = t$1454;
var v$1321 = t$1455;
continue lab$lr;
} else {return v$1321;
};
};
};
var lr$1319 = fix$1446.$lr;
return lr$1319(0,v$1315);
 break; }default: {var v$1327 = v$1316[0][0];
var v$1328 = v$1316[0][1];
var v$1329 = v$1316[0][2];
var fix$1456 = {};
fix$1456.$cols = function(j$1331,b$1332){lab$cols: while (true) {if (j$1331 >= v$1329) {return b$1332;
} else {var t$1463 = SmlPrims.chk_ovf_i32(j$1331 + 1);
var t$1462;
var fix$1457 = {};
fix$1457.$rows = function(i$1334,b$1335){lab$rows: while (true) {if (i$1334 >= v$1328) {return b$1335;
} else {var t$1459 = SmlPrims.chk_ovf_i32(i$1334 + 1);
var t$1458;
var v$1347;
var v$1336;
var v$1337 = v$1327.length;
if ((0 <= i$1334)?(i$1334 < v$1337):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1336 = v$1327[i$1334];
var v$1338 = v$1336.length;
if ((0 <= j$1331)?(j$1331 < v$1338):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1347 = v$1336[j$1331];
t$1458 = (v$490(v$1347));
var t$1460 = t$1459;
var t$1461 = t$1458;
var i$1334 = t$1460;
var b$1335 = t$1461;
continue lab$rows;
};
};
};
var rows$1333 = fix$1457.$rows;
t$1462 = (rows$1333(0,b$1332));
var t$1464 = t$1463;
var t$1465 = t$1462;
var j$1331 = t$1464;
var b$1332 = t$1465;
continue lab$cols;
};
};
};
var cols$1330 = fix$1456.$cols;
return cols$1330(0,v$1315);
} };
};
return fold$1313(1,0,v$493);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.appi$512 = function(v$515,v$518,v$521){switch (v$515) { case 0: {var v$549 = v$521[0];
var v$550 = v$549[0][0];
var v$551 = v$521[4];
var v$552 = v$521[1];
var v$553 = v$521[3];
var v$554 = v$521[2];
var v$958 = qRjCLbdhz7biMtuhsfIhkt$3basis$0VectorSlice$1$3.slice$217(v$550,v$551,v$553);
var v$959 = v$958[0];
var v$960 = v$958[1];
var v$961 = v$958[2];
var stop$962 = SmlPrims.chk_ovf_i32(v$960 + v$961);
var fix$1466 = {};
fix$1466.$lr = function(j$964){lab$lr: while (true) {if (j$964 < stop$962) {var v$1270 = SmlPrims.chk_ovf_i32(j$964 - v$960);
var v$1271 = v$959[j$964];
var v$1210 = ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.slice$215(v$1271,v$552,v$554);
var v$1211 = v$1210[0];
var v$1212 = v$1210[1];
var v$1213 = v$1210[2];
var stop$1214 = SmlPrims.chk_ovf_i32(v$1212 + v$1213);
var fix$1467 = {};
fix$1467.$lr = function(j$1216){lab$lr: while (true) {if (j$1216 < stop$1214) {var v$1272 = SmlPrims.chk_ovf_i32(j$1216 - v$1212);
var v$1273 = v$1211[j$1216];
v$518([SmlPrims.chk_ovf_i32(v$1270 + v$551),SmlPrims.chk_ovf_i32(v$1272 + v$552),v$1273]);
var t$1468 = SmlPrims.chk_ovf_i32(j$1216 + 1);
var j$1216 = t$1468;
continue lab$lr;
} else {return 0;
};
};
};
var lr$1215 = fix$1467.$lr;
lr$1215(v$1212);
var t$1469 = SmlPrims.chk_ovf_i32(j$964 + 1);
var j$964 = t$1469;
continue lab$lr;
} else {return 0;
};
};
};
var lr$963 = fix$1466.$lr;
return lr$963(v$960);
 break; }default: {return Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.foldi$383(1,function(v$559){var v$560 = v$559[0];
var v$561 = v$559[1];
var v$562 = v$559[2];
return v$518([v$560,v$561,v$562]);
},0,v$521);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.modify$565 = function(v$568,v$571,v$574){switch (v$568) { case 0: {var v$586 = v$574[0][0];
var n$975 = v$586.length;
var fix$1470 = {};
fix$1470.$lr = function(j$977){lab$lr: while (true) {if (j$977 < n$975) {var a$1220 = v$586[j$977];
var n$1221 = a$1220.length;
var fix$1471 = {};
fix$1471.$lr = function(j$1223){lab$lr: while (true) {if (j$1223 < n$1221) {(a$1220[j$1223] = (v$571(a$1220[j$1223])),0);
var t$1472 = SmlPrims.chk_ovf_i32(j$1223 + 1);
var j$1223 = t$1472;
continue lab$lr;
} else {return 0;
};
};
};
var lr$1222 = fix$1471.$lr;
lr$1222(0);
var t$1473 = SmlPrims.chk_ovf_i32(j$977 + 1);
var j$977 = t$1473;
continue lab$lr;
} else {return 0;
};
};
};
var lr$976 = fix$1470.$lr;
return lr$976(0);
 break; }default: {return Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.foldi$383(1,function(v$591){var v$592 = v$591[0];
var v$593 = v$591[1];
var v$594 = v$591[2];
var v$1277 = v$571(v$594);
var v$1226 = v$574[0][0];
v$574[0][1];
v$574[0][2];
var v$1230;
var v$1231 = v$1226.length;
if ((0 <= v$592)?(v$592 < v$1231):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1230 = v$1226[v$592];
var v$1232 = v$1230.length;
if ((0 <= v$593)?(v$593 < v$1232):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
return (v$1230[v$593] = v$1277,0);
},0,[v$574,0,[1],[1],0]);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.modifyi$602 = function(v$605,v$608,v$611){switch (v$605) { case 0: {var v$640 = v$611[0];
var v$641 = v$640[0][0];
var v$642 = v$611[4];
var v$643 = v$611[1];
var v$644 = v$611[3];
var v$645 = v$611[2];
var v$984 = qRjCLbdhz7biMtuhsfIhkt$3basis$0VectorSlice$1$3.slice$217(v$641,v$642,v$644);
var v$985 = v$984[0];
var v$986 = v$984[1];
var v$987 = v$984[2];
var stop$988 = SmlPrims.chk_ovf_i32(v$986 + v$987);
var fix$1474 = {};
fix$1474.$lr = function(j$990){lab$lr: while (true) {if (j$990 < stop$988) {var v$1278 = SmlPrims.chk_ovf_i32(j$990 - v$986);
var v$1279 = v$985[j$990];
var v$1239 = ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.slice$215(v$1279,v$643,v$645);
var v$1240 = v$1239[0];
var v$1241 = v$1239[1];
var v$1242 = v$1239[2];
var stop$1243 = SmlPrims.chk_ovf_i32(v$1241 + v$1242);
var fix$1475 = {};
fix$1475.$lr = function(j$1245){lab$lr: while (true) {if (j$1245 < stop$1243) {var t$1478 = v$1240;
var t$1477 = j$1245;
var t$1476;
var v$1280 = SmlPrims.chk_ovf_i32(j$1245 - v$1241);
var v$1281 = v$1240[j$1245];
t$1476 = (v$608([SmlPrims.chk_ovf_i32(v$1278 + v$642),SmlPrims.chk_ovf_i32(v$1280 + v$643),v$1281]));
(t$1478[t$1477] = t$1476,0);
var t$1479 = SmlPrims.chk_ovf_i32(j$1245 + 1);
var j$1245 = t$1479;
continue lab$lr;
} else {return 0;
};
};
};
var lr$1244 = fix$1475.$lr;
lr$1244(v$1241);
var t$1480 = SmlPrims.chk_ovf_i32(j$990 + 1);
var j$990 = t$1480;
continue lab$lr;
} else {return 0;
};
};
};
var lr$989 = fix$1474.$lr;
return lr$989(v$986);
 break; }default: {var v$656 = v$611[0];
return Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.foldi$383(1,function(v$650){var v$651 = v$650[0];
var v$652 = v$650[1];
var v$653 = v$650[2];
var v$1285 = v$608([v$651,v$652,v$653]);
var v$1251 = v$656[0][0];
v$656[0][1];
v$656[0][2];
var v$1255;
var v$1256 = v$1251.length;
if ((0 <= v$651)?(v$651 < v$1256):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
v$1255 = v$1251[v$651];
var v$1257 = v$1255.length;
if ((0 <= v$652)?(v$652 < v$1257):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
return (v$1255[v$652] = v$1285,0);
},0,v$611);
} };
};
Y5y0a7l58ITwhpccCWLDEa$3basis$0Array2$1.copy$657 = function(v$1293,v$1294,v$1295,v$712){var v$713 = v$712[0];
var v$714 = v$713[0][0];
var v$715 = v$713[0][1];
v$713[0][2];
var v$717 = v$712[4];
var v$718 = v$712[1];
var v$719 = v$712[3];
var v$720 = v$712[2];
var v$722 = v$1293[0][0];
v$1293[0][1];
v$1293[0][2];
var stoprow$675;
switch (v$719[0]) { case 1: {if ((v$717 < 0)?true:(v$717 > v$715)) {throw CompilerInitial.exn$Subscript$50;
} else {stoprow$675 = v$715;
};
 break; }default: {var v$1008 = v$719[1];
if ((v$717 < 0)?true:((v$1008 < 0)?true:((SmlPrims.chk_ovf_i32(v$717 + v$1008)) > v$715))) {throw CompilerInitial.exn$Subscript$50;
} else {stoprow$675 = (SmlPrims.chk_ovf_i32(v$717 + v$1008));
};
} };
var fix$1481 = {};
fix$1481.$bottomUp = function(from_row$679,to_row$682){lab$bottomUp: while (true) {if (from_row$679 < v$717) {return 0;
} else {var v$1296;
var v$689;
var t$1483 = ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.slice$215;
var t$1482;
var v$1012 = v$714.length;
if ((0 <= from_row$679)?(from_row$679 < v$1012):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1482 = v$714[from_row$679];
v$689 = (t$1483(t$1482,v$718,v$720));
var t$1485 = v$1294;
var t$1484;
var v$1016 = v$722.length;
if ((0 <= to_row$682)?(to_row$682 < v$1016):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1484 = v$722[to_row$682];
v$1296 = [t$1485,t$1484,v$689];
ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.copy$330(v$1296[0],v$1296[1],v$1296[2]);
var t$1486 = SmlPrims.chk_ovf_i32(from_row$679 - 1);
var t$1487 = SmlPrims.chk_ovf_i32(to_row$682 - 1);
var from_row$679 = t$1486;
var to_row$682 = t$1487;
continue lab$bottomUp;
};
};
};
var bottomUp$676 = fix$1481.$bottomUp;
var fix$1488 = {};
fix$1488.$topDown = function(from_row$695,to_row$698){lab$topDown: while (true) {if (from_row$695 >= stoprow$675) {return 0;
} else {var v$1297;
var v$705;
var t$1490 = ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.slice$215;
var t$1489;
var v$1020 = v$714.length;
if ((0 <= from_row$695)?(from_row$695 < v$1020):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1489 = v$714[from_row$695];
v$705 = (t$1490(t$1489,v$718,v$720));
var t$1492 = v$1294;
var t$1491;
var v$1024 = v$722.length;
if ((0 <= to_row$698)?(to_row$698 < v$1024):false) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
t$1491 = v$722[to_row$698];
v$1297 = [t$1492,t$1491,v$705];
ErW6Fhm4kgBhHxz7QscJZe$3basis$0ArraySlice$1$3.copy$330(v$1297[0],v$1297[1],v$1297[2]);
var t$1493 = SmlPrims.chk_ovf_i32(from_row$695 + 1);
var t$1494 = SmlPrims.chk_ovf_i32(to_row$698 + 1);
var from_row$695 = t$1493;
var to_row$698 = t$1494;
continue lab$topDown;
};
};
};
var topDown$692 = fix$1488.$topDown;
if (v$717 <= v$1295) {return bottomUp$676(SmlPrims.chk_ovf_i32(stoprow$675 - 1),SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(stoprow$675 - 1)) + v$1295)) - v$717));
} else {return topDown$692(v$717,v$1295);
};
};
return 0;
})();
