if ((typeof(basis$0List$1)) == "undefined") {basis$0List$1 = {};
};
(function(){basis$0List$1.en$Empty$55 = new String("Empty");
basis$0List$1.exn$Empty$55 = [basis$0List$1.en$Empty$55];
basis$0List$1.null$56 = function(v$59){return (v$59 == null)?true:false;
};
basis$0List$1.hd$64 = function(v$67){if (v$67 == null) {throw basis$0List$1.exn$Empty$55;
} else {return v$67[0];
};
};
basis$0List$1.tl$77 = function(v$80){if (v$80 == null) {throw basis$0List$1.exn$Empty$55;
} else {return v$80[1];
};
};
var fix$879 = {};
fix$879.$last = function(v$93){lab$last: while (true) {if (v$93 == null) {throw basis$0List$1.exn$Empty$55;
} else {var v$103 = v$93;
var v$104 = v$103[1];
if (v$104 == null) {return v$103[0];
} else {var t$880 = v$104;
var v$93 = t$880;
continue lab$last;
};
};
};
};
basis$0List$1.last$90 = fix$879.$last;
basis$0List$1.nth$107 = function(v$139,v$140){var fix$881 = {};
fix$881.$h = function(v$115,v$118){lab$h: while (true) {if (v$115 == null) {throw CompilerInitial.exn$Subscript$52;
} else {var v$131 = v$115;
var v$132 = v$131[0];
var v$133 = v$131[1];
if (v$118 == 0) {return v$132;
} else {var t$882 = v$133;
var t$883 = SmlPrims.chk_ovf_i32(v$118 - 1);
var v$115 = t$882;
var v$118 = t$883;
continue lab$h;
};
};
};
};
var h$112 = fix$881.$h;
if (v$140 < 0) {throw CompilerInitial.exn$Subscript$52;
} else {return h$112(v$139,v$140);
};
};
basis$0List$1.drop$141 = function(v$174,v$175){var fix$884 = {};
fix$884.$h = function(v$149,v$152){lab$h: while (true) {switch (v$152) { case 0: {return v$149;
 break; }default: {if (v$149 == null) {throw CompilerInitial.exn$Subscript$52;
} else {var v$167 = v$149;
var v$169 = v$167[1];
var t$885 = v$169;
var t$886 = SmlPrims.chk_ovf_i32(v$152 - 1);
var v$149 = t$885;
var v$152 = t$886;
continue lab$h;
};
} };
};
};
var h$146 = fix$884.$h;
if (v$175 < 0) {throw CompilerInitial.exn$Subscript$52;
} else {return h$146(v$174,v$175);
};
};
basis$0List$1.take$176 = function(v$209,v$210){var fix$887 = {};
fix$887.$h = function(v$184,v$187){switch (v$187) { case 0: {return null;
 break; }default: {if (v$184 == null) {throw CompilerInitial.exn$Subscript$52;
} else {var v$202 = v$184;
var v$203 = v$202[0];
var v$204 = v$202[1];
return [v$203,fix$887.$h(v$204,SmlPrims.chk_ovf_i32(v$187 - 1))];
};
} };
};
var h$181 = fix$887.$h;
if (v$210 < 0) {throw CompilerInitial.exn$Subscript$52;
} else {return h$181(v$209,v$210);
};
};
basis$0List$1.length$211 = function(xs$214){var fix$888 = {};
fix$888.$acc = function(v$218,v$221){lab$acc: while (true) {if (v$218 == null) {return v$221;
} else {var v$232 = v$218;
var v$234 = v$232[1];
var t$889 = v$234;
var t$890 = SmlPrims.chk_ovf_i32(v$221 + 1);
var v$218 = t$889;
var v$221 = t$890;
continue lab$acc;
};
};
};
var acc$215 = fix$888.$acc;
return acc$215(xs$214,0);
};
var fix$891 = {};
fix$891.$revAcc = function(v$239,v$242){lab$revAcc: while (true) {if (v$239 == null) {return v$242;
} else {var v$253 = v$239;
var v$254 = v$253[0];
var v$255 = v$253[1];
var t$892 = v$255;
var t$893 = [v$254,v$242];
var v$239 = t$892;
var v$242 = t$893;
continue lab$revAcc;
};
};
};
var revAcc$236 = fix$891.$revAcc;
basis$0List$1.rev$257 = function(xs$260){return revAcc$236(xs$260,null);
};
basis$0List$1.revAppend$261 = function(v$266,v$267){return revAcc$236(v$266,v$267);
};
basis$0List$1.s$n$268 = function(v$294,v$295){var fix$894 = {};
fix$894.$loop = function(v$276,v$279){lab$loop: while (true) {if (v$276 == null) {return v$279;
} else {var v$290 = v$276;
var v$291 = v$290[0];
var v$292 = v$290[1];
var t$895 = v$292;
var t$896 = [v$291,v$279];
var v$276 = t$895;
var v$279 = t$896;
continue lab$loop;
};
};
};
var loop$273 = fix$894.$loop;
return loop$273(revAcc$236(v$294,null),v$295);
};
var fix$897 = {};
fix$897.$concat = function(v$299){if (v$299 == null) {return null;
} else {var v$306 = v$299;
var v$307 = v$306[0];
var v$308 = v$306[1];
var v$819 = fix$897.$concat(v$308);
var fix$956 = {};
fix$956.$loop = function(v$755,v$756){lab$loop: while (true) {if (v$755 == null) {return v$756;
} else {var v$759 = v$755;
var v$760 = v$759[0];
var v$761 = v$759[1];
var t$957 = v$761;
var t$958 = [v$760,v$756];
var v$755 = t$957;
var v$756 = t$958;
continue lab$loop;
};
};
};
var loop$754 = fix$956.$loop;
return loop$754(revAcc$236(v$307,null),v$819);
};
};
basis$0List$1.concat$296 = fix$897.$concat;
var fix$898 = {};
fix$898.$app = function(v$312,v$315){lab$app: while (true) {if (v$315 == null) {return 0;
} else {var v$329 = v$315;
var v$330 = v$329[0];
var v$331 = v$329[1];
v$312(v$330);
var t$899 = v$312;
var t$900 = v$331;
var v$312 = t$899;
var v$315 = t$900;
continue lab$app;
};
};
};
basis$0List$1.app$309 = fix$898.$app;
basis$0List$1.map$332 = function(f$335,xs$338){var fix$901 = {};
fix$901.$map0 = function(v$351,v$875){lab$map0: while (true) {if (v$351 == null) {return v$875;
} else {var v$353 = v$351;
var v$354 = v$353[0];
var v$355 = v$353[1];
var t$902 = v$355;
var t$903 = [f$335(v$354),v$875];
var v$351 = t$902;
var v$875 = t$903;
continue lab$map0;
};
};
};
var map0$339 = fix$901.$map0;
var xs$764 = map0$339(xs$338,null);
return revAcc$236(xs$764,null);
};
basis$0List$1.mapPartial$357 = function(f$360,xs$363){var fix$904 = {};
fix$904.$aux = function(v$367,v$370){lab$aux: while (true) {if (v$370 == null) {return v$367;
} else {var v$389 = v$370;
var v$390 = v$389[0];
var v$391 = v$389[1];
var v$385 = f$360(v$390);
switch (v$385[0]) { case 0: {var v$387 = v$385[1];
var t$905 = [v$387,v$367];
var t$906 = v$391;
var v$367 = t$905;
var v$370 = t$906;
continue lab$aux;
 break; }default: {var t$907 = v$367;
var t$908 = v$391;
var v$367 = t$907;
var v$370 = t$908;
continue lab$aux;
} };
};
};
};
var aux$364 = fix$904.$aux;
var xs$765 = aux$364(null,xs$363);
return revAcc$236(xs$765,null);
};
var fix$909 = {};
fix$909.$find = function(v$395,v$398){lab$find: while (true) {if (v$398 == null) {return [1];
} else {var v$414 = v$398;
var v$415 = v$414[0];
var v$416 = v$414[1];
if (v$395(v$415)) {return [0,v$415];
} else {var t$910 = v$395;
var t$911 = v$416;
var v$395 = t$910;
var v$398 = t$911;
continue lab$find;
};
};
};
};
basis$0List$1.find$392 = fix$909.$find;
basis$0List$1.filter$417 = function(p$420,xs$423){var fix$912 = {};
fix$912.$aux = function(v$427,v$430){lab$aux: while (true) {if (v$430 == null) {return v$427;
} else {var v$446 = v$430;
var v$447 = v$446[0];
var v$448 = v$446[1];
if (p$420(v$447)) {var t$913 = [v$447,v$427];
var t$914 = v$448;
var v$427 = t$913;
var v$430 = t$914;
continue lab$aux;
} else {var t$915 = v$427;
var t$916 = v$448;
var v$427 = t$915;
var v$430 = t$916;
continue lab$aux;
};
};
};
};
var aux$424 = fix$912.$aux;
var xs$766 = aux$424(null,xs$423);
return revAcc$236(xs$766,null);
};
basis$0List$1.partition$449 = function(p$452,xs$455){var fix$917 = {};
fix$917.$h = function(v$459,v$462,v$465){lab$h: while (true) {if (v$459 == null) {return [revAcc$236(v$462,null),revAcc$236(v$465,null)];
} else {var v$483 = v$459;
var v$484 = v$483[0];
var v$485 = v$483[1];
if (p$452(v$484)) {var t$918 = v$485;
var t$919 = [v$484,v$462];
var t$920 = v$465;
var v$459 = t$918;
var v$462 = t$919;
var v$465 = t$920;
continue lab$h;
} else {var t$921 = v$485;
var t$922 = v$462;
var t$923 = [v$484,v$465];
var v$459 = t$921;
var v$462 = t$922;
var v$465 = t$923;
continue lab$h;
};
};
};
};
var h$456 = fix$917.$h;
return h$456(xs$455,null,null);
};
var fix$924 = {};
fix$924.$foldr = function(v$491,v$494,v$497){if (v$497 == null) {return v$494;
} else {var v$513 = v$497;
var v$514 = v$513[0];
var v$515 = v$513[1];
return v$491([v$514,fix$924.$foldr(v$491,v$494,v$515)]);
};
};
basis$0List$1.foldr$488 = fix$924.$foldr;
var fix$925 = {};
fix$925.$foldl = function(v$519,v$522,v$525){lab$foldl: while (true) {if (v$525 == null) {return v$522;
} else {var v$541 = v$525;
var v$542 = v$541[0];
var v$543 = v$541[1];
var t$926 = v$519;
var t$927 = v$519([v$542,v$522]);
var t$928 = v$543;
var v$519 = t$926;
var v$522 = t$927;
var v$525 = t$928;
continue lab$foldl;
};
};
};
basis$0List$1.foldl$516 = fix$925.$foldl;
var fix$929 = {};
fix$929.$exists = function(v$547,v$550){lab$exists: while (true) {if (v$550 == null) {return false;
} else {var v$566 = v$550;
var v$567 = v$566[0];
var v$568 = v$566[1];
if (v$547(v$567)) {return true;
} else {var t$930 = v$547;
var t$931 = v$568;
var v$547 = t$930;
var v$550 = t$931;
continue lab$exists;
};
};
};
};
basis$0List$1.exists$544 = fix$929.$exists;
var fix$932 = {};
fix$932.$all = function(v$572,v$575){lab$all: while (true) {if (v$575 == null) {return true;
} else {var v$591 = v$575;
var v$592 = v$591[0];
var v$593 = v$591[1];
if (v$572(v$592)) {var t$933 = v$572;
var t$934 = v$593;
var v$572 = t$933;
var v$575 = t$934;
continue lab$all;
} else {return false;
};
};
};
};
basis$0List$1.all$569 = fix$932.$all;
basis$0List$1.tabulate$594 = function(v$614,v$615){var fix$935 = {};
fix$935.$h = function(v$608,v$609){lab$h: while (true) {if (v$608 < v$614) {var t$936 = SmlPrims.chk_ovf_i32(v$608 + 1);
var t$937 = [v$615(v$608),v$609];
var v$608 = t$936;
var v$609 = t$937;
continue lab$h;
} else {return v$609;
};
};
};
var h$599 = fix$935.$h;
if (v$614 < 0) {throw CompilerInitial.exn$Size$53;
} else {var xs$769 = h$599(0,null);
return revAcc$236(xs$769,null);
};
};
basis$0List$1.getItem$616 = function(v$619){if (v$619 == null) {return [1];
} else {var v$626 = v$619;
return [0,[v$626[0],v$626[1]]];
};
};
var fix$938 = {};
fix$938.$collate = function(v$632,v$635){lab$collate: while (true) {var v$653 = v$635[0];
if (v$653 == null) {return (v$635[1] == null)?2:0;
} else {var v$657 = v$635[1];
if (v$657 == null) {return 1;
} else {var v$666 = v$653;
var v$667 = v$666[0];
var v$668 = v$666[1];
var v$669 = v$657;
var v$670 = v$669[0];
var v$671 = v$669[1];
switch (v$632([v$667,v$670])) { case 2: {var t$939 = v$632;
var t$940 = [v$668,v$671];
var v$632 = t$939;
var v$635 = t$940;
continue lab$collate;
 break; }case 1: {return 1;
 break; }default: {return 0;
} };
};
};
};
};
basis$0List$1.collate$629 = fix$938.$collate;
basis$0List$1.null$672 = function(a$675){return (a$675 == null)?true:false;
};
basis$0List$1.hd$676 = function(v$771){if (v$771 == null) {throw basis$0List$1.exn$Empty$55;
} else {return v$771[0];
};
};
basis$0List$1.tl$677 = function(v$772){if (v$772 == null) {throw basis$0List$1.exn$Empty$55;
} else {return v$772[1];
};
};
basis$0List$1.length$678 = function(a$681){var fix$941 = {};
fix$941.$acc = function(v$775,v$776){lab$acc: while (true) {if (v$775 == null) {return v$776;
} else {var v$779 = v$775;
var v$780 = v$779[1];
var t$942 = v$780;
var t$943 = SmlPrims.chk_ovf_i32(v$776 + 1);
var v$775 = t$942;
var v$776 = t$943;
continue lab$acc;
};
};
};
var acc$774 = fix$941.$acc;
return acc$774(a$681,0);
};
basis$0List$1.rev$682 = function(a$685){return revAcc$236(a$685,null);
};
basis$0List$1.s$n$686 = function(v$784,v$785){var fix$944 = {};
fix$944.$loop = function(v$787,v$788){lab$loop: while (true) {if (v$787 == null) {return v$788;
} else {var v$791 = v$787;
var v$792 = v$791[0];
var v$793 = v$791[1];
var t$945 = v$793;
var t$946 = [v$792,v$788];
var v$787 = t$945;
var v$788 = t$946;
continue lab$loop;
};
};
};
var loop$786 = fix$944.$loop;
return loop$786(revAcc$236(v$784,null),v$785);
};
basis$0List$1.app$690 = function(f$693,l$696){var fix$947 = {};
fix$947.$app = function(v$851){lab$app: while (true) {if (v$851 == null) {return 0;
} else {var v$852 = v$851;
var v$853 = v$852[0];
var v$854 = v$852[1];
f$693(v$853);
var t$948 = v$854;
var v$851 = t$948;
continue lab$app;
};
};
};
var app$850 = fix$947.$app;
return app$850(l$696);
};
basis$0List$1.map$697 = function(f$700,l$703){var fix$949 = {};
fix$949.$map0 = function(v$800,v$876){lab$map0: while (true) {if (v$800 == null) {return v$876;
} else {var v$801 = v$800;
var v$802 = v$801[0];
var v$803 = v$801[1];
var t$950 = v$803;
var t$951 = [f$700(v$802),v$876];
var v$800 = t$950;
var v$876 = t$951;
continue lab$map0;
};
};
};
var map0$798 = fix$949.$map0;
var xs$805 = map0$798(l$703,null);
return revAcc$236(xs$805,null);
};
basis$0List$1.foldr$704 = function(a$707,b$710,c$713){var fix$952 = {};
fix$952.$foldr = function(v$871){if (v$871 == null) {return b$710;
} else {var v$872 = v$871;
var v$873 = v$872[0];
var v$874 = v$872[1];
return a$707([v$873,fix$952.$foldr(v$874)]);
};
};
var foldr$870 = fix$952.$foldr;
return foldr$870(c$713);
};
basis$0List$1.foldl$714 = function(a$717,b$720,c$723){var fix$953 = {};
fix$953.$foldl = function(v$864,v$865){lab$foldl: while (true) {if (v$865 == null) {return v$864;
} else {var v$866 = v$865;
var v$867 = v$866[0];
var v$868 = v$866[1];
var t$954 = a$717([v$867,v$864]);
var t$955 = v$868;
var v$864 = t$954;
var v$865 = t$955;
continue lab$foldl;
};
};
};
var foldl$863 = fix$953.$foldl;
return foldl$863(b$720,c$723);
};
return 0;
})();
