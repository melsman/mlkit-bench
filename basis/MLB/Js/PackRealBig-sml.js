if ((typeof(basis$0PackRealBig$1)) == "undefined") {basis$0PackRealBig$1 = {};
};
(function(){basis$0PackRealBig$1.bytesPerElem$55 = 8;
basis$0PackRealBig$1.isBigEndian$56 = true;
basis$0PackRealBig$1.toBytes$65 = function(r$68){var v$185 = SmlPrims.real_to_bytes(r$68);
var len$186 = v$185.length;
if ((len$186 < 0)?true:(len$186 > 16777211)) {throw CompilerInitial.exn$Size$53;
} else {var t$187 = Array(len$186);
var fix$224 = {};
fix$224.$loop = function(j$189){lab$loop: while (true) {if (j$189 < len$186) {var t$227 = t$187;
var t$226 = j$189;
var t$225;
var v$191 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$186 - 1)) - j$189);
if ((v$191 < 0)?true:(v$191 >= v$185.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$225 = (v$185.charCodeAt(v$191));
};
(t$227[t$226] = t$225,0);
var t$228 = SmlPrims.chk_ovf_i32(j$189 + 1);
var j$189 = t$228;
continue lab$loop;
} else {return 0;
};
};
};
var loop$188 = fix$224.$loop;
loop$188(0);
return SmlPrims.charArrayToString(t$187);
};
};
basis$0PackRealBig$1.fromBytes$69 = function(v$72){var t$229;
var len$193 = v$72.length;
if ((len$193 < 0)?true:(len$193 > 16777211)) {throw CompilerInitial.exn$Size$53;
} else {var t$194 = Array(len$193);
var fix$230 = {};
fix$230.$loop = function(j$196){lab$loop: while (true) {if (j$196 < len$193) {var t$233 = t$194;
var t$232 = j$196;
var t$231;
var v$198 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$193 - 1)) - j$196);
if ((v$198 < 0)?true:(v$198 >= v$72.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$231 = (v$72.charCodeAt(v$198));
};
(t$233[t$232] = t$231,0);
var t$234 = SmlPrims.chk_ovf_i32(j$196 + 1);
var j$196 = t$234;
continue lab$loop;
} else {return 0;
};
};
};
var loop$195 = fix$230.$loop;
loop$195(0);
t$229 = (SmlPrims.charArrayToString(t$194));
};
return SmlPrims.bytes_to_real(t$229);
};
basis$0PackRealBig$1.subVec$73 = function(v$78,v$79){var v$171;
var len$200 = v$78.length;
if ((len$200 < 0)?true:(len$200 > 16777211)) {throw CompilerInitial.exn$Size$53;
} else {var t$201 = Array(len$200);
var fix$235 = {};
fix$235.$loop = function(j$203){lab$loop: while (true) {if (j$203 < len$200) {var t$238 = t$201;
var t$237 = j$203;
var t$236;
var v$205 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$200 - 1)) - j$203);
if ((v$205 < 0)?true:(v$205 >= v$78.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$236 = (v$78.charCodeAt(v$205));
};
(t$238[t$237] = t$236,0);
var t$239 = SmlPrims.chk_ovf_i32(j$203 + 1);
var j$203 = t$239;
continue lab$loop;
} else {return 0;
};
};
};
var loop$202 = fix$235.$loop;
loop$202(0);
v$171 = (SmlPrims.charArrayToString(t$201));
};
var fix$240 = {};
fix$240.$toL = function(v$144,v$145){lab$toL: while (true) {switch (v$144) { case 9: {return v$145;
 break; }default: {var t$244 = SmlPrims.chk_ovf_i32(v$144 + 1);
var t$243;
var t$242;
var t$241;
var v$146 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$79 + 1)) * 8)) - v$144);
if ((v$146 < 0)?true:(v$146 >= v$171.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$241 = (v$171.charCodeAt(v$146));
};
t$242 = [t$241,v$145];
t$243 = t$242;
var t$245 = t$244;
var t$246 = t$243;
var v$144 = t$245;
var v$145 = t$246;
continue lab$toL;
} };
};
};
var toL$143 = fix$240.$toL;
return SmlPrims.bytes_to_real(SmlPrims.implode(toL$143(1,null)));
};
basis$0PackRealBig$1.subArr$80 = function(v$88,v$89){var v$173;
var t$160 = Array(8);
var fix$247 = {};
fix$247.$loop = function(j$162){lab$loop: while (true) {if (j$162 < 8) {var t$250 = t$160;
var t$249 = j$162;
var t$248;
var v$209 = SmlPrims.chk_ovf_i32(j$162 + (SmlPrims.chk_ovf_i32(v$89 * 8)));
if ((v$209 < 0)?true:(v$209 >= v$88.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$248 = v$88[v$209];
};
(t$250[t$249] = t$248,0);
var t$251 = SmlPrims.chk_ovf_i32(j$162 + 1);
var j$162 = t$251;
continue lab$loop;
} else {return 0;
};
};
};
var loop$161 = fix$247.$loop;
loop$161(0);
v$173 = (SmlPrims.charArrayToString(t$160));
var v$179;
var len$211 = v$173.length;
if ((len$211 < 0)?true:(len$211 > 16777211)) {throw CompilerInitial.exn$Size$53;
} else {var t$212 = Array(len$211);
var fix$252 = {};
fix$252.$loop = function(j$214){lab$loop: while (true) {if (j$214 < len$211) {var t$255 = t$212;
var t$254 = j$214;
var t$253;
var v$216 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$211 - 1)) - j$214);
if ((v$216 < 0)?true:(v$216 >= v$173.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$253 = (v$173.charCodeAt(v$216));
};
(t$255[t$254] = t$253,0);
var t$256 = SmlPrims.chk_ovf_i32(j$214 + 1);
var j$214 = t$256;
continue lab$loop;
} else {return 0;
};
};
};
var loop$213 = fix$252.$loop;
loop$213(0);
v$179 = (SmlPrims.charArrayToString(t$212));
};
var fix$257 = {};
fix$257.$toL = function(v$154,v$155){lab$toL: while (true) {switch (v$154) { case 9: {return v$155;
 break; }default: {var t$261 = SmlPrims.chk_ovf_i32(v$154 + 1);
var t$260;
var t$259;
var t$258;
var v$156 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(8 + 1)) * 8)) - v$154);
if ((v$156 < 0)?true:(v$156 >= v$179.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$258 = (v$179.charCodeAt(v$156));
};
t$259 = [t$258,v$155];
t$260 = t$259;
var t$262 = t$261;
var t$263 = t$260;
var v$154 = t$262;
var v$155 = t$263;
continue lab$toL;
} };
};
};
var toL$153 = fix$257.$toL;
return SmlPrims.bytes_to_real(SmlPrims.implode(toL$153(1,null)));
};
basis$0PackRealBig$1.update$90 = function(v$99,v$100,v$101){var t$272 = basis$0ByteTable$1$22.copyVec$4055;
var t$271 = v$100;
var t$270 = v$99;
var t$264;
var v$217 = SmlPrims.real_to_bytes(v$101);
var len$218 = v$217.length;
if ((len$218 < 0)?true:(len$218 > 16777211)) {throw CompilerInitial.exn$Size$53;
} else {var t$219 = Array(len$218);
var fix$265 = {};
fix$265.$loop = function(j$221){lab$loop: while (true) {if (j$221 < len$218) {var t$268 = t$219;
var t$267 = j$221;
var t$266;
var v$223 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(len$218 - 1)) - j$221);
if ((v$223 < 0)?true:(v$223 >= v$217.length)) {throw CompilerInitial.exn$Subscript$52;
} else {t$266 = (v$217.charCodeAt(v$223));
};
(t$268[t$267] = t$266,0);
var t$269 = SmlPrims.chk_ovf_i32(j$221 + 1);
var j$221 = t$269;
continue lab$loop;
} else {return 0;
};
};
};
var loop$220 = fix$265.$loop;
loop$220(0);
t$264 = (SmlPrims.charArrayToString(t$219));
};
return t$272(t$271,t$270,t$264);
};
return 0;
})();