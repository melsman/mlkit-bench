if ((typeof(oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1)) == "undefined") {oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1 = {};
};
(function(){oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.bytesPerElem$52 = 4;
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.isBigEndian$53 = false;
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subVec$77 = function(v$83,v$84){var v$274 = v$83.length;
var len$173 = (SmlPrims.i32_to_w32(v$274)) >>> 2;
if ((SmlPrims.i32_to_w32(v$84)) < len$173) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
var k$82 = SmlPrims.w32_to_i32_X(((SmlPrims.i32_to_w32(v$84)) << (2 & 31)) & 4294967295);
var v$342;
if ((k$82 < 0)?true:(k$82 >= v$83.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$342 = (v$83.charCodeAt(k$82));
};
var v$343;
var v$283 = SmlPrims.chk_ovf_i32(k$82 + 1);
if ((v$283 < 0)?true:(v$283 >= v$83.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$343 = (v$83.charCodeAt(v$283));
};
var v$344;
var v$285 = SmlPrims.chk_ovf_i32(k$82 + 2);
if ((v$285 < 0)?true:(v$285 >= v$83.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$344 = (v$83.charCodeAt(v$285));
};
var t$353;
var t$352;
var t$351;
var t$350;
var v$287 = SmlPrims.chk_ovf_i32(k$82 + 3);
if ((v$287 < 0)?true:(v$287 >= v$83.length)) {throw CompilerInitial.exn$Subscript$50;
} else {t$350 = (v$83.charCodeAt(v$287));
};
t$351 = t$350;
t$352 = t$351;
t$353 = ((t$352 << (24 & 31)) & 4294967295);
return t$353 | (((v$344 << (16 & 31)) & 4294967295) | (((v$343 << (8 & 31)) & 4294967295) | v$342));
};
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subVecX$85 = function(v$90,v$91){return oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subVec$77(v$90,v$91);
};
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subArr$92 = function(v$98,v$99){var v$288 = v$98.length;
var len$198 = (SmlPrims.i32_to_w32(v$288)) >>> 2;
if ((SmlPrims.i32_to_w32(v$99)) < len$198) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
var k$97 = SmlPrims.w32_to_i32_X(((SmlPrims.i32_to_w32(v$99)) << (2 & 31)) & 4294967295);
var v$346;
if ((k$97 < 0)?true:(k$97 >= v$98.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$346 = v$98[k$97];
};
var v$347;
var v$297 = SmlPrims.chk_ovf_i32(k$97 + 1);
if ((v$297 < 0)?true:(v$297 >= v$98.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$347 = v$98[v$297];
};
var v$348;
var v$299 = SmlPrims.chk_ovf_i32(k$97 + 2);
if ((v$299 < 0)?true:(v$299 >= v$98.length)) {throw CompilerInitial.exn$Subscript$50;
} else {v$348 = v$98[v$299];
};
var t$357;
var t$356;
var t$355;
var t$354;
var v$301 = SmlPrims.chk_ovf_i32(k$97 + 3);
if ((v$301 < 0)?true:(v$301 >= v$98.length)) {throw CompilerInitial.exn$Subscript$50;
} else {t$354 = v$98[v$301];
};
t$355 = t$354;
t$356 = t$355;
t$357 = ((t$356 << (24 & 31)) & 4294967295);
return t$357 | (((v$348 << (16 & 31)) & 4294967295) | (((v$347 << (8 & 31)) & 4294967295) | v$346));
};
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subArrX$100 = function(v$105,v$106){return oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.subArr$92(v$105,v$106);
};
oazD1Cd4rrhfjBvkPDMRDk$3basis$0Pack32Little$1.update$107 = function(v$120,v$121,v$122){var v$302 = v$120.length;
var len$223 = (SmlPrims.i32_to_w32(v$302)) >>> 2;
if ((SmlPrims.i32_to_w32(v$121)) < len$223) {0;
} else {throw CompilerInitial.exn$Subscript$50;
};
var k$113 = SmlPrims.w32_to_i32_X(((SmlPrims.i32_to_w32(v$121)) << (2 & 31)) & 4294967295);
var v$310 = 255 & v$122;
if ((k$113 < 0)?true:(k$113 >= v$120.length)) {throw CompilerInitial.exn$Subscript$50;
} else {(v$120[k$113] = v$310,0);
};
var v$312 = SmlPrims.chk_ovf_i32(k$113 + 1);
var v$313 = 255 & (v$122 >>> 8);
if ((v$312 < 0)?true:(v$312 >= v$120.length)) {throw CompilerInitial.exn$Subscript$50;
} else {(v$120[v$312] = v$313,0);
};
var v$317 = SmlPrims.chk_ovf_i32(k$113 + 2);
var v$318 = 255 & (v$122 >>> 16);
if ((v$317 < 0)?true:(v$317 >= v$120.length)) {throw CompilerInitial.exn$Subscript$50;
} else {(v$120[v$317] = v$318,0);
};
var v$322 = SmlPrims.chk_ovf_i32(k$113 + 3);
var v$323 = 255 & (v$122 >>> 24);
if ((v$322 < 0)?true:(v$322 >= v$120.length)) {throw CompilerInitial.exn$Subscript$50;
} else {return (v$120[v$322] = v$323,0);
};
};
return 0;
})();
