if ((typeof(h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1)) == "undefined") {h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1 = {};
};
(function(){h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toInt$104 = function(x$107){return x$107;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLargeWord$108 = function(w$111){return w$111;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLarge$112 = function(w$583){return w$583;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLargeInt$113 = function(w$116){return TtcHr52auHS5gydZjBhWJe$3basis$0IntInfRep$1.fromWord8$1189(w$116);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fromLargeWord$121 = function(w$124){return 255 & w$124;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fromLarge$125 = function(w$588){return 255 & w$588;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.wordSize$126 = 8;
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fromInt$127 = function(x$130){return 255 & x$130;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$p$131 = function(w$134){return 255 & (SmlPrims.chk_ovf_i32(-(w$134)));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.andb$135 = function(v$745,v$746){return v$745 & v$746;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.orb$142 = function(v$747,v$748){return v$747 | v$748;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.xorb$149 = function(v$749,v$750){return 255 & (v$749 ^ v$750);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toIntX$156 = function(w$159){return (w$159 < 128)?w$159:(SmlPrims.chk_ovf_i32(w$159 - 256));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLargeIntX$164 = function(w$167){return TtcHr52auHS5gydZjBhWJe$3basis$0IntInfRep$1.fromWord8X$1193(w$167);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fromLargeInt$168 = function(i$171){return 255 & (FlgjElH6CKA8G1u69F9ITd$3basis$0Word32$1.fromLargeInt$96(i$171));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLargeWordX$172 = function(w$175){return (w$175 < 128)?w$175:(w$175 | 4294967040);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toLargeX$180 = function(w$621){return (w$621 < 128)?w$621:(w$621 | 4294967040);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.notb$181 = function(x$184){return 255 & (x$184 ^ 255);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$jj$185 = function(v$194,v$195){return (v$195 >= 8)?0:(255 & ((v$194 << (v$195 & 31)) & 4294967295));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$ll$196 = function(v$205,v$206){return (v$206 >= 8)?0:(v$205 >>> v$206);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$pll$207 = function(v$224,v$225){return (v$224 < 128)?((v$225 >= 8)?0:((v$225 >= 8)?0:(v$224 >>> v$225))):((v$225 >= 8)?255:(255 & ((v$224 | 65280) >>> v$225)));
};
var conv$257 = function(radix$260,w$263){var fix$780 = {};
fix$780.$h = function(n$267,res$270){lab$h: while (true) {if (n$267 == 0) {return res$270;
} else {var t$784 = SmlPrims.div_i32(n$267,radix$260,CompilerInitial.exn$Div$45);
var t$783;
var t$782;
var t$781;
var i$651 = SmlPrims.mod_i32(n$267,radix$260,CompilerInitial.exn$Div$45);
t$781 = ((i$651 < 10)?(h8UYb53cympluUQFSuaSASi$3basis$0Char$1.chr$71(SmlPrims.chk_ovf_i32(i$651 + 48))):(h8UYb53cympluUQFSuaSASi$3basis$0Char$1.chr$71(SmlPrims.chk_ovf_i32(i$651 + 55))));
t$782 = [t$781,res$270];
t$783 = t$782;
var t$785 = t$784;
var t$786 = t$783;
var n$267 = t$785;
var res$270 = t$786;
continue lab$h;
};
};
};
var h$264 = fix$780.$h;
var t$787;
var n$654 = w$263;
var t$792 = h$264;
var t$791 = SmlPrims.div_i32(n$654,radix$260,CompilerInitial.exn$Div$45);
var t$790;
var t$789;
var t$788;
var i$655 = SmlPrims.mod_i32(n$654,radix$260,CompilerInitial.exn$Div$45);
t$788 = ((i$655 < 10)?(h8UYb53cympluUQFSuaSASi$3basis$0Char$1.chr$71(SmlPrims.chk_ovf_i32(i$655 + 48))):(h8UYb53cympluUQFSuaSASi$3basis$0Char$1.chr$71(SmlPrims.chk_ovf_i32(i$655 + 55))));
t$789 = [t$788,null];
t$790 = t$789;
t$787 = (t$792(t$791,t$790));
return SmlPrims.implode(t$787);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.scan$279 = function(radix$282,getc$285,source$288){var source$289 = (EnFzYQm6gMCyrmnEKObeWe$3basis$0StringCvt$1.splitl$204(function(c$659){return (c$659 == 32)?true:((9 <= c$659)?(c$659 <= 13):false);
},getc$285,source$288))[1];
var v$411;
switch (radix$282) { case 3: {v$411 = [function(c$431){return (48 <= c$431)?(c$431 <= 49):false;
},2];
 break; }case 0: {v$411 = [function(c$424){return (48 <= c$424)?(c$424 <= 55):false;
},8];
 break; }case 2: {v$411 = [function(c$660){return (48 <= c$660)?(c$660 <= 57):false;
},10];
 break; }default: {v$411 = [function(c$661){return ((48 <= c$661)?(c$661 <= 57):false)?true:(((97 <= c$661)?(c$661 <= 102):false)?true:((65 <= c$661)?(c$661 <= 70):false));
},16];
} };
var v$412 = v$411[0];
var v$413 = v$411[1];
var dig1$304 = function(v$307){switch (v$307[0]) { case 1: {return [1];
 break; }default: {var v$348 = v$307[1];
var v$349 = v$348[0];
var v$350 = v$348[1];
var fix$793 = {};
fix$793.$digr = function(res$317,src$320){lab$digr: while (true) {var v$326 = getc$285(src$320);
switch (v$326[0]) { case 1: {if (res$317 <= 255) {return [0,[res$317,src$320]];
} else {throw CompilerInitial.exn$Overflow$48;
};
 break; }default: {var v$341 = v$326[1];
var v$342 = v$341[0];
var v$343 = v$341[1];
if (v$412(v$342)) {var res1$331 = (v$413 * res$317) & 4294967295;
var res2$332 = (res1$331 + (SmlPrims.i32_to_w32(((48 <= v$342)?(v$342 <= 57):false)?(SmlPrims.chk_ovf_i32(v$342 - 48)):(SmlPrims.mod_i32(SmlPrims.chk_ovf_i32(v$342 - 55),32,CompilerInitial.exn$Div$45))))) & 4294967295;
if ((res1$331 < res$317)?true:(res2$332 < res1$331)) {throw CompilerInitial.exn$Overflow$48;
} else {var t$794 = res2$332;
var t$795 = v$343;
var res$317 = t$794;
var src$320 = t$795;
continue lab$digr;
};
} else {if (res$317 <= 255) {return [0,[res$317,src$320]];
} else {throw CompilerInitial.exn$Overflow$48;
};
};
} };
};
};
var digr$314 = fix$793.$digr;
if (v$412(v$349)) {return digr$314(SmlPrims.i32_to_w32(((48 <= v$349)?(v$349 <= 57):false)?(SmlPrims.chk_ovf_i32(v$349 - 48)):(SmlPrims.mod_i32(SmlPrims.chk_ovf_i32(v$349 - 55),32,CompilerInitial.exn$Div$45))),v$350);
} else {return [1];
};
} };
};
var hexprefix$363 = function(after0$366,src$369){var t$796;
var v$744 = 1;
t$796 = ((EnFzYQm6gMCyrmnEKObeWe$3basis$0StringCvt$1.eq_radix$304(radix$282,v$744))?false:true);
if (t$796) {var v$677 = dig1$304(getc$285(src$369));
switch (v$677[0]) { case 1: {return [0,[0,after0$366]];
 break; }default: {return v$677;
} };
} else {var v$383 = getc$285(src$369);
switch (v$383[0]) { case 0: {var v$385 = v$383[1];
switch (v$385[0]) { case 120: {var v$388 = v$385[1];
var v$682 = dig1$304(getc$285(v$388));
switch (v$682[0]) { case 1: {return [0,[0,after0$366]];
 break; }default: {return v$682;
} };
 break; }case 88: {var v$387 = v$385[1];
var v$687 = dig1$304(getc$285(v$387));
switch (v$687[0]) { case 1: {return [0,[0,after0$366]];
 break; }default: {return v$687;
} };
 break; }default: {var v$692 = dig1$304(getc$285(src$369));
switch (v$692[0]) { case 1: {return [0,[0,after0$366]];
 break; }default: {return v$692;
} };
} };
 break; }default: {return [0,[0,after0$366]];
} };
};
};
var v$395 = getc$285(source$289);
switch (v$395[0]) { case 0: {var v$397 = v$395[1];
switch (v$397[0]) { case 48: {var v$410 = v$397[1];
var v$405 = getc$285(v$410);
switch (v$405[0]) { case 0: {var v$407 = v$405[1];
switch (v$407[0]) { case 119: {var v$409 = v$407[1];
return hexprefix$363(v$410,v$409);
 break; }default: {return hexprefix$363(v$410,v$410);
} };
 break; }default: {return [0,[0,v$410]];
} };
 break; }default: {return dig1$304(getc$285(source$289));
} };
 break; }default: {return [1];
} };
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fmt$436 = function(v$439){switch (v$439) { case 3: {return function(v$770){return conv$257(2,v$770);
};
 break; }case 0: {return function(v$772){return conv$257(8,v$772);
};
 break; }case 2: {return function(v$774){return conv$257(10,v$774);
};
 break; }default: {return function(v$776){return conv$257(16,v$776);
};
} };
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.toString$448 = function(w$451){return conv$257(16,w$451);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.fromString$452 = function(s$455){var t$798 = EnFzYQm6gMCyrmnEKObeWe$3basis$0StringCvt$1.scanString$161;
var t$797;
var v$777 = 1;
t$797 = (function(v$778){return function(v$779){return h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.scan$279(v$777,v$778,v$779);
};
});
return t$798(t$797,s$455);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$f$456 = function(v$751,v$752){return 255 & ((v$751 + v$752) & 4294967295);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$g$463 = function(v$753,v$754){return 255 & ((v$753 - v$754) & 4294967295);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$t$470 = function(v$755,v$756){return 255 & ((v$755 * v$756) & 4294967295);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.div$477 = function(v$757,v$758){return 255 & (SmlPrims.div_w32(v$757,v$758,CompilerInitial.exn$Div$45));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.mod$484 = function(v$759,v$760){return 255 & (SmlPrims.mod_w32(v$759,v$760,CompilerInitial.exn$Div$45));
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.min$491 = function(v$500,v$501){return (v$500 > v$501)?v$501:v$500;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.max$502 = function(v$511,v$512){return (v$511 > v$512)?v$511:v$512;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.compare$513 = function(v$526,v$527){return (v$526 < v$527)?0:((v$526 > v$527)?1:2);
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$l$528 = function(v$761,v$762){return v$761 > v$762;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$lk$535 = function(v$763,v$764){return v$763 >= v$764;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$j$542 = function(v$765,v$766){return v$765 < v$766;
};
h6udeSTTjW7TwTlwwjgy92j$3basis$0Word8$1.s$jk$549 = function(v$767,v$768){return v$767 <= v$768;
};
return 0;
})();
