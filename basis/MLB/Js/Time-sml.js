if ((typeof(g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1)) == "undefined") {g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1 = {};
};
(function(){g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.en$Time$57 = new String("Time");
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57 = [g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.en$Time$57];
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.zeroTime$58 = [-2147483648,0];
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.now$59 = function(v$61){return SmlPrims.getrealtime();
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fromSeconds$66 = function(s$69){if (xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$j$2220(s$69,[0,[null,false]])) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {return [SmlPrims.chk_ovf_i32((xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.toInt$1823(s$69)) + (-2147483648)),0];
};
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fromMilliseconds$74 = function(ms$77){if (xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$j$2220(ms$77,[0,[null,false]])) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {return [SmlPrims.chk_ovf_i32((SmlPrims.div_i32(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.toInt$1823(ms$77),1000,CompilerInitial.exn$Div$45)) + (-2147483648)),SmlPrims.chk_ovf_i32((SmlPrims.mod_i32(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.toInt$1823(ms$77),1000,CompilerInitial.exn$Div$45)) * 1000)];
};
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fromMicroseconds$82 = function(us$85){if (xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$j$2220(us$85,[0,[null,false]])) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {return [SmlPrims.chk_ovf_i32((SmlPrims.div_i32(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.toInt$1823(us$85),1000000,CompilerInitial.exn$Div$45)) + (-2147483648)),SmlPrims.mod_i32(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.toInt$1823(us$85),1000000,CompilerInitial.exn$Div$45)];
};
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.toSeconds$90 = function(v$95,v$596){return xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$g$1985(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(v$95),xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(-2147483648));
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.toMilliseconds$97 = function(v$102,v$103){return xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$f$1941(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$t$1898(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$g$1985(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(v$102),xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(-2147483648)),[0,[[1000,null],false]]),xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.div$2159([xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(v$103),[0,[[1000,null],false]]]));
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.toMicroseconds$104 = function(v$109,v$110){return xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$f$1941(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$t$1898(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.s$g$1985(xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(v$109),xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(-2147483648)),[0,[[1000000,null],false]]),xe4rCkobvAWmCENzbhJ8Ed$3basis$0IntInf$1.fromInt$1827(v$110));
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fromReal$111 = function(r$114){try {var rf$115;
if (r$114 < 0.0) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {rf$115 = (SmlPrims.chk_ovf_i32(Math.floor(r$114 + (-2147483648))));
};
return [rf$115,SmlPrims.chk_ovf_i32(Math.floor(1000000.0 * ((r$114 + (-2147483648)) - rf$115)))];
} catch(v$615) {return (function(Overflow$122){var t$616 = Overflow$122;
if (t$616[0] == CompilerInitial.en$Overflow$48) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {throw Overflow$122;
};
})(v$615);
};
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.toReal$123 = function(v$597,v$598){return (v$597 - (-2147483648)) + (v$598 / 1000000.0);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fmt$137 = function(p$140,t$143){return (y1CWDAIcrGTm72MVeo9tyi$3basis$0Real$1.fmt$169([2,[0,(p$140 > 0)?p$140:0]]))((t$143[0] - (-2147483648)) + (t$143[1] / 1000000.0));
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.toString$148 = function(v$599,v$600){return (y1CWDAIcrGTm72MVeo9tyi$3basis$0Real$1.fmt$169([2,[0,3]]))((v$599 - (-2147483648)) + (v$600 / 1000000.0));
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.scan$152 = function(getc$155,source$158){var fix$617 = {};
fix$617.$pow10 = function(v$173){switch (v$173) { case 0: {return 1;
 break; }default: {return SmlPrims.chk_ovf_i32(10 * (fix$617.$pow10(SmlPrims.chk_ovf_i32(v$173 - 1))));
} };
};
var pow10$170 = fix$617.$pow10;
var fix$618 = {};
fix$618.$skipdigs = function(src$193){lab$skipdigs: while (true) {var v$199 = getc$155(src$193);
switch (v$199[0]) { case 1: {return src$193;
 break; }default: {var v$204 = v$199[1];
var v$205 = v$204[0];
var v$206 = v$204[1];
if ((48 <= v$205)?(v$205 <= 57):false) {var t$619 = v$206;
var src$193 = t$619;
continue lab$skipdigs;
} else {return src$193;
};
} };
};
};
var skipdigs$190 = fix$618.$skipdigs;
var fix$620 = {};
fix$620.$intg = function(intgv$240,src$243){lab$intg: while (true) {var v$252 = getc$155(src$243);
switch (v$252[0]) { case 1: {var t$622;
var t$621;
var intgv$500 = intgv$240;
var usecs$503 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - 6))) * 0)) + 5),10,CompilerInitial.exn$Div$45);
t$621 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((intgv$500 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$503,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$503,1000000,CompilerInitial.exn$Div$45)];
t$622 = [t$621,src$243];
return [0,t$622];
 break; }default: {var v$253 = v$252[1];
var v$254 = v$253[0];
switch (v$254) { case 46: {var v$255 = v$253[1];
var intgv$552 = intgv$240;
var fix$623 = {};
fix$623.$frac = function(decs$554,fracv$555,src$556){lab$frac: while (true) {if (decs$554 >= 7) {var t$625;
var t$624;
var usecs$560 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$554))) * fracv$555)) + 5),10,CompilerInitial.exn$Div$45);
t$624 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((intgv$552 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$560,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$560,1000000,CompilerInitial.exn$Div$45)];
t$625 = [t$624,skipdigs$190(src$556)];
return [0,t$625];
} else {var v$561 = getc$155(src$556);
switch (v$561[0]) { case 1: {var t$627;
var t$626;
var usecs$565 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$554))) * fracv$555)) + 5),10,CompilerInitial.exn$Div$45);
t$626 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((intgv$552 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$565,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$565,1000000,CompilerInitial.exn$Div$45)];
t$627 = [t$626,src$556];
return [0,t$627];
 break; }default: {var v$566 = v$561[1];
var v$567 = v$566[0];
var v$568 = v$566[1];
if ((48 <= v$567)?(v$567 <= 57):false) {var t$630 = SmlPrims.chk_ovf_i32(decs$554 + 1);
var t$631 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(10 * fracv$555)) + (SmlPrims.chk_ovf_i32(v$567 - 48)));
var t$632 = v$568;
var decs$554 = t$630;
var fracv$555 = t$631;
var src$556 = t$632;
continue lab$frac;
} else {var t$629;
var t$628;
var usecs$573 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$554))) * fracv$555)) + 5),10,CompilerInitial.exn$Div$45);
t$628 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((intgv$552 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$573,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$573,1000000,CompilerInitial.exn$Div$45)];
t$629 = [t$628,src$556];
return [0,t$629];
};
} };
};
};
};
var frac$553 = fix$623.$frac;
return frac$553(0,0,v$255);
 break; }default: {var v$260 = v$253[1];
if ((48 <= v$254)?(v$254 <= 57):false) {var t$635 = (10.0 * intgv$240) + (SmlPrims.chk_ovf_i32(v$254 - 48));
var t$636 = v$260;
var intgv$240 = t$635;
var src$243 = t$636;
continue lab$intg;
} else {var t$634;
var t$633;
var intgv$507 = intgv$240;
var usecs$510 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - 6))) * 0)) + 5),10,CompilerInitial.exn$Div$45);
t$633 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((intgv$507 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$510,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$510,1000000,CompilerInitial.exn$Div$45)];
t$634 = [t$633,src$243];
return [0,t$634];
};
} };
} };
};
};
var intg$237 = fix$620.$intg;
var v$269 = getc$155((EnFzYQm6gMCyrmnEKObeWe$3basis$0StringCvt$1.splitl$204(function(c$517){return (c$517 == 32)?true:((9 <= c$517)?(c$517 <= 13):false);
},getc$155,source$158))[1]);
switch (v$269[0]) { case 1: {return [1];
 break; }default: {var v$270 = v$269[1];
var v$271 = v$270[0];
switch (v$271) { case 46: {var v$285 = v$270[1];
var v$277 = getc$155(v$285);
switch (v$277[0]) { case 1: {return [1];
 break; }default: {var v$282 = v$277[1];
var v$283 = v$282[0];
var v$284 = v$282[1];
if ((48 <= v$283)?(v$283 <= 57):false) {var fix$637 = {};
fix$637.$frac = function(decs$576,fracv$577,src$578){lab$frac: while (true) {if (decs$576 >= 7) {var t$639;
var t$638;
var usecs$582 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$576))) * fracv$577)) + 5),10,CompilerInitial.exn$Div$45);
t$638 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((0.0 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$582,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$582,1000000,CompilerInitial.exn$Div$45)];
t$639 = [t$638,skipdigs$190(src$578)];
return [0,t$639];
} else {var v$583 = getc$155(src$578);
switch (v$583[0]) { case 1: {var t$641;
var t$640;
var usecs$587 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$576))) * fracv$577)) + 5),10,CompilerInitial.exn$Div$45);
t$640 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((0.0 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$587,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$587,1000000,CompilerInitial.exn$Div$45)];
t$641 = [t$640,src$578];
return [0,t$641];
 break; }default: {var v$588 = v$583[1];
var v$589 = v$588[0];
var v$590 = v$588[1];
if ((48 <= v$589)?(v$589 <= 57):false) {var t$644 = SmlPrims.chk_ovf_i32(decs$576 + 1);
var t$645 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(10 * fracv$577)) + (SmlPrims.chk_ovf_i32(v$589 - 48)));
var t$646 = v$590;
var decs$576 = t$644;
var fracv$577 = t$645;
var src$578 = t$646;
continue lab$frac;
} else {var t$643;
var t$642;
var usecs$595 = SmlPrims.div_i32(SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32((pow10$170(SmlPrims.chk_ovf_i32(7 - decs$576))) * fracv$577)) + 5),10,CompilerInitial.exn$Div$45);
t$642 = [SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(Math.floor((0.0 + (-2147483648)) + 0.5))) + (SmlPrims.div_i32(usecs$595,1000000,CompilerInitial.exn$Div$45))),SmlPrims.mod_i32(usecs$595,1000000,CompilerInitial.exn$Div$45)];
t$643 = [t$642,src$578];
return [0,t$643];
};
} };
};
};
};
var frac$575 = fix$637.$frac;
var v$611 = SmlPrims.chk_ovf_i32(v$283 - 48);
return frac$575(1,v$611,v$284);
} else {return [1];
};
} };
 break; }default: {var v$290 = v$270[1];
if ((48 <= v$271)?(v$271 <= 57):false) {return intg$237(SmlPrims.chk_ovf_i32(v$271 - 48),v$290);
} else {return [1];
};
} };
} };
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.fromString$291 = function(s$294){return EnFzYQm6gMCyrmnEKObeWe$3basis$0StringCvt$1.scanString$161(function(v$613){return function(v$614){return g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.scan$152(v$613,v$614);
};
},s$294);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$f$295 = function(v$303,v$601){var v$304 = v$303[0];
var v$305 = v$303[1];
var v$307 = v$601[0];
var v$308 = v$601[1];
var usecs$302 = SmlPrims.chk_ovf_i32(v$305 + v$308);
return [SmlPrims.chk_ovf_i32(SmlPrims.trunc(((v$304 - (-2147483648)) + v$307) + (SmlPrims.div_i32(usecs$302,1000000,CompilerInitial.exn$Div$45)))),SmlPrims.mod_i32(usecs$302,1000000,CompilerInitial.exn$Div$45)];
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$g$309 = function(v$325,v$602){var v$326 = v$325[0];
var v$327 = v$325[1];
var v$329 = v$602[0];
var v$330 = v$602[1];
try {var usecs$316 = SmlPrims.chk_ovf_i32(v$327 - v$330);
var secs$317 = SmlPrims.chk_ovf_i32((SmlPrims.chk_ovf_i32(v$326 - v$329)) + (SmlPrims.div_i32(usecs$316,1000000,CompilerInitial.exn$Div$45)));
if (secs$317 < 0) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {return [SmlPrims.chk_ovf_i32(secs$317 + (-2147483648)),SmlPrims.mod_i32(usecs$316,1000000,CompilerInitial.exn$Div$45)];
};
} catch(v$647) {return (function(Overflow$324){var t$648 = Overflow$324;
if (t$648[0] == CompilerInitial.en$Overflow$48) {throw g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.exn$Time$57;
} else {throw Overflow$324;
};
})(v$647);
};
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$j$331 = function(v$346,v$603){var v$347 = v$346[0];
var v$348 = v$346[1];
var v$350 = v$603[0];
var v$351 = v$603[1];
return (v$347 < v$350)?true:((v$347 == v$350)?(v$348 < v$351):false);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$jk$352 = function(v$367,v$604){var v$368 = v$367[0];
var v$369 = v$367[1];
var v$371 = v$604[0];
var v$372 = v$604[1];
return (v$368 < v$371)?true:((v$368 == v$371)?(v$369 <= v$372):false);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$l$373 = function(v$388,v$605){var v$389 = v$388[0];
var v$390 = v$388[1];
var v$392 = v$605[0];
var v$393 = v$605[1];
return (v$389 > v$392)?true:((v$389 == v$392)?(v$390 > v$393):false);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.s$lk$394 = function(v$409,v$606){var v$410 = v$409[0];
var v$411 = v$409[1];
var v$413 = v$606[0];
var v$414 = v$606[1];
return (v$410 > v$413)?true:((v$410 == v$413)?(v$411 >= v$414):false);
};
g14hFoVCUPM3ZC3WsORS6h$3basis$0Time$1.compare$415 = function(v$428,v$429){var t$650;
var v$530 = v$428[0];
var v$531 = v$428[1];
var v$533 = v$429[0];
var v$534 = v$429[1];
t$650 = ((v$530 < v$533)?true:((v$530 == v$533)?(v$531 < v$534):false));
if (t$650) {return 0;
} else {var t$649;
var v$537 = v$428[0];
var v$538 = v$428[1];
var v$540 = v$429[0];
var v$541 = v$429[1];
t$649 = ((v$537 > v$540)?true:((v$537 == v$540)?(v$538 > v$541):false));
return t$649?1:2;
};
};
return 0;
})();
