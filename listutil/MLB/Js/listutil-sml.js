if ((typeof(listutil$0listutil$1)) == "undefined") {listutil$0listutil$1 = {};
};
(function(){listutil$0listutil$1.filtermap$89 = function(f$92,xs$95){var fix$1699 = {};
fix$1699.$filtermap0 = function(v$1616,v$1617){lab$filtermap0: while (true) {if (v$1616 == null) {return basis$0List$1.rev$257(v$1617);
} else {var v$1618 = v$1616;
var v$1619 = v$1618[0];
var v$1620 = v$1618[1];
var t$1701 = v$1620;
var t$1700;
var v$1621 = f$92(v$1619);
switch (v$1621[0]) { case 0: {t$1700 = [v$1621[1],v$1617];
 break; }default: {t$1700 = v$1617;
} };
var t$1702 = t$1701;
var t$1703 = t$1700;
var v$1616 = t$1702;
var v$1617 = t$1703;
continue lab$filtermap0;
};
};
};
var filtermap0$1615 = fix$1699.$filtermap0;
var v$1665 = null;
return filtermap0$1615(xs$95,v$1665);
};
var fix$1704 = {};
fix$1704.$findKey = function(v$1180,v$99,v$102){lab$findKey: while (true) {if (v$99 == null) {return [1];
} else {var v$118 = v$99;
var v$119 = v$118[0];
var v$120 = v$119[0];
var v$121 = v$119[1];
var v$122 = v$118[1];
if (v$1180([v$102,v$120])) {return [0,v$121];
} else {var t$1705 = v$1180;
var t$1706 = v$122;
var t$1707 = v$102;
var v$1180 = t$1705;
var v$99 = t$1706;
var v$102 = t$1707;
continue lab$findKey;
};
};
};
};
listutil$0listutil$1.findKey$96 = fix$1704.$findKey;
listutil$0listutil$1.appi$124 = function(f$127,xs$130){var fix$1708 = {};
fix$1708.$ai = function(v$1624,v$1625){lab$ai: while (true) {if (v$1625 == null) {return 0;
} else {var v$1626 = v$1625;
var v$1627 = v$1626[0];
var v$1628 = v$1626[1];
f$127([v$1627,v$1624]);
var t$1709 = SmlPrims.chk_ovf_i32(v$1624 + 1);
var t$1710 = v$1628;
var v$1624 = t$1709;
var v$1625 = t$1710;
continue lab$ai;
};
};
};
var ai$1623 = fix$1708.$ai;
return ai$1623(0,xs$130);
};
listutil$0listutil$1.mapi$161 = function(f$164,xs$167){var fix$1711 = {};
fix$1711.$mi = function(v$1631,v$1632,v$1633){lab$mi: while (true) {if (v$1632 == null) {return basis$0List$1.rev$682(v$1633);
} else {var v$1634 = v$1632;
var v$1635 = v$1634[0];
var v$1636 = v$1634[1];
var t$1712 = SmlPrims.chk_ovf_i32(v$1631 + 1);
var t$1713 = v$1636;
var t$1714 = [f$164([v$1635,v$1631]),v$1633];
var v$1631 = t$1712;
var v$1632 = t$1713;
var v$1633 = t$1714;
continue lab$mi;
};
};
};
var mi$1630 = fix$1711.$mi;
var v$1670 = null;
return mi$1630(0,xs$167,v$1670);
};
listutil$0listutil$1.map$203 = function(f$206,xs$209){var fix$1715 = {};
fix$1715.$mymap0 = function(v$222,v$1658){lab$mymap0: while (true) {if (v$222 == null) {return v$1658;
} else {var v$225 = v$222;
var v$226 = v$225[0];
var v$227 = v$225[1];
var y$224 = f$206(v$226);
var t$1716 = v$227;
var t$1717 = [y$224,v$1658];
var v$222 = t$1716;
var v$1658 = t$1717;
continue lab$mymap0;
};
};
};
var mymap0$210 = fix$1715.$mymap0;
return basis$0List$1.rev$682(mymap0$210(xs$209,null));
};
listutil$0listutil$1.mapPartial$229 = function(f$232){var fix$1718 = {};
fix$1718.$aux = function(v$236,v$239){lab$aux: while (true) {if (v$239 == null) {return v$236;
} else {var v$258 = v$239;
var v$259 = v$258[0];
var v$260 = v$258[1];
var v$254 = f$232(v$259);
switch (v$254[0]) { case 0: {var v$256 = v$254[1];
var t$1719 = [v$256,v$236];
var t$1720 = v$260;
var v$236 = t$1719;
var v$239 = t$1720;
continue lab$aux;
 break; }default: {var t$1721 = v$236;
var t$1722 = v$260;
var v$236 = t$1721;
var v$239 = t$1722;
continue lab$aux;
} };
};
};
};
var aux$233 = fix$1718.$aux;
var v$1487;
var v$1671 = null;
v$1487 = (function(v$1672){return aux$233(v$1671,v$1672);
});
return function(x$1202){return basis$0List$1.rev$682(v$1487(x$1202));
};
};
var fix$1723 = {};
fix$1723.$contains = function(v$1181,v$264){lab$contains: while (true) {var v$272 = v$264[0];
if (v$272 == null) {return false;
} else {var v$277 = v$272;
var v$278 = v$277[0];
var v$279 = v$277[1];
var v$280 = v$264[1];
if (v$1181([v$278,v$280])) {return true;
} else {var t$1724 = v$1181;
var t$1725 = [v$279,v$280];
var v$1181 = t$1724;
var v$264 = t$1725;
continue lab$contains;
};
};
};
};
listutil$0listutil$1.contains$261 = fix$1723.$contains;
listutil$0listutil$1.dedup$281 = function(v$1182,xs$284){var fix$1726 = {};
fix$1726.$aux = function(v$1226,v$1228){lab$aux: while (true) {if (v$1228 == null) {return v$1226;
} else {var v$1235 = v$1228;
var v$1236 = v$1235[0];
var v$1237 = v$1235[1];
var t$1729;
var fix$1727 = {};
fix$1727.$exists = function(v$1248){lab$exists: while (true) {if (v$1248 == null) {return false;
} else {var v$1249 = v$1248;
var v$1250 = v$1249[0];
var v$1251 = v$1249[1];
if (v$1182([v$1236,v$1250])) {return true;
} else {var t$1728 = v$1251;
var v$1248 = t$1728;
continue lab$exists;
};
};
};
};
var exists$1247 = fix$1727.$exists;
t$1729 = (exists$1247(v$1226));
if (t$1729) {var t$1730 = v$1226;
var t$1731 = v$1237;
var v$1226 = t$1730;
var v$1228 = t$1731;
continue lab$aux;
} else {var t$1732 = [v$1236,v$1226];
var t$1733 = v$1237;
var v$1226 = t$1732;
var v$1228 = t$1733;
continue lab$aux;
};
};
};
};
var aux$1225 = fix$1726.$aux;
var v$1673 = null;
return aux$1225(v$1673,xs$284);
};
listutil$0listutil$1.dedupBy$313 = function(v$1184,f$316){var fix$1734 = {};
fix$1734.$aux = function(v$320,v$323){lab$aux: while (true) {if (v$323 == null) {var fix$1735 = {};
fix$1735.$mymap0 = function(v$1265,v$1659){lab$mymap0: while (true) {if (v$1265 == null) {return v$1659;
} else {var v$1266 = v$1265;
var v$1267 = v$1266[0];
var v$1268 = v$1266[1];
var y$1270 = v$1267[1];
var t$1736 = v$1268;
var t$1737 = [y$1270,v$1659];
var v$1265 = t$1736;
var v$1659 = t$1737;
continue lab$mymap0;
};
};
};
var mymap0$1263 = fix$1735.$mymap0;
return basis$0List$1.rev$682(mymap0$1263(v$320,null));
} else {var v$348 = v$323;
var v$349 = v$348[0];
var v$350 = v$348[1];
var x$$338 = f$316(v$349);
var t$1741;
var fix$1738 = {};
fix$1738.$exists = function(v$1276){lab$exists: while (true) {if (v$1276 == null) {return false;
} else {var v$1277 = v$1276;
var v$1278 = v$1277[0];
var v$1279 = v$1277[1];
var t$1739;
var v$1285 = v$1278[0];
t$1739 = (v$1184([x$$338,v$1285]));
if (t$1739) {return true;
} else {var t$1740 = v$1279;
var v$1276 = t$1740;
continue lab$exists;
};
};
};
};
var exists$1275 = fix$1738.$exists;
t$1741 = (exists$1275(v$320));
if (t$1741) {var t$1742 = v$320;
var t$1743 = v$350;
var v$320 = t$1742;
var v$323 = t$1743;
continue lab$aux;
} else {var t$1744 = [[x$$338,v$349],v$320];
var t$1745 = v$350;
var v$320 = t$1744;
var v$323 = t$1745;
continue lab$aux;
};
};
};
};
var aux$317 = fix$1734.$aux;
var v$1675 = null;
return function(v$1676){return aux$317(v$1675,v$1676);
};
};
var fix$1746 = {};
fix$1746.$lookup = function(v$1185,v$354,v$357,v$360){lab$lookup: while (true) {if (v$357 == null) {return v$354(0);
} else {var v$380 = v$357;
var v$381 = v$380[0];
var v$382 = v$381[0];
var v$383 = v$381[1];
var v$384 = v$380[1];
if (v$1185([v$382,v$360])) {return v$383;
} else {var t$1747 = v$1185;
var t$1748 = v$354;
var t$1749 = v$384;
var t$1750 = v$360;
var v$1185 = t$1747;
var v$354 = t$1748;
var v$357 = t$1749;
var v$360 = t$1750;
continue lab$lookup;
};
};
};
};
listutil$0listutil$1.lookup$351 = fix$1746.$lookup;
var fix$1751 = {};
fix$1751.$lookupOpt = function(v$1186,v$389,v$392){lab$lookupOpt: while (true) {if (v$389 == null) {return [1];
} else {var v$408 = v$389;
var v$409 = v$408[0];
var v$410 = v$409[0];
var v$411 = v$409[1];
var v$412 = v$408[1];
if (v$1186([v$410,v$392])) {return [0,v$411];
} else {var t$1752 = v$1186;
var t$1753 = v$412;
var t$1754 = v$392;
var v$1186 = t$1752;
var v$389 = t$1753;
var v$392 = t$1754;
continue lab$lookupOpt;
};
};
};
};
listutil$0listutil$1.lookupOpt$386 = fix$1751.$lookupOpt;
listutil$0listutil$1.replace$414 = function(v$1187,l$417,s$420,new$423){var fix$1755 = {};
fix$1755.$loop = function(v$427,v$430){lab$loop: while (true) {if (v$427 == null) {return [1];
} else {var v$446 = v$427;
var v$447 = v$446[0];
var v$448 = v$447[0];
var v$449 = v$447[1];
var v$450 = v$446[1];
if (v$1187([v$448,s$420])) {return [0,basis$0List$1.revAppend$261([new$423,v$430],v$450)];
} else {var t$1756 = v$450;
var t$1757 = [[v$448,v$449],v$430];
var v$427 = t$1756;
var v$430 = t$1757;
continue lab$loop;
};
};
};
};
var loop$424 = fix$1755.$loop;
return loop$424(l$417,null);
};
listutil$0listutil$1.removeOpt$452 = function(v$1188,l$455,key$458){var fix$1758 = {};
fix$1758.$loop = function(v$462,v$465){lab$loop: while (true) {if (v$462 == null) {return [1];
} else {var v$481 = v$462;
var v$482 = v$481[0];
var v$483 = v$482[0];
var v$484 = v$482[1];
var v$485 = v$481[1];
if (v$1188([v$483,key$458])) {return [0,basis$0List$1.revAppend$261(v$465,v$485)];
} else {var t$1759 = v$485;
var t$1760 = [[v$483,v$484],v$465];
var v$462 = t$1759;
var v$465 = t$1760;
continue lab$loop;
};
};
};
};
var loop$459 = fix$1758.$loop;
return loop$459(l$455,null);
};
listutil$0listutil$1.remove$487 = function(v$1189,l$490,key$493){var fix$1761 = {};
fix$1761.$loop = function(v$497,v$500){lab$loop: while (true) {if (v$497 == null) {return basis$0List$1.revAppend$261(v$500,null);
} else {var v$516 = v$497;
var v$517 = v$516[0];
var v$518 = v$517[0];
var v$519 = v$517[1];
var v$520 = v$516[1];
if (v$1189([v$518,key$493])) {return basis$0List$1.revAppend$261(v$500,v$520);
} else {var t$1762 = v$520;
var t$1763 = [[v$518,v$519],v$500];
var v$497 = t$1762;
var v$500 = t$1763;
continue lab$loop;
};
};
};
};
var loop$494 = fix$1761.$loop;
return loop$494(l$490,null);
};
var fix$1764 = {};
fix$1764.$chopN = function(N$525,ss$528,f$531){lab$chopN: while (true) {var fix$1765 = {};
fix$1765.$tk = function(v$535,v$538,v$541){lab$tk: while (true) {switch (v$535) { case 0: {return [basis$0List$1.rev$682(v$541),v$538];
 break; }default: {if (v$538 == null) {return [basis$0List$1.rev$682(v$541),null];
} else {var v$561 = v$538;
var v$562 = v$561[0];
var v$563 = v$561[1];
var t$1766 = SmlPrims.chk_ovf_i32(v$535 - 1);
var t$1767 = v$563;
var t$1768 = [v$562,v$541];
var v$535 = t$1766;
var v$538 = t$1767;
var v$541 = t$1768;
continue lab$tk;
};
} };
};
};
var tk$532 = fix$1765.$tk;
var v$574 = tk$532(N$525,ss$528,null);
var v$575 = v$574[0];
if (v$575 == null) {if (v$574[1] == null) {return 0;
} else {var v$1311 = v$574[0];
var v$1312 = v$574[1];
f$531(v$1311);
var t$1769 = N$525;
var t$1770 = v$1312;
var t$1771 = f$531;
var N$525 = t$1769;
var ss$528 = t$1770;
var f$531 = t$1771;
continue lab$chopN;
};
} else {if (v$574[1] == null) {return f$531(v$575);
} else {var v$1314 = v$574[0];
var v$1315 = v$574[1];
f$531(v$1314);
var t$1772 = N$525;
var t$1773 = v$1315;
var t$1774 = f$531;
var N$525 = t$1772;
var ss$528 = t$1773;
var f$531 = t$1774;
continue lab$chopN;
};
};
};
};
listutil$0listutil$1.chopN$522 = fix$1764.$chopN;
var fix$1775 = {};
fix$1775.$chopNacc = function(N$586,ss$589,a$592,f$595){lab$chopNacc: while (true) {var fix$1776 = {};
fix$1776.$tk = function(v$599,v$602,v$605){lab$tk: while (true) {switch (v$599) { case 0: {return [basis$0List$1.rev$682(v$605),v$602];
 break; }default: {if (v$602 == null) {return [basis$0List$1.rev$682(v$605),null];
} else {var v$625 = v$602;
var v$626 = v$625[0];
var v$627 = v$625[1];
var t$1777 = SmlPrims.chk_ovf_i32(v$599 - 1);
var t$1778 = v$627;
var t$1779 = [v$626,v$605];
var v$599 = t$1777;
var v$602 = t$1778;
var v$605 = t$1779;
continue lab$tk;
};
} };
};
};
var tk$596 = fix$1776.$tk;
var v$638 = tk$596(N$586,ss$589,null);
var v$639 = v$638[0];
if (v$639 == null) {if (v$638[1] == null) {return a$592;
} else {var v$1317 = v$638[0];
var v$1318 = v$638[1];
var t$1780 = N$586;
var t$1781 = v$1318;
var t$1782 = f$595([v$1317,a$592]);
var t$1783 = f$595;
var N$586 = t$1780;
var ss$589 = t$1781;
var a$592 = t$1782;
var f$595 = t$1783;
continue lab$chopNacc;
};
} else {if (v$638[1] == null) {return f$595([v$639,a$592]);
} else {var v$1320 = v$638[0];
var v$1321 = v$638[1];
var t$1784 = N$586;
var t$1785 = v$1321;
var t$1786 = f$595([v$1320,a$592]);
var t$1787 = f$595;
var N$586 = t$1784;
var ss$589 = t$1785;
var a$592 = t$1786;
var f$595 = t$1787;
continue lab$chopNacc;
};
};
};
};
listutil$0listutil$1.chopNacc$583 = fix$1775.$chopNacc;
listutil$0listutil$1.cross$645 = function(f$648,xs$651){var t$1795 = basis$0List$1.concat$296;
var t$1794;
var fix$1788 = {};
fix$1788.$mymap0 = function(v$1326,v$1660){lab$mymap0: while (true) {if (v$1326 == null) {return v$1660;
} else {var v$1327 = v$1326;
var v$1328 = v$1327[0];
var v$1329 = v$1327[1];
var y$1331;
var fix$1789 = {};
fix$1789.$mymap0 = function(v$1574,v$1661){lab$mymap0: while (true) {if (v$1574 == null) {return v$1661;
} else {var v$1575 = v$1574;
var v$1576 = v$1575[0];
var v$1577 = v$1575[1];
var y$1579 = f$648([v$1328,v$1576]);
var t$1790 = v$1577;
var t$1791 = [y$1579,v$1661];
var v$1574 = t$1790;
var v$1661 = t$1791;
continue lab$mymap0;
};
};
};
var mymap0$1572 = fix$1789.$mymap0;
y$1331 = (basis$0List$1.rev$682(mymap0$1572(xs$651,null)));
var t$1792 = v$1329;
var t$1793 = [y$1331,v$1660];
var v$1326 = t$1792;
var v$1660 = t$1793;
continue lab$mymap0;
};
};
};
var mymap0$1324 = fix$1788.$mymap0;
t$1794 = (basis$0List$1.rev$682(mymap0$1324(xs$651,null)));
return t$1795(t$1794);
};
listutil$0listutil$1.mapPrev$658 = function(v$661,v$664){if (v$664 == null) {return null;
} else {var v$675 = v$664;
var v$677 = v$675[1];
return basis$0ListPair$1.map$169(v$661,[v$677,v$664]);
};
};
var fix$1796 = {};
fix$1796.$dropWhile = function(v$710,v$713){lab$dropWhile: while (true) {if (v$713 == null) {return null;
} else {var v$729 = v$713;
var v$730 = v$729[0];
var v$731 = v$729[1];
if (v$710(v$730)) {var t$1797 = v$710;
var t$1798 = v$731;
var v$710 = t$1797;
var v$713 = t$1798;
continue lab$dropWhile;
} else {return [v$730,v$731];
};
};
};
};
listutil$0listutil$1.dropWhile$707 = fix$1796.$dropWhile;
listutil$0listutil$1.takeWhile$732 = function(p$735){var fix$1799 = {};
fix$1799.$takeWhile$ = function(v$739,v$742){lab$takeWhile$: while (true) {if (v$742 == null) {return basis$0List$1.rev$682(v$739);
} else {var v$758 = v$742;
var v$759 = v$758[0];
var v$760 = v$758[1];
if (p$735(v$759)) {var t$1800 = [v$759,v$739];
var t$1801 = v$760;
var v$739 = t$1800;
var v$742 = t$1801;
continue lab$takeWhile$;
} else {return basis$0List$1.rev$682(v$739);
};
};
};
};
var takeWhile$$736 = fix$1799.$takeWhile$;
var v$1677 = null;
return function(v$1678){return takeWhile$$736(v$1677,v$1678);
};
};
listutil$0listutil$1.splitAt$761 = function(p$764){var fix$1802 = {};
fix$1802.$splitAt$ = function(v$768,v$771){lab$splitAt$: while (true) {if (v$771 == null) {return [basis$0List$1.rev$682(v$768),null];
} else {var v$787 = v$771;
var v$788 = v$787[0];
var v$789 = v$787[1];
if (p$764(v$788)) {return [basis$0List$1.rev$682(v$768),[v$788,v$789]];
} else {var t$1803 = [v$788,v$768];
var t$1804 = v$789;
var v$768 = t$1803;
var v$771 = t$1804;
continue lab$splitAt$;
};
};
};
};
var splitAt$$765 = fix$1802.$splitAt$;
var v$1679 = null;
return function(v$1680){return splitAt$$765(v$1679,v$1680);
};
};
listutil$0listutil$1.groupBy$790 = function(v$1190,discriminator$793){var fix$1805 = {};
fix$1805.$aux = function(v$825,v$828){lab$aux: while (true) {if (v$828 == null) {return v$825;
} else {var v$840 = v$828;
var v$841 = v$840[0];
var v$842 = v$840[1];
var t$1807;
var fix$1806 = {};
fix$1806.$insert = function(v$1639){if (v$1639 == null) {return [[discriminator$793(v$841),[v$841,null]],null];
} else {var v$1640 = v$1639;
var v$1641 = v$1640[0];
var v$1642 = v$1641[0];
var v$1643 = v$1641[1];
var v$1644 = v$1640[1];
var t$1808;
var v$1645 = discriminator$793(v$841);
t$1808 = (v$1190([v$1645,v$1642]));
return t$1808?[[v$1642,[v$841,v$1643]],v$1644]:[[v$1642,v$1643],fix$1806.$insert(v$1644)];
};
};
var insert$1638 = fix$1806.$insert;
t$1807 = (insert$1638(v$825));
var t$1809 = t$1807;
var t$1810 = v$842;
var v$825 = t$1809;
var v$828 = t$1810;
continue lab$aux;
};
};
};
var aux$822 = fix$1805.$aux;
var v$1681 = null;
return function(v$1682){return aux$822(v$1681,v$1682);
};
};
listutil$0listutil$1.groupBy$$843 = function(v$1191,discriminator$846){var fix$1811 = {};
fix$1811.$aux = function(v$890,v$893){lab$aux: while (true) {if (v$893 == null) {return v$890;
} else {var v$905 = v$893;
var v$906 = v$905[0];
var v$907 = v$905[1];
var t$1813;
var fix$1812 = {};
fix$1812.$insert = function(v$1648){if (v$1648 == null) {var v$1649 = discriminator$846(v$906);
return [[v$1649[0],[v$1649[1],null]],null];
} else {var v$1650 = v$1648;
var v$1651 = v$1650[0];
var v$1652 = v$1651[0];
var v$1653 = v$1651[1];
var v$1654 = v$1650[1];
var v$1655 = discriminator$846(v$906);
var v$1656 = v$1655[0];
var v$1657 = v$1655[1];
return (v$1191([v$1656,v$1652]))?[[v$1652,[v$1657,v$1653]],v$1654]:[[v$1652,v$1653],fix$1812.$insert(v$1654)];
};
};
var insert$1647 = fix$1812.$insert;
t$1813 = (insert$1647(v$890));
var t$1814 = t$1813;
var t$1815 = v$907;
var v$890 = t$1814;
var v$893 = t$1815;
continue lab$aux;
};
};
};
var aux$887 = fix$1811.$aux;
var v$1683 = null;
return function(v$1684){return aux$887(v$1683,v$1684);
};
};
listutil$0listutil$1.locate$908 = function(v$1192,elem$911){var fix$1816 = {};
fix$1816.$aux = function(v$915,v$918){lab$aux: while (true) {if (v$918 == null) {return [1];
} else {var v$932 = v$918;
var v$933 = v$932[0];
var v$934 = v$932[1];
if (v$1192([v$933,elem$911])) {return [0,v$915];
} else {var t$1817 = SmlPrims.chk_ovf_i32(v$915 + 1);
var t$1818 = v$934;
var v$915 = t$1817;
var v$918 = t$1818;
continue lab$aux;
};
};
};
};
var aux$912 = fix$1816.$aux;
return function(v$1686){return aux$912(0,v$1686);
};
};
var fix$1819 = {};
fix$1819.$mem = function(v$1193,v$938,v$941){lab$mem: while (true) {if (v$941 == null) {return false;
} else {var v$957 = v$941;
var v$958 = v$957[0];
var v$959 = v$957[1];
if (v$1193([v$938,v$958])) {return true;
} else {var t$1820 = v$1193;
var t$1821 = v$938;
var t$1822 = v$959;
var v$1193 = t$1820;
var v$938 = t$1821;
var v$941 = t$1822;
continue lab$mem;
};
};
};
};
listutil$0listutil$1.mem$935 = fix$1819.$mem;
listutil$0listutil$1.listToString$960 = function(xs$963){return ("[" + (basis$0String$1.concatWith$182(", ",xs$963))) + "]";
};
listutil$0listutil$1.update$964 = function(v$1194,v$968){var v$979 = v$968[0];
var v$980 = v$968[1];
return function(xs$971){var v$976;
var new$1584 = [v$979,v$980];
var fix$1823 = {};
fix$1823.$loop = function(v$1586,v$1587){lab$loop: while (true) {if (v$1586 == null) {return [1];
} else {var v$1588 = v$1586;
var v$1589 = v$1588[0];
var v$1590 = v$1589[0];
var v$1591 = v$1589[1];
var v$1592 = v$1588[1];
if (v$1194([v$1590,v$979])) {return [0,basis$0List$1.revAppend$261([new$1584,v$1587],v$1592)];
} else {var t$1824 = v$1592;
var t$1825 = [[v$1590,v$1591],v$1587];
var v$1586 = t$1824;
var v$1587 = t$1825;
continue lab$loop;
};
};
};
};
var loop$1585 = fix$1823.$loop;
v$976 = (loop$1585(xs$971,null));
switch (v$976[0]) { case 0: {return v$976[1];
 break; }default: {return [[v$979,v$980],xs$971];
} };
};
};
listutil$0listutil$1.last$981 = function(xs$984){try {return [0,basis$0List$1.last$90(xs$984)];
} catch(v$1826) {return (function(Empty$987){var t$1827 = Empty$987;
if (t$1827[0] == basis$0List$1.en$Empty$55) {return [1];
} else {throw Empty$987;
};
})(v$1826);
};
};
listutil$0listutil$1.head$988 = function(v$991){if (v$991 == null) {return [1];
} else {var v$997 = v$991;
var v$998 = v$997[0];
return [0,v$998];
};
};
listutil$0listutil$1.maximumBy$999 = function(v$1002,v$1005){if (v$1005 == null) {return [1];
} else {var v$1027 = v$1005;
var v$1028 = v$1027[0];
var v$1029 = v$1027[1];
var t$1832;
var fix$1828 = {};
fix$1828.$foldl = function(v$1364,v$1365){lab$foldl: while (true) {if (v$1365 == null) {return v$1364;
} else {var v$1366 = v$1365;
var v$1367 = v$1366[0];
var v$1368 = v$1366[1];
var t$1829;
switch (v$1002([v$1367,v$1364])) { case 1: {t$1829 = v$1367;
 break; }default: {t$1829 = v$1364;
} };
var t$1830 = t$1829;
var t$1831 = v$1368;
var v$1364 = t$1830;
var v$1365 = t$1831;
continue lab$foldl;
};
};
};
var foldl$1363 = fix$1828.$foldl;
t$1832 = (foldl$1363(v$1028,v$1029));
return [0,t$1832];
};
};
listutil$0listutil$1.concatMap$1030 = function(f$1033,xs$1036){var t$1841 = basis$0List$1.rev$682;
var t$1833;
var b$1370 = null;
var fix$1834 = {};
fix$1834.$foldl = function(v$1373,v$1374){lab$foldl: while (true) {if (v$1374 == null) {return v$1373;
} else {var v$1375 = v$1374;
var v$1376 = v$1375[0];
var v$1377 = v$1375[1];
var t$1835;
var c$1600 = f$1033(v$1376);
var fix$1836 = {};
fix$1836.$foldl = function(v$1602,v$1603){lab$foldl: while (true) {if (v$1603 == null) {return v$1602;
} else {var v$1604 = v$1603;
var v$1605 = v$1604[0];
var v$1606 = v$1604[1];
var t$1837 = [v$1605,v$1602];
var t$1838 = v$1606;
var v$1602 = t$1837;
var v$1603 = t$1838;
continue lab$foldl;
};
};
};
var foldl$1601 = fix$1836.$foldl;
t$1835 = (foldl$1601(v$1373,c$1600));
var t$1839 = t$1835;
var t$1840 = v$1377;
var v$1373 = t$1839;
var v$1374 = t$1840;
continue lab$foldl;
};
};
};
var foldl$1372 = fix$1834.$foldl;
t$1833 = (foldl$1372(b$1370,xs$1036));
return t$1841(t$1833);
};
var fix$1842 = {};
fix$1842.$nth = function(v$1057,v$1662){lab$nth: while (true) {if (v$1057 == null) {return [1];
} else {switch (v$1662) { case 0: {var v$1059 = v$1057;
var v$1060 = v$1059[0];
return [0,v$1060];
 break; }default: {var v$1061 = v$1057;
var v$1062 = v$1061[1];
var t$1843 = v$1062;
var t$1844 = SmlPrims.chk_ovf_i32(v$1662 - 1);
var v$1057 = t$1843;
var v$1662 = t$1844;
continue lab$nth;
} };
};
};
};
listutil$0listutil$1.nth$1044 = fix$1842.$nth;
listutil$0listutil$1.withPct$1063 = function(N$1066,data$1069){var total$1070;
var fix$1845 = {};
fix$1845.$foldl = function(v$1395,v$1396){lab$foldl: while (true) {if (v$1396 == null) {return v$1395;
} else {var v$1397 = v$1396;
var v$1398 = v$1397[0];
var v$1399 = v$1397[1];
var t$1846;
var v$1560 = v$1395;
t$1846 = ((Math.abs(v$1398[1])) + v$1560);
var t$1847 = t$1846;
var t$1848 = v$1399;
var v$1395 = t$1847;
var v$1396 = t$1848;
continue lab$foldl;
};
};
};
var foldl$1394 = fix$1845.$foldl;
total$1070 = (foldl$1394(0.0,data$1069));
if (basis$0Real$1.s$kk$547(total$1070,0.0)) {return null;
} else {var data$1082;
var fix$1849 = {};
fix$1849.$mymap0 = function(v$1409,v$1663){lab$mymap0: while (true) {if (v$1409 == null) {return v$1663;
} else {var v$1410 = v$1409;
var v$1411 = v$1410[0];
var v$1412 = v$1410[1];
var v$1608 = v$1411[0];
var v$1609 = v$1411[1];
var y$1414 = [v$1608,v$1609,SmlPrims.chk_ovf_i32(Math.floor(((Math.abs(v$1609)) * N$1066) / total$1070))];
var t$1850 = v$1412;
var t$1851 = [y$1414,v$1663];
var v$1409 = t$1850;
var v$1663 = t$1851;
continue lab$mymap0;
};
};
};
var mymap0$1407 = fix$1849.$mymap0;
data$1082 = (basis$0List$1.rev$682(mymap0$1407(data$1069,null)));
var total2$1089;
var fix$1852 = {};
fix$1852.$foldl = function(v$1427,v$1428){lab$foldl: while (true) {if (v$1428 == null) {return v$1427;
} else {var v$1429 = v$1428;
var v$1430 = v$1429[0];
var v$1431 = v$1429[1];
var t$1853 = SmlPrims.chk_ovf_i32(v$1430[2] + v$1427);
var t$1854 = v$1431;
var v$1427 = t$1853;
var v$1428 = t$1854;
continue lab$foldl;
};
};
};
var foldl$1426 = fix$1852.$foldl;
total2$1089 = (foldl$1426(0,data$1082));
var to_distribute$1097 = SmlPrims.chk_ovf_i32(N$1066 - total2$1089);
var data$1098 = basis$0List$1.map$332(function(v$1103){var v$1104 = v$1103[0];
var v$1105 = v$1103[1];
var v$1106 = v$1103[2];
return [v$1104,v$1105,v$1106,(Math.abs(v$1105)) - v$1106];
},data$1082);
var data$1107 = listsort$0ListSort$1.sort$54(function(v$1111){var v$1120 = v$1111[0];
var v$1121 = v$1111[1];
return basis$0Real$1.compare$532(v$1120[3],v$1121[3]);
},data$1098);
var v$1132;
var fix$1855 = {};
fix$1855.$foldl = function(v$1453,v$1454){lab$foldl: while (true) {if (v$1454 == null) {return v$1453;
} else {var v$1455 = v$1454;
var v$1456 = v$1455[0];
var v$1457 = v$1455[1];
var t$1856;
var v$1460 = v$1456[0];
var v$1461 = v$1456[1];
var v$1462 = v$1456[2];
var v$1463 = v$1456[3];
var v$1465 = v$1453[0];
var v$1466 = v$1453[1];
t$1856 = ((v$1466 > 0)?[[[v$1460,v$1461,SmlPrims.chk_ovf_i32(v$1462 + 1),v$1463],v$1465],SmlPrims.chk_ovf_i32(v$1466 - 1)]:[[[v$1460,v$1461,v$1462,v$1463],v$1465],v$1466]);
var t$1857 = t$1856;
var t$1858 = v$1457;
var v$1453 = t$1857;
var v$1454 = t$1858;
continue lab$foldl;
};
};
};
var foldl$1452 = fix$1855.$foldl;
var v$1697 = [null,to_distribute$1097];
v$1132 = (foldl$1452(v$1697,data$1107));
var v$1133 = v$1132[0];
return basis$0List$1.map$332(function(v$1128){return [v$1128[0],v$1128[1],(100.0 * v$1128[2]) / N$1066];
},v$1133);
};
};
return 0;
})();