%% @doc
%%   GeoHash https://en.wikipedia.org/wiki/Geohash
-module(hash_geo).

-export([encode/1, decode/1]).

%%
%% latitude grid table
-define(LAT, [
   90.0,
   45.0,
   22.5,
   11.25,
   5.625,
   2.8125,
   1.40625,
   0.703125,
   0.3515625,
   0.17578125,
   0.087890625,
   0.0439453125,
   0.02197265625,
   0.010986328125,
   0.0054931640625,
   0.00274658203125,
   0.001373291015625,
   6.866455078125e-4,
   3.4332275390625e-4,
   1.71661376953125e-4,
   8.58306884765625e-5,
   4.291534423828125e-5,
   2.1457672119140625e-5,
   1.0728836059570313e-5,
   5.364418029785156e-6,
   2.682209014892578e-6,
   1.341104507446289e-6,
   6.705522537231445e-7,
   3.3527612686157227e-7,
   1.6763806343078613e-7,
   8.381903171539307e-8,
   4.190951585769653e-8
]).

%%
%% longitude grid table
-define(LNG, [
   180.0,
   90.0,
   45.0,
   22.5,
   11.25,
   5.625,
   2.8125,
   1.40625,
   0.703125,
   0.3515625,
   0.17578125,
   0.087890625,
   0.0439453125,
   0.02197265625,
   0.010986328125,
   0.0054931640625,
   0.00274658203125,
   0.001373291015625,
   6.866455078125e-4,
   3.4332275390625e-4,
   1.71661376953125e-4,
   8.58306884765625e-5,
   4.291534423828125e-5,
   2.1457672119140625e-5,
   1.0728836059570313e-5,
   5.364418029785156e-6,
   2.682209014892578e-6,
   1.341104507446289e-6,
   6.705522537231445e-7,
   3.3527612686157227e-7,
   1.6763806343078613e-7,
   8.381903171539307e-8
]).

%%
%%
encode({Lat, Lng}) -> 
   Xlat = encode_geo(Lat, ?LAT,  -90,  90),
   Xlng = encode_geo(Lng, ?LNG, -180, 180),
   encode_base32(<< <<X:2>> || <<X:2>> <- zip(Xlng, Xlat) >>).    

zip([Ha | Ta], [Hb | Tb]) ->
   [<<Ha:1, Hb:1>> | zip(Ta, Tb)];
zip([], []) ->
   [].

%%
%%
decode(X) ->
   Bin = decode_base32(X),
   Lng = decode_geo([A || <<A:1, _:1>> <= Bin], ?LNG, -180, 180), 
   Lat = decode_geo([B || <<_:1, B:1>> <= Bin], ?LAT,  -90,  90), 
   {Lat, Lng}.   

%%
%%
decode_base32(X) ->
   << <<(decode32(V)):5>> || <<V:8>> <= X >>.   

decode32($0) ->  0;
decode32($1) ->  1;
decode32($2) ->  2;
decode32($3) ->  3;
decode32($4) ->  4;
decode32($5) ->  5;
decode32($6) ->  6;
decode32($7) ->  7;
decode32($8) ->  8;
decode32($9) ->  9;
decode32($b) -> 10;
decode32($c) -> 11;
decode32($d) -> 12;
decode32($e) -> 13;
decode32($f) -> 14;
decode32($g) -> 15;
decode32($h) -> 16;
decode32($j) -> 17;
decode32($k) -> 18;
decode32($m) -> 19;
decode32($n) -> 20;
decode32($p) -> 21;
decode32($q) -> 22;
decode32($r) -> 23;
decode32($s) -> 24;
decode32($t) -> 25;
decode32($u) -> 26;
decode32($v) -> 27;
decode32($w) -> 28;
decode32($x) -> 29;
decode32($y) -> 30;
decode32($z) -> 31.

%%
%%
encode_base32(X) ->
   << <<(encode32(V)):8>> || <<V:5>> <= X >>.   

encode32( 0) -> $0;
encode32( 1) -> $1;
encode32( 2) -> $2;
encode32( 3) -> $3;
encode32( 4) -> $4;
encode32( 5) -> $5;
encode32( 6) -> $6;
encode32( 7) -> $7;
encode32( 8) -> $8;
encode32( 9) -> $9;
encode32(10) -> $b;
encode32(11) -> $c;
encode32(12) -> $d;
encode32(13) -> $e;
encode32(14) -> $f;
encode32(15) -> $g;
encode32(16) -> $h;
encode32(17) -> $j;
encode32(18) -> $k;
encode32(19) -> $m;
encode32(20) -> $n;
encode32(21) -> $p;
encode32(22) -> $q;
encode32(23) -> $r;
encode32(24) -> $s;
encode32(25) -> $t;
encode32(26) -> $u;
encode32(27) -> $v;
encode32(28) -> $w;
encode32(29) -> $x;
encode32(30) -> $y;
encode32(31) -> $z.


%%
%%
decode_geo([1 | Tx], [M | Tm], A, B) ->
   decode_geo(Tx, Tm, A + M, B);

decode_geo([0 | Tx], [M | Tm], A, B) ->
   decode_geo(Tx, Tm, A, B - M);

decode_geo([], [M | _], A, _) ->
   A + M.

%%
%%
encode_geo(_, [_], _, _) ->
   [];
encode_geo(X, [M | Tm], A, B)
 when X >= A + M ->
   [1 | encode_geo(X, Tm, A + M, B)];
encode_geo(X, [M | Tm], A, B) ->
   [0 | encode_geo(X, Tm, A, B - M)].

