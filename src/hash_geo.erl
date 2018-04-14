%% @doc
%%   GeoHash https://en.wikipedia.org/wiki/Geohash
-module(hash_geo).

-export([encode/1, decode/1, proximity/1]).

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
   decode_base32_pad(<< <<(decode32(V)):5>> || <<V:8>> <= X >>).

decode_base32_pad(X)
 when bit_size(X) rem 2 =:= 0 ->
   X;
decode_base32_pad(X) ->
   <<X/bitstring, 0:1>>.


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

%%
%% Based on
%% https://github.com/chrisveness/latlon-geohash/blob/master/latlon-geohash.js
proximity(Hash) ->
   #{
      n  => adjacent_n(Hash),
      ne => adjacent_e(adjacent_n(Hash)),
      e  => adjacent_e(Hash),
      se => adjacent_e(adjacent_s(Hash)),
      s  => adjacent_s(Hash),
      sw => adjacent_w(adjacent_s(Hash)),
      w  => adjacent_w(Hash),
      nw => adjacent_w(adjacent_n(Hash))
   }.

adjacent_n(Hash) -> adjacent(fun is_edge_n/2, fun shift_n/2, Hash).
adjacent_s(Hash) -> adjacent(fun is_edge_s/2, fun shift_s/2, Hash).
adjacent_e(Hash) -> adjacent(fun is_edge_e/2, fun shift_e/2, Hash).
adjacent_w(Hash) -> adjacent(fun is_edge_w/2, fun shift_w/2, Hash).

adjacent(Edge, Shift, Hash) ->
   Size = size(Hash),
   Type = Size rem 2,
   Part = Size - 1,
   case Hash of
      <<Last:8>> ->
         <<(encode32(Shift(Type, Last)))>>;

      <<Prefix:Part/binary, Last:8>> ->
         case Edge(Type, Last) of
            true ->
               <<(adjacent(Edge, Shift, Prefix))/binary, (encode32(Shift(Type, Last)))>>;
            false ->
               <<Prefix/binary, (encode32(Shift(Type, Last)))>>
         end
   end.
   
%%
%% north
is_edge_n(0, $p) -> true;
is_edge_n(0, $r) -> true;
is_edge_n(0, $x) -> true;
is_edge_n(0, $z) -> true;

is_edge_n(1, $b) -> true;
is_edge_n(1, $c) -> true;
is_edge_n(1, $f) -> true;
is_edge_n(1, $g) -> true;
is_edge_n(1, $u) -> true;
is_edge_n(1, $v) -> true;
is_edge_n(1, $y) -> true;
is_edge_n(1, $z) -> true;
is_edge_n(_,  _) -> false.

shift_n(0, $p) -> 0;
shift_n(0, $0) -> 1;
shift_n(0, $r) -> 2;
shift_n(0, $2) -> 3;
shift_n(0, $1) -> 4;
shift_n(0, $4) -> 5;
shift_n(0, $3) -> 6;
shift_n(0, $6) -> 7;
shift_n(0, $x) -> 8;
shift_n(0, $8) -> 9;
shift_n(0, $z) ->10;
shift_n(0, $b) ->11;
shift_n(0, $9) ->12;
shift_n(0, $d) ->13;
shift_n(0, $c) ->14;
shift_n(0, $f) ->15;
shift_n(0, $5) ->16;
shift_n(0, $h) ->17;
shift_n(0, $7) ->18;
shift_n(0, $k) ->19;
shift_n(0, $j) ->20;
shift_n(0, $n) ->21;
shift_n(0, $m) ->22;
shift_n(0, $q) ->23;
shift_n(0, $e) ->24;
shift_n(0, $s) ->25;
shift_n(0, $g) ->26;
shift_n(0, $u) ->27;
shift_n(0, $t) ->28;
shift_n(0, $w) ->29;
shift_n(0, $v) ->30;
shift_n(0, $y) ->31;

shift_n(1, $b) -> 0;
shift_n(1, $c) -> 1;
shift_n(1, $0) -> 2;
shift_n(1, $1) -> 3;
shift_n(1, $f) -> 4;
shift_n(1, $g) -> 5;
shift_n(1, $4) -> 6;
shift_n(1, $5) -> 7;
shift_n(1, $2) -> 8;
shift_n(1, $3) -> 9;
shift_n(1, $8) ->10;
shift_n(1, $9) ->11;
shift_n(1, $6) ->12;
shift_n(1, $7) ->13;
shift_n(1, $d) ->14;
shift_n(1, $e) ->15;
shift_n(1, $u) ->16;
shift_n(1, $v) ->17;
shift_n(1, $h) ->18;
shift_n(1, $j) ->19;
shift_n(1, $y) ->20;
shift_n(1, $z) ->21;
shift_n(1, $n) ->22;
shift_n(1, $p) ->23;
shift_n(1, $k) ->24;
shift_n(1, $m) ->25;
shift_n(1, $s) ->26;
shift_n(1, $t) ->27;
shift_n(1, $q) ->28;
shift_n(1, $r) ->29;
shift_n(1, $w) ->30;
shift_n(1, $x) ->31.


%%
%% south
is_edge_s(0, $0) -> true;
is_edge_s(0, $2) -> true;
is_edge_s(0, $8) -> true;
is_edge_s(0, $b) -> true;

is_edge_s(1, $0) -> true;
is_edge_s(1, $1) -> true;
is_edge_s(1, $4) -> true;
is_edge_s(1, $5) -> true;
is_edge_s(1, $h) -> true;
is_edge_s(1, $j) -> true;
is_edge_s(1, $n) -> true;
is_edge_s(1, $p) -> true;
is_edge_s(_,  _) ->false.

shift_s(0, $1) -> 0;
shift_s(0, $4) -> 1;
shift_s(0, $3) -> 2;
shift_s(0, $6) -> 3;
shift_s(0, $5) -> 4;
shift_s(0, $h) -> 5;
shift_s(0, $7) -> 6;
shift_s(0, $k) -> 7;
shift_s(0, $9) -> 8;
shift_s(0, $d) -> 9;
shift_s(0, $c) ->10;
shift_s(0, $f) ->11;
shift_s(0, $e) ->12;
shift_s(0, $s) ->13;
shift_s(0, $g) ->14;
shift_s(0, $u) ->15;
shift_s(0, $j) ->16;
shift_s(0, $n) ->17;
shift_s(0, $m) ->18;
shift_s(0, $q) ->19;
shift_s(0, $p) ->20;
shift_s(0, $0) ->21;
shift_s(0, $r) ->22;
shift_s(0, $2) ->23;
shift_s(0, $t) ->24;
shift_s(0, $w) ->25;
shift_s(0, $v) ->26;
shift_s(0, $y) ->27;
shift_s(0, $x) ->28;
shift_s(0, $8) ->29;
shift_s(0, $z) ->30;
shift_s(0, $b) ->31;

shift_s(1, $2) -> 0;
shift_s(1, $3) -> 1;
shift_s(1, $8) -> 2;
shift_s(1, $9) -> 3;
shift_s(1, $6) -> 4;
shift_s(1, $7) -> 5;
shift_s(1, $d) -> 6;
shift_s(1, $e) -> 7;
shift_s(1, $b) -> 8;
shift_s(1, $c) -> 9;
shift_s(1, $0) ->10;
shift_s(1, $1) ->11;
shift_s(1, $f) ->12;
shift_s(1, $g) ->13;
shift_s(1, $4) ->14;
shift_s(1, $5) ->15;
shift_s(1, $k) ->16;
shift_s(1, $m) ->17;
shift_s(1, $s) ->18;
shift_s(1, $t) ->19;
shift_s(1, $q) ->20;
shift_s(1, $r) ->21;
shift_s(1, $w) ->22;
shift_s(1, $x) ->23;
shift_s(1, $u) ->24;
shift_s(1, $v) ->25;
shift_s(1, $h) ->26;
shift_s(1, $j) ->27;
shift_s(1, $y) ->28;
shift_s(1, $z) ->29;
shift_s(1, $n) ->30;
shift_s(1, $p) ->31.

%%
%% east
is_edge_e(0, $b) -> true;
is_edge_e(0, $c) -> true;
is_edge_e(0, $f) -> true;
is_edge_e(0, $g) -> true;
is_edge_e(0, $u) -> true;
is_edge_e(0, $v) -> true;
is_edge_e(0, $y) -> true;
is_edge_e(0, $z) -> true;

is_edge_e(1, $p) -> true;
is_edge_e(1, $r) -> true;
is_edge_e(1, $x) -> true;
is_edge_e(1, $z) -> true;
is_edge_e(_,  _) -> false.

shift_e(0, $b) -> 0;
shift_e(0, $c) -> 1;
shift_e(0, $0) -> 2;
shift_e(0, $1) -> 3;
shift_e(0, $f) -> 4;
shift_e(0, $g) -> 5;
shift_e(0, $4) -> 6;
shift_e(0, $5) -> 7;
shift_e(0, $2) -> 8;
shift_e(0, $3) -> 9;
shift_e(0, $8) ->10;
shift_e(0, $9) ->11;
shift_e(0, $6) ->12;
shift_e(0, $7) ->13;
shift_e(0, $d) ->14;
shift_e(0, $e) ->15;
shift_e(0, $u) ->16;
shift_e(0, $v) ->17;
shift_e(0, $h) ->18;
shift_e(0, $j) ->19;
shift_e(0, $y) ->20;
shift_e(0, $z) ->21;
shift_e(0, $n) ->22;
shift_e(0, $p) ->23;
shift_e(0, $k) ->24;
shift_e(0, $m) ->25;
shift_e(0, $s) ->26;
shift_e(0, $t) ->27;
shift_e(0, $q) ->28;
shift_e(0, $r) ->29;
shift_e(0, $w) ->30;
shift_e(0, $x) ->31;

shift_e(1, $p) -> 0;
shift_e(1, $0) -> 1;
shift_e(1, $r) -> 2;
shift_e(1, $2) -> 3;
shift_e(1, $1) -> 4;
shift_e(1, $4) -> 5;
shift_e(1, $3) -> 6;
shift_e(1, $6) -> 7;
shift_e(1, $x) -> 8;
shift_e(1, $8) -> 9;
shift_e(1, $z) ->10;
shift_e(1, $b) ->11;
shift_e(1, $9) ->12;
shift_e(1, $d) ->13;
shift_e(1, $c) ->14;
shift_e(1, $f) ->15;
shift_e(1, $5) ->16;
shift_e(1, $h) ->17;
shift_e(1, $7) ->18;
shift_e(1, $k) ->19;
shift_e(1, $j) ->20;
shift_e(1, $n) ->21;
shift_e(1, $m) ->22;
shift_e(1, $q) ->23;
shift_e(1, $e) ->24;
shift_e(1, $s) ->25;
shift_e(1, $g) ->26;
shift_e(1, $u) ->27;
shift_e(1, $t) ->28;
shift_e(1, $w) ->29;
shift_e(1, $v) ->30;
shift_e(1, $y) ->31.

%%
%% west
is_edge_w(0, $0) -> true;
is_edge_w(0, $1) -> true;
is_edge_w(0, $4) -> true;
is_edge_w(0, $5) -> true;
is_edge_w(0, $h) -> true;
is_edge_w(0, $j) -> true;
is_edge_w(0, $n) -> true;
is_edge_w(0, $p) -> true;

is_edge_w(1, $0) -> true;
is_edge_w(1, $2) -> true;
is_edge_w(1, $8) -> true;
is_edge_w(1, $b) -> true;
is_edge_w(_,  _) -> false.

shift_w(0, $2) -> 0;
shift_w(0, $3) -> 1;
shift_w(0, $8) -> 2;
shift_w(0, $9) -> 3;
shift_w(0, $6) -> 4;
shift_w(0, $7) -> 5;
shift_w(0, $d) -> 6;
shift_w(0, $e) -> 7;
shift_w(0, $b) -> 8;
shift_w(0, $c) -> 9;
shift_w(0, $0) ->10;
shift_w(0, $1) ->11;
shift_w(0, $f) ->12;
shift_w(0, $g) ->13;
shift_w(0, $4) ->14;
shift_w(0, $5) ->15;
shift_w(0, $k) ->16;
shift_w(0, $m) ->17;
shift_w(0, $s) ->18;
shift_w(0, $t) ->19;
shift_w(0, $q) ->20;
shift_w(0, $r) ->21;
shift_w(0, $w) ->22;
shift_w(0, $x) ->23;
shift_w(0, $u) ->24;
shift_w(0, $v) ->25;
shift_w(0, $h) ->26;
shift_w(0, $j) ->27;
shift_w(0, $y) ->28;
shift_w(0, $z) ->29;
shift_w(0, $n) ->30;
shift_w(0, $p) ->31;

shift_w(1, $1) -> 0;
shift_w(1, $4) -> 1;
shift_w(1, $3) -> 2;
shift_w(1, $6) -> 3;
shift_w(1, $5) -> 4;
shift_w(1, $h) -> 5;
shift_w(1, $7) -> 6;
shift_w(1, $k) -> 7;
shift_w(1, $9) -> 8;
shift_w(1, $d) -> 9;
shift_w(1, $c) ->10;
shift_w(1, $f) ->11;
shift_w(1, $e) ->12;
shift_w(1, $s) ->13;
shift_w(1, $g) ->14;
shift_w(1, $u) ->15;
shift_w(1, $j) ->16;
shift_w(1, $n) ->17;
shift_w(1, $m) ->18;
shift_w(1, $q) ->19;
shift_w(1, $p) ->20;
shift_w(1, $0) ->21;
shift_w(1, $r) ->22;
shift_w(1, $2) ->23;
shift_w(1, $t) ->24;
shift_w(1, $w) ->25;
shift_w(1, $v) ->26;
shift_w(1, $y) ->27;
shift_w(1, $x) ->28;
shift_w(1, $8) ->29;
shift_w(1, $z) ->30;
shift_w(1, $b) ->31.
