%%
%%   Copyright 2012 Dmitry Kolesnikov, All Rights Reserved
%%
%%   Licensed under the Apache License, Version 2.0 (the "License");
%%   you may not use this file except in compliance with the License.
%%   You may obtain a copy of the License at
%%
%%       http://www.apache.org/licenses/LICENSE-2.0
%%
%%   Unless required by applicable law or agreed to in writing, software
%%   distributed under the License is distributed on an "AS IS" BASIS,
%%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%   See the License for the specific language governing permissions and
%%   limitations under the License.
%%
%% @doc
%%   https://en.wikipedia.org/wiki/Fowler–Noll–Vo_hash_function
-module(hash_fnv).

-export([fnv32/1, fnv32a/1, fnv32m/1]).
-export([fnv64/1, fnv64a/1]).
-export([fnv128/1, fnv128a/1]).

%%
%%  FNV32 initial state
-define(FNV32_PRIME, 16777619).
-define(FNV32_INIT,  2166136261).
-define(FNV32_MASK,  16#FFFFFFFF).

%%
%%  FNV64 initial state
-define(FNV64_PRIME, 1099511628211).
-define(FNV64_INIT, 14695981039346656037).
-define(FNV64_MASK, 16#FFFFFFFFFFFFFFFF).

%%
%%  FNV128 initial state
-define(FNV128_PRIME, 309485009821345068724781371).
-define(FNV128_INIT, 144066263297769815596495629667062367629).
-define(FNV128_MASK, 16#FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF).

%%
%%
fnv32(Data) ->
   fnv32(Data, ?FNV32_INIT).

fnv32(<<H:8, T/bytes>>, State) -> 
   Hash  = ( ( State * ?FNV32_PRIME ) band ?FNV32_MASK ) bxor H,
   fnv32(T, Hash);

fnv32(<<>>, State) -> 
   State.

%%
%%
fnv32a(Data) ->
   fnv32a(Data, ?FNV32_INIT).

fnv32a(<<H:8, T/bytes>>, State) -> 
   Hash  =  ( ( State bxor H ) * ?FNV32_PRIME ) band ?FNV32_MASK,
   fnv32a(T, Hash);

fnv32a(<<>>, State) -> 
   State.


%%
%% @see http://home.comcast.net/~bretm/hash/6.html (available via web archive)
fnv32m(Data) ->
   fnv32m(Data, ?FNV32_INIT).

fnv32m(<<H:8, T/bytes>>, State) -> 
   Hash  =  ( ( State bxor H ) * ?FNV32_PRIME ) band ?FNV32_MASK,
   fnv32m(T, Hash);

fnv32m(<<>>, State) -> 
   Hash1 = (State + (State bsl 13)) band ?FNV32_MASK,
   Hash2 = (Hash1 bxor (Hash1 bsr 7)) band ?FNV32_MASK,
   Hash3 = (Hash2 + (Hash2 bsl 3)) band ?FNV32_MASK,
   Hash4 = (Hash3 bxor (Hash3 bsr 17)) band ?FNV32_MASK,
   Hash5 = (Hash4 + (Hash4 bsl 5)) band ?FNV32_MASK,
   Hash5.

%%
%% @see http://www.isthe.com/chongo/tech/comp/fnv/
fnv64(Data) ->
   fnv64(Data, ?FNV64_INIT).

fnv64(<<H:8, T/bytes>>, State) ->
   Hash  = ( ( State * ?FNV64_PRIME ) band ?FNV64_MASK ) bxor H,
   fnv64(T, Hash);

fnv64(<<>>, State) ->
   State.

%%
%%
fnv64a(Data) ->
   fnv64a(Data, ?FNV64_INIT).

fnv64a(<<H:8, T/bytes>>, State) ->
   Hash  =  ( ( State bxor H ) * ?FNV64_PRIME ) band ?FNV64_MASK,
   fnv64a(T, Hash);

fnv64a(<<>>, State) ->
   State.

%%
%% @see http://www.isthe.com/chongo/tech/comp/fnv/
fnv128(Data) ->
   fnv128(Data, ?FNV128_INIT).

fnv128(<<H:8, T/bytes>>, State) ->
   Hash  = ( ( State * ?FNV128_PRIME ) band ?FNV128_MASK ) bxor H,
   fnv128(T, Hash);

fnv128(<<>>, State) ->
   State.

%%
%%
fnv128a(Data) ->
   fnv128a(Data, ?FNV128_INIT).

fnv128a(<<H:8, T/bytes>>, State) ->
   Hash  =  ( ( State bxor H ) * ?FNV128_PRIME ) band ?FNV128_MASK,
   fnv128a(T, Hash);

fnv128a(<<>>, State) ->
   State.
