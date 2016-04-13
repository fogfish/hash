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

%%
%%  FNV32 initial state
-define(FNV32_PRIME, 16777619).
-define(FNV32_INIT,  2166136261).
-define(FNV32_MASK,  16#FFFFFFFF).

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


