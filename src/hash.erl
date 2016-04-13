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
%%  @description
%%
-module(hash).

-export([fnv32/1, fnv32a/1, fnv32m/1]).
-export([seq31/1, seq32/1]).

%%
%% Fowler–Noll–Vo
fnv32(X)  -> hash_fnv:fnv32(s(X)).
fnv32a(X) -> hash_fnv:fnv32a(s(X)).
fnv32m(X) -> hash_fnv:fnv32m(s(X)).

%%
%% Uniformly distributed pseudo-random sequences
seq31(X) when is_integer(X) -> hash_seq:seq31(X).
seq32(X) when is_integer(X) -> hash_seq:seq32(X).


%%%----------------------------------------------------------------------------   
%%%
%%% private
%%%
%%%----------------------------------------------------------------------------   

s(X) when is_binary(X) -> X;
s(X) when is_list(X)   -> iolist_to_binary(X).
