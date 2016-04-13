%%
%%   Copyright 2014 Dmitry Kolesnikov, All Rights Reserved
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
%%   Cyclic polynomial (buzhash) - http://en.wikipedia.org/wiki/Rolling_hash
-module(hash_buz).

-export([buz32/1, buz32/2]).


-define(BUZ32_MASK,  16#FFFFFFFF).


%%
%% internal hash state
-record(hash, {
   value  = 0         :: integer(),    %% last hash value 
   k      = 0         :: integer(),    %% hash parameter K 
   n      = 0         :: integer(),    %% number of hashes at signature
   hashes = undefined :: queue:queue() %% signature
}).

%%
%% init hash state 
buz32(N) ->
   #hash{k = N rem 32, n = N, hashes = queue:new()}.

%%
%% calculate rolling hash
buz32(X, #hash{value = Hash0, k = K, n = 0, hashes = Hashes0} = State) ->
   {{value, HashK}, Hashes1} = queue:out(Hashes0),
   HashX = h32(X),
   Hash1 = s32(Hash0, 1) bxor s32(HashK, K) bxor HashX,
   {Hash1, State#hash{value = Hash1, hashes = queue:in(HashX, Hashes1)}};

buz32(X, #hash{value = Hash0, n = N, hashes = Hashes0} = State) ->
   HashX = h32(X),
   Hash1 = s32(Hash0, 1) bxor HashX,
   {Hash1, State#hash{value = Hash1, n = N - 1, hashes = queue:in(HashX, Hashes0)}}.

%%
%%
s32(Hash, K) ->
   ((Hash bsl K) bor (Hash bsr (32 - K)) ) band ?BUZ32_MASK.

h32(X)
 when is_integer(X), X < ?BUZ32_MASK ->
   X;
h32(X) ->
   erlang:phash2(X).

