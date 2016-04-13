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
%%   Additive congruential method of generating values in pseudo-random order by Roy Hann.
-module(hash_seq).

-export([seq31/1, seq32/1]).

%% Initially the shift register contains the value 0001. The two rightmost bits are XOR-ed, 
%% the result is fed into the leftmost bit position and previous register contents shift
%% one bit right. Choosing correct bits tap position is important. see E.J. 
%% Watson "Primitive Polynomials", Math of Computation
%%
%% Acceptable polynomials:
%% 
%%  8bit {0, 2, 3, 4}
%% 16bit {0, 2, 3, 5}
%% 31bit {0, 3}
%% 32bit {0, 1, 2, 3, 5, 7}
%% 64bit {0, 1, 3, 4}


seq31(N) ->
   (N bsr 1) bor ((((N bsr 3) bxor N) band 1) bsl 30).

seq32(N) ->
   (N bsr 1) bor ((((N bsr 7) bxor (N bsr 5) bxor (N bsr 3) bxor (N bsr 2) bxor (N bsr 1) bxor N) band 1) bsl 30).

