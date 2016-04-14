%%
%%   Copyright 2016 Dmitry Kolesnikov, All Rights Reserved
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
-module(hash_SUITE).
-include_lib("common_test/include/ct.hrl").

%%
%% common test
-export([
   all/0
  ,groups/0
  ,init_per_suite/1
  ,end_per_suite/1
  ,init_per_group/2
  ,end_per_group/2
]).

%%
%% unit tests
-export([
   fnv/1, seq/1, fold/1, buz/1, pbkdf2/1
]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, hf}
   ].

groups() ->
   [
      {hf, [parallel], 
         [fnv, seq, fold, buz, pbkdf2]}
   ].

%%%----------------------------------------------------------------------------   
%%%
%%% init
%%%
%%%----------------------------------------------------------------------------   
init_per_suite(Config) ->
   Config.

end_per_suite(_Config) ->
   ok.

%% 
%%
init_per_group(_, Config) ->
   Config.

end_per_group(_, _Config) ->
   ok.

%%%----------------------------------------------------------------------------   
%%%
%%% unit tests
%%%
%%%----------------------------------------------------------------------------   

fnv(_Config) ->
   693272536  = hash:fnv32("erlang"),
   88406154   = hash:fnv32a("erlang"),
   2424231706 = hash:fnv32m("erlang").


seq(_Config) ->
   1073741824 = hash:seq31(1),
   536870912  = hash:seq31( hash:seq31(1) ),
   1073741824 = hash:seq32(1),
   536870912  = hash:seq32( hash:seq32(1) ).


fold(_Config) ->
   1701970438 = hash:fold32("erlang").


buz(_Config) ->
   A0 = hash:buz32(4),
   { 97, A1} = hash:buz32($a, A0),
   {160, A2} = hash:buz32($b, A1),
   {291, A3} = hash:buz32($c, A2),
   {546, A4} = hash:buz32($d, A3),
   {561,  _} = hash:buz32($e, A4).


pbkdf2(_Config) ->
   %% see https://www.ietf.org/rfc/rfc6070.txt
   <<"0c60c80f961f0e71f3a9b524af6012062fe037a6">> = btoh( hash:pbkdf2(sha, <<"password">>, <<"salt">>, 1, 20 * 8) ),
   <<"ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957">> = btoh( hash:pbkdf2(sha, <<"password">>, <<"salt">>, 2, 20 * 8) ),
   <<"4b007901b765489abead49d926f721d065a429c1">> = btoh( hash:pbkdf2(sha, <<"password">>, <<"salt">>, 4096, 20 * 8) ).
   % takes too long for daily test but good for hash validation
   % <<"eefe3d61cd4da4e4e9945b3d6ba2158c2634e984">> = btoh( hash:pbkdf2(sha, <<"password">>, <<"salt">>, 16777216, 20 * 8) ).


btoh(X) ->
   << <<(if A < 10 -> $0 + A; A >= 10 -> $a + (A - 10) end):8>> || <<A:4>> <=X >>.
