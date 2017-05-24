%%
%%   Copyright 2017 Dmitry Kolesnikov, All Rights Reserved
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
-module(hash_aws_v4_SUITE).
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

-export([host/1, post/1]).

%%%----------------------------------------------------------------------------   
%%%
%%% suite
%%%
%%%----------------------------------------------------------------------------   
all() ->
   [
      {group, aws}
   ].

groups() ->
   [
      {aws, [], 
         [host, post]}
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

host(_) ->
   meck:new(calendar, [unstick, passthrough]),
   meck:expect(calendar, now_to_universal_time, fun(_) -> {{2016, 6, 18}, {22, 4, 5}} end),

   Hash = hash:aws_v4(<<"YOURKEY">>, <<"YOURSECRET">>, 'us-east-1', 'es'),
   Sign = hash:aws_v4_sign("search-foo.us-east-1.es.amazonaws.com", Hash),
   
   meck:unload(calendar),
   {'x-amz-date', <<"20160618T220405Z">>} = lists:keyfind('x-amz-date', 1, Sign),
   {'Host', <<"search-foo.us-east-1.es.amazonaws.com">>} = lists:keyfind('Host', 1, Sign),
   {'Authorization', <<"AWS4-HMAC-SHA256 Credential=YOURKEY/20160618/us-east-1/es/aws4_request, SignedHeaders=host;x-amz-date, Signature=ca0a856286efce2a4bd96a978ca6c8966057e53184776c0685169d08abd74739">>} = lists:keyfind('Authorization', 1, Sign).

post(_) ->
   meck:new(calendar, [unstick, passthrough]),
   meck:expect(calendar, now_to_universal_time, fun(_) -> {{2016, 6, 18}, {22, 4, 5}} end),

   Hash = hash:aws_v4(<<"YOURKEY">>, <<"YOURSECRET">>, 'us-east-1', 'es'),
   Sign = hash:aws_v4_sign('POST', "search-foo.us-east-1.es.amazonaws.com", "/", [], [], <<"foo=bar">> ,Hash),

   meck:unload(calendar),
   {'x-amz-date', <<"20160618T220405Z">>} = lists:keyfind('x-amz-date', 1, Sign),
   {'Host', <<"search-foo.us-east-1.es.amazonaws.com">>} = lists:keyfind('Host', 1, Sign),
   {'Authorization', <<"AWS4-HMAC-SHA256 Credential=YOURKEY/20160618/us-east-1/es/aws4_request, SignedHeaders=host;x-amz-date, Signature=a6fd88e5f5c43e005482894001d9b05b43f6710e96be6098bcfcfccdeb8ed812">>} = lists:keyfind('Authorization', 1, Sign).


