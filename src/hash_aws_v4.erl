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
%% @doc
%%   aws sign v4 protocol implementation
%%   https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html
-module(hash_aws_v4).

-export([new/5, sign/7]).

%%
%% internal hash state
-define(ALGORITHM, <<"AWS4-HMAC-SHA256">>).
-record(aws, {
   access  = undefined :: binary(),
   secret  = undefined :: binary(),
   token   = undefined :: binary(),
   region  = undefined :: binary(),
   service = undefined :: binary()
}).

%%
%% seed hash function
new(Access, Secret, Token, Region, Service) ->
   #aws{
      access  = hash:s(Access), 
      secret  = hash:s(Secret),
      token   = Token,
      region  = hash:s(Region), 
      service = hash:s(Service)
   }.

sign(Mthd, Host, Path, Query, Head, Data, #aws{access = Access} = Hash) ->
   <<Date:8/binary, _/binary>> = AmzDate = amzdate(),
   Head1   = headers(AmzDate, Host, Head, Hash),
   HeadSgn = signed_headers(Head1),
   Request = lists:join(<<$\n>>, [
      hash:s(Mthd),
      canonical_path(Path),
      canonical_q(Query),
      canonical_head(Head1),
      HeadSgn,
      payload_hash(Data)
   ]),
   KeyScope = credential_scope(Date, Hash),
   ToSign   = lists:join(<<$\n>>, [?ALGORITHM, AmzDate, KeyScope, btoh(crypto:hash(sha256, Request))]),
   Signature = btoh(sign(signing_key(Date, Hash), ToSign)),
   AwsAuth   = iolist_to_binary([
      ?ALGORITHM, 
      <<" ">>,
      <<"Credential=">>, Access, <<$/>>, KeyScope, 
      <<", ">>,
      <<"SignedHeaders=">>, HeadSgn,
      <<", ">>,
      <<"Signature=">>, Signature
   ]),
   Tail = [X || X = {K, _} <- Head1, 
      K =:= 'x-amz-date' orelse K =:= 'Host' orelse K =:= 'x-amz-security-token'],
   [{'Authorization', AwsAuth}|Tail].

headers(AmzDate, Host, Head, Hash) ->
   header_amz_date(AmzDate,
      header_host(Host,
         header_amz_token(Hash, Head)
      )
   ). 

header_amz_date(AmzDate, Head) ->
   [{'x-amz-date', AmzDate} | Head].

header_amz_token(#aws{token = undefined}, Head) ->
   Head;
header_amz_token(#aws{token = Token}, Head) ->
   [{'x-amz-security-token', Token} | Head].

header_host(Host, Head) ->
   case lists:keyfind('Host', 1, Head) of
      false ->
         [{'Host', hash:s(Host)} | Head];
      _ ->
         Head
   end.


%% Step 2: Create canonical URI--the part of the URI from domain to query 
%% string (use '/' if no path)
canonical_path(undefined) -> <<$/>>;
canonical_path(Path) -> Path.

%% Step 3: Create the canonical query string. Query string values must
%% be URL-encoded (space=%20). The parameters must be sorted by name.
canonical_q(Query) ->
   iolist_to_binary(
      lists:join(<<$&>>, 
         [uri:escape(<<K/binary, $=, V/binary>>) || 
            {K, V} <- lists:sort(listT(Query))]
      )
   ).

listT(undefined) -> [];
listT(X) -> X.

%% Step 4: Create the canonical headers and signed headers. Header names
%% and value must be trimmed and lowercase, and sorted in ASCII order.
%% Note that there is a trailing \n.
canonical_head(Head) ->
   iolist_to_binary(
      lists:sort(
         [<<(lowercase(K))/binary, $:, (hash:s(V))/binary, $\n>> || {K, V} <- Head]
      )
   ).

%% Step 5: Create the list of signed headers. This lists the headers
%% in the canonical_headers list, delimited with ";" and in alpha order.
%% Note: The request can include any headers; canonical_headers and
%% signed_headers lists those that you want to be included in the 
%% hash of the request. "Host" and "x-amz-date" are always required.
signed_headers(Head) ->
   iolist_to_binary(
      lists:join(<<$;>>,
         lists:sort(
            [lowercase(K) || {K, _} <- Head]
         )
      )
   ). 

lowercase(X) ->
   << <<(to_lower(C)):8>> || <<C:8>> <= hash:s(X) >>.

to_lower(X) 
 when X >= $A, X =< $Z ->
   X + 32;
to_lower(X) ->
   X.

%% Step 6: Create payload hash (hash of the request body content). 
%% For GET requests, the payload is an empty string ("").
payload_hash(undefined) ->
   payload_hash(<<>>);
payload_hash(Payload) ->
   btoh(crypto:hash(sha256, Payload)).

%%
%% Create a date for headers and the credential string
amzdate() ->
   {{Year,Month,Day},{Hour,Min,Sec}} = calendar:now_to_universal_time(os:timestamp()),
   iolist_to_binary(
      io_lib:format(
         "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0BZ",
         [Year, Month, Day, Hour, Min, Sec]
      )
   ).

%%
%%
credential_scope(Date, #aws{region = Region, service = Service}) ->
   iolist_to_binary(
      lists:join(<<$/>>, [
         Date,
         Region,
         Service,
         <<"aws4_request">>
      ])
   ).

%%
%%
signing_key(Date, #aws{secret = Secret, region = Region, service = Service}) ->
   get_signature_key(Secret, Date, Region, Service).


%%
%%
get_signature_key(Key, Date, Region, Service) ->
   KDate    = sign(<<"AWS4", Key/binary>>, Date),
   KRegion  = sign(KDate, Region),
   KService = sign(KRegion, Service),
   sign(KService, <<"aws4_request">>). 

%%
%%
sign(Key, Data) ->
   crypto:hmac(sha256, Key, Data).
   

btoh(X) ->
   << <<(if A < 10 -> $0 + A; A >= 10 -> $a + (A - 10) end):8>> || <<A:4>> <=X >>.
