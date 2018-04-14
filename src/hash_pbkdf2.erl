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
%%   http://en.wikipedia.org/wiki/PBKDF2
-module(hash_pbkdf2).

-export([pbkdf2/5]).
-compile({no_auto_import,[ceil/1]}).

pbkdf2(PRF, Pass, Salt, C, DkLen) ->
   Init = crypto:hmac(PRF, Pass, Salt),
   N    = ceil(DkLen / (byte_size(Init) * 8)),
   binary:part(    
      erlang:iolist_to_binary(
         [fpbkdf2(C, PRF, Pass, <<Salt/binary, I:32/integer>>) || I <- lists:seq(1, N)]
      ),
      0,
      DkLen div 8
   ).

fpbkdf2(C, PRF, Pass, Data) ->
   Init = crypto:hmac(PRF, Pass, Data),
   fpbkdf2(C - 1, PRF, Pass, Init, Init).   

fpbkdf2(0, _PRF, _Pass, _Data, Acc) ->
   Acc;
fpbkdf2(C,  PRF,  Pass,  Data, Acc) ->
   Next = crypto:hmac(PRF, Pass, Data),
   fpbkdf2(C - 1, PRF, Pass, Next, crypto:exor(Acc, Next)).

ceil(X) ->
   case trunc(X) of 
      Y when Y < X -> Y + 1; 
      Y            -> Y 
   end.
