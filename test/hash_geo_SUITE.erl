%%
-module(hash_geo_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([
   all/0,
   latlng/1,
   geohash/1,
   proximity/1
]).


all() -> 
   [
      latlng,
      geohash,
      proximity
   ].

latlng(_) ->
   <<"u6zfhe78s000">> = hash:geo({60.8382511138916, 22.35116958618164}).

geohash(_) ->
   {60.83825119771063, 22.351169753819704} = hash:geo(<<"u6zfhe78s000">>).

proximity(_) ->
   #{
      nw := <<"u6zfhe73">>,
      n  := <<"u6zfhe79">>,
      ne := <<"u6zfhe7c">>,

      w  := <<"u6zfhe72">>,
      e  := <<"u6zfhe7b">>,

      sw := <<"u6zfhe5r">>,
      s  := <<"u6zfhe5x">>,
      se := <<"u6zfhe5z">>
   } = hash:geo_proximity(<<"u6zfhe78">>).

