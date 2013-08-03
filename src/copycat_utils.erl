%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(copycat_utils).

%% API
-export([
    bin_curtime/0,
    bin_msgkey/0,
    bin_msgkey/1,
    get_env/2]).

bin_curtime() ->
    {{Y,M,D},{H,N,S}} = calendar:universal_time(),
    L = io_lib:format("~4..0w-~2..0w-~2..0wT~2..0w:~2..0w:~2..0w.000",[Y,M,D,H,N,S]),
    iolist_to_binary(L).

bin_msgkey() ->
    bin_msgkey(<<"REFCLNT">>).

bin_msgkey(Prefix) ->
    TS = {_,_,Micro} = os:timestamp(),
    {{Y,M,D},{H,N,S}} = calendar:now_to_universal_time(TS),
    L = io_lib:format("~s~4..0w~2..0w~2..0wT~2..0w~2..0w~2..0w~6..0w",[Prefix,Y,M,D,H,N,S,Micro]),
    iolist_to_binary(L).

get_env(Key, Def) ->
    AllEnv = application:get_all_env(copy_cat),
    proplists:get_value(Key, AllEnv, Def).