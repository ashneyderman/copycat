%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(copycat_app).

-behaviour(application).

-define(APP, copycat).

%% Application callbacks
-export([start/0, start/2, stop/1]).


%% ===================================================================
%% Public APIs
%% ===================================================================
start() ->
    a_start(?APP,temporary).

%% ===================================================================
%% Application callbacks
%% ===================================================================
start(_StartType, _StartArgs) ->
    D = cowboy_router:compile([
        {'_', [
            {"/wsocket", copycat_wsocket, []},
            {"/[...]", cowboy_static, [
                %%                    {directory, {priv_dir, ref_client, []}},
                {directory, <<"./priv">>},
                {mimetypes, {fun mimetypes:path_to_mimes/2, default}}
            ]}
        ]}
    ]),

    {ok,_} = cowboy:start_http(http, 10, [{port,9900}], [{env, [{dispatch,D}]}]),

    copycat_sup:start_link().

stop(_State) ->
    ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================
a_start(App, Type) ->
    start_ok(App, Type, application:start(App, Type)).

start_ok(_App, _Type, ok) -> ok;
start_ok(_App, _Type, {error, {already_started, _App}}) -> ok;
start_ok(App, Type, {error, {not_started, Dep}}) ->
    ok = a_start(Dep, Type),
    a_start(App, Type);
start_ok(App, _Type, {error, Reason}) ->
    erlang:error({app_start_failed, App, Reason}).