%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(copycat_dispatcher).

-behaviour(gen_server).

%% API
-export([
    start_link/0,
    login/2,
    logout/2,
    chat_msg/2,
    read_receipt/2,
    fetch_dir/0,
    presence/2,
    terminate/1]).

%% gen_server
-export([
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-record(session, {
            wsocket = undefined,
            username = undefined}).

-record(state, {
    sessions = dict:new(),
    directory = dict:new()}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

login(WSocket, JsxJson) ->
    gen_server:call(?MODULE, {login, WSocket, JsxJson}).

logout(WSocket, JsxJson) ->
    gen_server:call(?MODULE, {logout, WSocket, JsxJson}).

chat_msg(WSocket, JsxJson) ->
    gen_server:cast(?MODULE, {chat_msg_or_read_receipt, WSocket, JsxJson}),
    {ok, noreply}.

read_receipt(WSocket, JsxJson) ->
    gen_server:cast(?MODULE, {chat_msg_or_read_receipt, WSocket, JsxJson}),
    {ok, noreply}.

presence(logged_in,Username) ->
    Payload = [
        {msgtype,<<"presence">>},
        {msgkey, copycat_utils:bin_msgkey()},
        {msgtime, copycat_utils:bin_curtime()},
        {username,Username},
        {presence,<<"online">>}],
    gen_server:cast(?MODULE, {presence, Payload});
presence(logged_out,Username) ->
    Payload = [
        {msgtype,<<"presence">>},
        {msgkey, copycat_utils:bin_msgkey()},
        {msgtime, copycat_utils:bin_curtime()},
        {username,Username},
        {presence,<<"offline">>}],
    gen_server:cast(?MODULE, {presence, Payload});
presence(disconnected,Username) ->
    Payload = [
        {msgtype,<<"presence">>},
        {msgkey, copycat_utils:bin_msgkey()},
        {msgtime, copycat_utils:bin_curtime()},
        {username,Username},
        {presence,<<"disconnected">>}],
    gen_server:cast(?MODULE, {presence, Payload});
presence(reconnected,Username) ->
    Payload = [
        {msgtype,<<"presence">>},
        {msgkey, copycat_utils:bin_msgkey()},
        {msgtime, copycat_utils:bin_curtime()},
        {username,Username},
        {presence,<<"online">>}],
    gen_server:cast(?MODULE, {presence, Payload}).

fetch_dir() ->
    gen_server:call(?MODULE, fetch_dir).

terminate(WSocket) ->
    gen_server:cast(?MODULE, {terminate, WSocket}).

%% ===================================================================
%% gen_server callbacks
%% ===================================================================

init(_Args) ->
    PeopleDir = create_directory(),
    {ok, #state{directory = PeopleDir}}.

handle_call({login, WSocket, JsxJson}, _From, #state{sessions = Sessions, directory = PeopleDir} = State) ->
    NewUsername = proplists:get_value(<<"username">>, JsxJson),
    Password = proplists:get_value(<<"password">>, JsxJson),

    Session = case dict:find(WSocket, Sessions) of
                  {ok, #session{username = NewUsername}} -> session_didnot_logout;
                  {ok, #session{}} -> session_has_user;
                  {ok,_} -> no_valid_session;
                  error -> #session{wsocket = WSocket, username = NewUsername}
              end,
    case do_login(Password, JsxJson, State, Session) of
        {ok, Reply} ->
            {ShutdownSessions, NewSessions} = do_register_session(Session, Sessions),
            do_notify_shutdown_sessions(ShutdownSessions),
            NewPeopleDir = do_mark_user_online(PeopleDir,NewUsername),
            presence(logged_in, NewUsername),
            {reply, {ok, Reply}, State#state{sessions = NewSessions, directory = NewPeopleDir}};
        {error, Reply} ->
            {reply, {error, Reply}, State}
    end;

handle_call({logout, WSocket, Payload}, _From, #state{sessions = Sessions, directory = PeopleDir} = State) ->
    Session = #session{username = Username} = case dict:find(WSocket, Sessions) of
                                                  {ok,S} -> S;
                                                  error -> no_session_found
                                              end,
    lager:debug("logout is called on ~p, ~p", [WSocket, Payload]),
    case do_logout(Payload, Session) of
        {ok, Reply} ->
            NewSessions = dict:erase(WSocket,Sessions),
            NewPeopleDir = do_mark_user_offline(PeopleDir,Username),
            presence(logged_out,Username),
            {reply, {ok, Reply}, State#state{sessions = NewSessions, directory = NewPeopleDir}};
        {error, Reply} ->
            {reply, {error, Reply}, State}
    end;

handle_call(_Request, _From, #state{ directory = PeopleDir } = State) ->
    Result = dict:fold(
                    fun (K, V, AccIn) -> AccIn ++ [[{<<"username">>, K} | V]] end,
                    [],
                    PeopleDir),
    {reply, jsx:encode(Result), State};

handle_call(_Request, _From, State) ->
    {noreply, State}.

handle_cast({chat_msg_or_read_receipt, _WSocket, Payload}, #state{sessions = Sessions} = State) ->
    MsgType = proplists:get_value(<<"msgtype">>, Payload),
    To = proplists:get_value(<<"to">>, Payload),
    UserSessions = do_find_sessions_by_username(Sessions,To),
    Bin  = jsx:encode(Payload),
    lager:debug("~p message received to be delivered to: ~p; total sesssions ~p; bin: ~p", [MsgType, To, length(UserSessions), Bin]),
    lists:foreach(fun(#session{wsocket = ToWSocket}) -> ToWSocket ! {putonwire , Bin} end, UserSessions),
    {noreply, State};
handle_cast({presence, Payload}, #state{ sessions = Sessions } = State) ->
    Bin = jsx:encode(Payload),
    dict:fold(fun(_K,#session{wsocket = WSocket},A) -> WSocket ! {putonwire, Bin}, A end, [], Sessions),
    {noreply, State};
handle_cast({terminate, WSocket}, #state{sessions = Sessions} = State) ->
    lager:debug("terminate is called on ~p", [WSocket]),
    NewSessions = dict:erase(WSocket,Sessions),
    {noreply, State#state{ sessions = NewSessions }};
handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ===================================================================
%% Private functions
%% ===================================================================
do_mark_user_online(PeopleDir,Username) ->
    {ok, UserInfo} = dict:find(Username,PeopleDir),
    dict:store(Username, [{K,V} || {K,V} <- UserInfo, K =/= <<"presence">>] ++ [{<<"presence">>,<<"online">>}],PeopleDir).

do_mark_user_offline(PeopleDir,Username) ->
    {ok, UserInfo} = dict:find(Username,PeopleDir),
    dict:store(Username, [{K,V} || {K,V} <- UserInfo, K =/= <<"presence">>] ++ [{<<"presence">>,<<"offline">>}],PeopleDir).

do_notify_shutdown_sessions(_Sessions) -> ok.

do_register_session(#session{ username = Username, wsocket = WSocket } = NewSession, Sessions) ->
    ShutdownSessions = do_find_sessions_by_username(Sessions, Username),
    NewSessions = dict:store(WSocket,NewSession, Sessions),
    {ShutdownSessions, NewSessions}.

do_logout(Payload,#session{}) ->
    MsgKey = proplists:get_value(<<"msgkey">>, Payload),
    Response = [{msgtype,<<"logout_resp">>},
        {msgkey,MsgKey},
        {msgtime, copycat_utils:bin_curtime()}],
    {ok, jsx:encode(Response)};
do_logout(Payload,no_session_found) ->
    MsgKey = proplists:get_value(<<"msgkey">>, Payload),
    Response = [{msgtype,<<"error">>},
        {msgkey,MsgKey},
        {msgtime, copycat_utils:bin_curtime()},
        {original_msgtype,<<"logout_req">>},
        {error_code, <<"100">>},
        {error_message, <<"No session found for the logout. You might have to login before doing a logout.">>}],
    {ok, jsx:encode(Response)}.

do_login(<<"secret">>, CmdPayload, #state{directory = PeopleDir}, #session{}) ->
    MsgKey = proplists:get_value(<<"msgkey">>, CmdPayload),
    Username = proplists:get_value(<<"username">>, CmdPayload),
    {ok, UserInfo} = dict:find(Username, PeopleDir),

    Roster = dict:fold(
        fun (K, _V, AccIn) when K =:= Username  -> AccIn;
            (K, V, AccIn) -> AccIn ++ [[{<<"username">>, K} | V]]
        end, [], PeopleDir),

    Response = [
        {msgtype, <<"login_resp">>},
        {msgkey, MsgKey},
        {msgtime, copycat_utils:bin_curtime()},
        {session, [
                    {session_token, generate_session_token()},
                    {username, Username},
                    {last_name, proplists:get_value(<<"last_name">>, UserInfo)},
                    {first_name, proplists:get_value(<<"first_name">>, UserInfo)},
                    {title, proplists:get_value(<<"title">>, UserInfo)},
                    {phone_num, proplists:get_value(<<"phone_num">>, UserInfo)},
                    {ext, proplists:get_value(<<"ext">>, UserInfo)},
                    {presence, proplists:get_value(<<"presence">>, UserInfo)}
                 ]
        },
        {roster, [ [{username, proplists:get_value(<<"username">>, UserInfo1)},
                    {last_name, proplists:get_value(<<"last_name">>, UserInfo1)},
                    {first_name, proplists:get_value(<<"first_name">>, UserInfo1)},
                    {title, proplists:get_value(<<"title">>, UserInfo1)},
                    {phone_num, proplists:get_value(<<"phone_num">>, UserInfo1)},
                    {ext, proplists:get_value(<<"ext">>, UserInfo1)},
                    {presence, proplists:get_value(<<"presence">>, UserInfo1)}] || UserInfo1 <- Roster
                 ]
        }
    ],
    {ok, jsx:encode(Response)};

do_login(_, CmdPayload, _, session_didnot_logout) ->
    do_login_error(CmdPayload, <<"1">>, <<"Session is still logged in.">>);
do_login(_, CmdPayload, _, session_has_user) ->
    do_login_error(CmdPayload, <<"2">>, <<"Session still has logged in user.">>);
do_login(_, CmdPayload, _, no_valid_session) ->
    do_login_error(CmdPayload, <<"3">>, <<"Unable to find valid session.">>);
do_login(_, CmdPayload, _, _) ->
    do_login_error(CmdPayload, <<"4">>, <<"Wrong password. Try secret.">>).

do_login_error(CmdPayload, ErrorCode, ErrorMsg) ->
    MsgKey = proplists:get_value(<<"msgkey">>, CmdPayload),
    Response = [
        {msgtype, <<"error">>},
        {msgkey, MsgKey},
        {msgtime, copycat_utils:bin_curtime()},
        {original_msgtype, <<"login_req">>},
        {error_code, ErrorCode},
        {error_message, ErrorMsg}],
    {error, jsx:encode(Response)}.

create_directory() ->
    L = [
        {<<"brad">>, [
            {<<"last_name">>, <<"Pitt">>},
            {<<"first_name">>, <<"Brad">>},
            {<<"title">>, <<"Gerry Lane">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"mireille">>, [
            {<<"last_name">>, <<"Enos">>},
            {<<"first_name">>, <<"Mireille">>},
            {<<"title">>, <<"Karin Lane">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"daniella">>, [
            {<<"last_name">>, <<"Kertesz">>},
            {<<"first_name">>, <<"Daniella">>},
            {<<"title">>, <<"Segen">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"james">>, [
            {<<"last_name">>, <<"Dale">>},
            {<<"first_name">>, <<"James">>},
            {<<"title">>, <<"Captain Speke">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"ludi">>, [
            {<<"last_name">>, <<"Boeken">>},
            {<<"first_name">>, <<"Ludi">>},
            {<<"title">>, <<"Jurgen Warmbrunn">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"matthew">>, [
            {<<"last_name">>, <<"Fox">>},
            {<<"first_name">>, <<"Matthew">>},
            {<<"title">>, <<"Parajumper">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"fana">>, [
            {<<"last_name">>, <<"Mokoena">>},
            {<<"first_name">>, <<"Fana">>},
            {<<"title">>, <<"Thierry Umutoni">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"david">>, [
            {<<"last_name">>, <<"Morse">>},
            {<<"first_name">>, <<"David">>},
            {<<"title">>, <<"Ex-CIA Agent">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"elyes">>, [
            {<<"last_name">>, <<"Gabel">>},
            {<<"first_name">>, <<"Elyes">>},
            {<<"title">>, <<"Andrew Fassbach">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]},
        {<<"abigail">>, [
            {<<"last_name">>, <<"Hargrove">>},
            {<<"first_name">>, <<"Abigail">>},
            {<<"title">>, <<"Rachel Lane">>},
            {<<"phone_num">>, <<"212-555-12-12">>},
            {<<"ext">>, <<"911">>},
            {<<"presence">>, <<"offline">>}
            ]}
    ],
    lists:foldl(fun({Username, FullInfo}, Dict) -> Dict:store(Username, FullInfo) end, dict:new(), L).

generate_session_token() ->
    iolist_to_binary([io_lib:format("~2.16.0B", [Byte]) || <<Byte:8>> <= crypto:sha(crypto:rand_bytes(16))]).

do_find_sessions_by_username(Sessions, Username) ->
    dict:fold(
        fun(_K,#session{username = U} = V,A) ->
            case U == Username of
                true -> [V|A];
                false -> A
            end
        end, [], Sessions).