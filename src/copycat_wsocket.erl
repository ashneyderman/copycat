%%% -*- coding: utf-8; Mode: erlang; tab-width: 4; c-basic-offset: 4; indent-tabs-mode: nil -*-
%%% ex: set softtabstop=4 tabstop=4 shiftwidth=4 expandtab fileencoding=utf-8:

-module(copycat_wsocket).

-behaviour(cowboy_websocket_handler).

-define(SOCKET_UNRESPONSIVE_TIMEOUT, 30000).

-export([
    init/3,
    websocket_init/3,
    websocket_handle/3,
    websocket_info/3,
    websocket_terminate/3]).

-record(state, { ping_timer = undefined, last_pong = undefined, req_proc = copycat_dispatcher }).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
    lager:info("New socket arrived: ~p ", [self()]),
    {ok, TimerRef} = timer:send_interval(?SOCKET_UNRESPONSIVE_TIMEOUT, self(), send_ping),
    {ok, Req, #state{ ping_timer = TimerRef, last_pong = pong }}.

websocket_handle({text, Msg}, Req, #state{ req_proc = ReqProc } = State) ->
    Json = jsx:decode(Msg),
    MsgType = proplists:get_value(<<"msgtype">>, Json),
    Result = case MsgType of
         <<"login_req">> ->
             ReqProc:login(self(), Json);
         <<"logout_req">> ->
             ReqProc:logout(self(), Json);
         <<"chat_msg">> ->
             ReqProc:chat_msg(self(), Json),
             {ok, noreply};
         <<"read_receipt">> ->
             ReqProc:read_receipt(self(), Json),
             {ok, noreply};
         <<"pong">> ->
             {ok, pong};
         _ ->
             lager:debug("We ignore any and all messages for now, except pong ~p", [Msg]),
             {ok, noreply}
    end,
    case Result of
        {ok, pong} ->
            {ok, Req, State#state{ last_pong = pong }};
        {_,noreply} ->
            {ok, Req, State};
        {_,Reply} ->
            {reply, {text, Reply}, Req, State}
    end;

websocket_handle(_Data, Req, State) ->
    lager:warning("Received unknown message on handle ignoring for now: ~p", [_Data]),
    {ok, Req, State}.

websocket_info(send_ping, Req, #state{ last_pong = ping, ping_timer = TimerRef } = State) ->
    lager:warning("Dead socket detected: ~p", [self()]),
    timer:cancel(TimerRef),
    {shutdown, Req, State#state{ ping_timer = undefined }};

websocket_info({putonwire, Bin}, Req, State) -> {reply, {text, Bin}, Req, State};

websocket_info(send_ping, Req, #state{ last_pong = pong } = State) ->
    Jsx = [{msgtype,<<"ping">>},
        {msgkey, copycat_utils:bin_msgkey()},
        {msgtime, copycat_utils:bin_curtime()}],
    Bin = jsx:encode(Jsx),
    {reply, {text, Bin}, Req, State#state{ last_pong = ping }};

websocket_info(Info, Req, State) ->
    lager:warning("Received unknown message on info ignoring for now: ~p", [Info]),
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, #state{ ping_timer = TimerRef }) ->
    lager:info("WebSocket is terminating ~p its reason ~p", [self(), _Reason]),
    timer:cancel(TimerRef).

