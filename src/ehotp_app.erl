%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_app).

-behaviour(application).

%% Application callbacks
-export([start/2
         ,stop/1
         ,get_env/2
        ]).


start(_Type, StartArgs) ->
    case ehotp_sup:start_link(StartArgs) of
	{ok, Pid} -> 
	    {ok, Pid};
	Error ->
	    Error
    end.


stop(_State) ->
    ok.


get_env(Key, Default) ->
    case application:get_env(ehotp, Key) of
        {ok, Value} -> Value;
        _           -> Default
    end.
