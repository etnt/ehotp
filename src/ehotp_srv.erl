%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_srv).

-behaviour(gen_server).

-export([start_link/0
         ,foo/1
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(s, {
          backend,            % backend callback module
          backend_data        % opaque backend data
         }).

%%% --------------------------------------------------------------------
%%% @spec foo( string() ) -> ok.
%%%
%%% @doc Blaha...
%%% @end
%%% --------------------------------------------------------------------
-spec foo( string() ) -> ok.

foo(X) ->
    ok = gen_server:call(?SERVER, {foo, X}).
            

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Backend = ehotp:get_env(backend, ehotp_ets),
    State = #s{backend      = Backend,
               backend_data = Backend:init()},
    {ok, State}.

handle_call({foo, X}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

