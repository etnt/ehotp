%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_srv).

-behaviour(gen_server).

-export([start_link/0
         ,in/1
         ,out/1
         ,verify/3
        ]).

-import(ehotp, [hotp_6/2,cnt/1,cnt/2,lkey/1,unlock_key/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-include("ehotp.hrl").

-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

-record(s, {
          backend,            % backend callback module
          backend_data        % opaque backend data
         }).

%%% --------------------------------------------------------------------
%%% @spec in( #ehotp{} ) -> ok.
%%%
%%% @doc Blaha...
%%% @end
%%% --------------------------------------------------------------------
-spec in( #ehotp{} ) -> ok.

in(E) when is_record(E, ehotp)  ->
    ok = gen_server:call(?SERVER, {in, E}).
            
%%% --------------------------------------------------------------------
%%% @spec out( Key::any() ) -> {ok, #ehotp{}} | {error, Reason::any()}.
%%%
%%% @doc Blaha...
%%% @end
%%% --------------------------------------------------------------------
-spec out( any() ) -> ok.

out(Uid) ->
    gen_server:call(?SERVER, {out, Uid}).

%%% --------------------------------------------------------------------
%%% @spec verify( Uid::any() , Pin::integer() , OTP::integer() ) -> bool().
%%%
%%% @doc Blaha...
%%% @end
%%% --------------------------------------------------------------------
-spec verify( any() , integer() , integer() ) -> bool().

verify(Uid, Pin, OTP) ->
    gen_server:call(?SERVER, {verify, Uid, Pin, OTP}).
            
            

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
    Backend = ehotp_app:get_env(backend, ehotp_ets),
    State = #s{backend      = Backend,
               backend_data = Backend:init()},
    {ok, State}.

handle_call({in, E}, _From, State) ->
    Backend = State#s.backend,
    D = Backend:in(State#s.backend_data, E),
    {reply, ok, State#s{backend_data = D}};

handle_call({out, Uid}, _From, State) ->
    Backend = State#s.backend,
    {D,Res} = Backend:out(State#s.backend_data, Uid),
    {reply, Res, State#s{backend_data = D}};

handle_call({verify, Uid, Pin, OTP}, _From, State) ->
    Backend = State#s.backend,
    try
        {D,{ok,E}} = Backend:out(State#s.backend_data, Uid),
        D2 = Backend:in(D, cnt(cnt(E)+1,E)),
        {reply, do_verify(E,Pin,OTP), State#s{backend_data = D2}}
    catch 
        _:_ -> {reply, false, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


do_verify(E, Pin, OTP) ->
    try
        Key = unlock_key(Pin, lkey(E)),
        Cnt = cnt(E),
        OTP == hotp_6(Key,Cnt)
    catch
        _:_ -> false
    end.
