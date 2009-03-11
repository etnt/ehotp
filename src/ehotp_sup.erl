%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).


start_link(_) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init(_) ->
    EhotpIdSrv = {ehotp_srv,{ehotp_srv,start_link,[]},
                  permanent,2000,worker,[ehotp_srv]},

    {ok,{{one_for_all,0,1}, [EhotpIdSrv]}}.

