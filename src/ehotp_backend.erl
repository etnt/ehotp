%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_backend).

-export([behaviour_info/1]).

%%--------------------------------------------------------------------
%% @doc The ehotp_backend behaviour
%%
%% @end
%%--------------------------------------------------------------------
behaviour_info(callbacks) ->
    [{init,0}];
behaviour_info(_Other) ->
    undefined.
