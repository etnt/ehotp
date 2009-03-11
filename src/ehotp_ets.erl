%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_ets).

-behaviour(ehotp_backend).

-export([init/0
        ]).

-define(TABLE, ?MODULE).

-record(ehotp_ets, {
           table
          }).


init() ->
    #ehotp_ets{table = ets:new(?TABLE, [named_table, private])}.


