%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_ets).

-behaviour(ehotp_backend).

-export([init/0
         ,in/2
         ,out/2
        ]).

-include("ehotp.hrl").

-define(TABLE, ?MODULE).



init() ->
     ets:new(?TABLE, [named_table
                      ,private
                      ,{keypos, #ehotp.uid}
                     ]).

in(R, X) when is_record(X, ehotp) ->
    ets:insert(?TABLE, X),
    R.

out(R, Key) ->
    case ets:lookup(?TABLE, Key) of
        [X] -> {R, {ok, X}};
        _   -> {R, {error, not_found}}
    end.
            
    


