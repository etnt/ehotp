%%%-------------------------------------------------------------------
%%% Copyright (c) 2008-2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created   : 11 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%%-------------------------------------------------------------------
-module(ehotp_ets).

-behaviour(ehotp_backend).

-export([init/0
         ,in/1
         ,out/1
        ]).

-include("ehotp.hrl").

-define(TABLE, ?MODULE).

-record(ehotp_ets, {
           table
          }).


init() ->
    #ehotp_ets{table = ets:new(?TABLE, [named_table
                                        ,private
                                        ,{keypos, #ehotp.uid}
                                       ])}.

in(X) when is_record(X, ehotp) ->
    ets:insert(?TABLE, X).

out(Key) ->
    case ets:lookup(?TABLE, Key) of
        [X] -> {ok, X};
        _   -> {error, not_found}
    end.
            
    


