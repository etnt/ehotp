%%%-------------------------------------------------------------------
%%% Copyright (c) 2009 Torbjorn Tornkvist
%%% See the (MIT) LICENSE file for licensing information.
%%%
%%% Created : 10 Mar 2009 by Torbjorn Tornkvist <tobbe@tornkvist.org>
%%% Desc.   : Implementation of RFC 4226, HOTP.
%%% 
%%% @doc An HMAC-Based One-Time Password Algorithm.
%%%
%%% We also implements routines for creating the key and for generation 
%%% of a PIN code. The latter is also used to encrypt the Key for 
%%% protection when storing the Key.
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(ehotp).
-export([start/0
         ,hotp_6/2
         ,hotp_7/2
         ,hotp_8/2
         ,lock_key/2
         ,unlock_key/2
         ,generate_random_key/0
         ,generate_pin_code/0
         ,new/0
         ,uid/1, uid/2
         ,pin/1, pin/2
         ,lkey/1, lkey/2
         ,cnt/1, cnt/2
         ,fail/1, fail/2
         ,foldf/2
        ]).

-export([test/0]).

-include("ehotp.hrl").

%%%
%%% ADT access primitives
%%%

new() -> #ehotp{}.

uid(#ehotp{uid=Uid}) -> Uid;
uid(Uid) -> fun(E) -> uid(Uid, E) end.
uid(Uid, E) when is_record(E,ehotp) -> E#ehotp{uid=Uid}.

pin(#ehotp{pin=Pin}) -> Pin;
pin(Pin) -> fun(E) -> pin(Pin, E) end.
pin(Pin, E) when is_record(E,ehotp) -> E#ehotp{pin=Pin}.
    
lkey(#ehotp{lkey=Lkey}) -> Lkey;
lkey(Lkey) -> fun(E) -> lkey(Lkey, E) end.
lkey(Lkey, E) when is_record(E,ehotp) -> E#ehotp{lkey=Lkey}.

cnt(#ehotp{cnt=Cnt}) -> Cnt;
cnt(Cnt) -> fun(E) -> cnt(Cnt, E) end.
cnt(Cnt, E) when is_record(E,ehotp) -> E#ehotp{cnt=Cnt}.

fail(#ehotp{fail=Fail}) -> Fail;
fail(Fail) -> fun(E) -> fail(Fail, E) end.
fail(Fail, E) when is_record(E,ehotp) -> E#ehotp{fail=Fail}.

foldf(Fs, E) -> lists:foldl(fun(F,D) -> F(D) end, E, Fs).
                                    
    

%%% @doc Make sure the crypto application also is started.
%%%
start() ->
    crypto:start().

%%% @doc Generate a 6 figures HOTP number
%%%
hotp_6(Key, Cnt) -> hotp(Key, Cnt, 6).

%%% @doc Generate a 7 figurer HOTP number
%%%
hotp_7(Key, Cnt) -> hotp(Key, Cnt, 7).

%%% @doc Generate a 8 figures HOTP number
%%%
hotp_8(Key, Cnt) -> hotp(Key, Cnt, 8).

%%% @doc Generate a random 4 figures PIN code.
%%%
generate_pin_code() ->
    <<X:32,Y:32,Z:32>> = urandom(12),
    random:seed(X,Y,Z),
    non_zero()*1000 + num()*100 + num()*10 + num().

num() -> random:uniform(10) rem 10.
non_zero() -> random:uniform(9).
    
%%% @doc Generate a random 20 byte key.
%%%
generate_random_key() -> 
    {A,B,C} = erlang:now(),
    crypto:sha_mac(<<A:32,B:32,C:32>>, urandom(64)).

%%% @doc Encrypt (lock) the key using the Pin code.    
%%%
lock_key(Pin, Key) when is_binary(Key) -> 
    Pin2 = Pin*Pin*Pin,
    PinB = crypto:sha_mac(<<Pin:16>>, <<Pin2:32>>),
    crypto:exor(PinB, Key).

%%% @doc Decrypt (unlock) the key using the Pin code.    
%%%
unlock_key(Pin, LockedKey) when is_binary(LockedKey) -> 
    lock_key(Pin, LockedKey).

%%% @doc Implement the HOTP algoritm returning N(6-8) figures.
%%%
hotp(Key, Cnt, N) when is_binary(Key), is_integer(Cnt), is_integer(N) -> 
    'Truncate'(crypto:sha_mac(Key, <<Cnt:64>>), N).

'Truncate'(Mac, N) ->
    <<_:19/bytes, _:4/bits, Offset:4>> = Mac,
    <<_:Offset/bytes, _:1/bits, P:31, _/binary>> = Mac,
    P rem n(N).

n(6) -> 1000000;
n(7) -> 10000000;
n(8) -> 100000000.
    
urandom(N) when is_integer(N) ->
    list_to_binary(os:cmd("dd if=/dev/urandom ibs="++
                          integer_to_list(N)++
                          " count=1 2>/dev/null")).

%%% @doc Unit Tests
%%% @private
test() -> [t(1),t(2),t(3),t(4)].

t(1) -> 872921   == 'Truncate'(b(), 6);
t(2) -> 7872921  == 'Truncate'(b(), 7);
t(3) -> 57872921 == 'Truncate'(b(), 8);
t(4) -> 
    Key = generate_random_key(),
    Pin = generate_pin_code(),
    Lkey = lock_key(Pin, Key),
    Key == unlock_key(Pin, Lkey).

b() ->
    <<16#1f,16#86,16#98,16#69,16#0e,16#02,16#ca,16#16,16#61,16#85,
     16#50,16#ef,16#7f,16#19,16#da,16#8e,16#94,16#5b,16#55,16#5a>>.


    



