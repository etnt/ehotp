%%%-------------------------------------------------------------------
%%% Created : 10 Mar 2009 by Torbjorn Tornkvist <tobbe@kreditor.se>
%%% Desc.   : Implementation of RFC 4226, HOTP.
%%%           An HMAC-Based One-Time Password Algorithm.
%%%-------------------------------------------------------------------
-module(ehotp).
-export([hotp_6/2
         ,hotp_7/2
         ,hotp_8/2
         ,unlock_key/2
         ,generate_random_key/0
        ]).

-export([test/0]).

generate_random_key() -> tbd. % FIXME
    
unlock_key(Pin, Lkey) -> tbd. % FIXME Pin xor Lkey
    

hotp_6(Key, Cnt) -> hotp(Key, Cnt, 6).
hotp_7(Key, Cnt) -> hotp(Key, Cnt, 7).
hotp_8(Key, Cnt) -> hotp(Key, Cnt, 8).

hotp(Key, Cnt, N) when is_binary(Key), is_integer(Cnt), is_integer(N) -> 
    'Truncate'(crypto:sha_mac(Key, <<Cnt:64>>), N).

'Truncate'(Mac, N) ->
    <<_:19/bytes, _:4/bits, Offset:4>> = Mac,
    <<_:Offset/bytes, _:1/bits, P:31, _/binary>> = Mac,
    P rem n(N).

n(6) -> 1000000;
n(7) -> 10000000;
n(8) -> 100000000.
    


%%% Unit Test
test() -> [t(1),t(2),t(3)].

t(1) -> 872921   == 'Truncate'(b(), 6);
t(2) -> 7872921  == 'Truncate'(b(), 7);
t(3) -> 57872921 == 'Truncate'(b(), 8).

b() ->
    <<16#1f,16#86,16#98,16#69,16#0e,16#02,16#ca,16#16,16#61,16#85,
     16#50,16#ef,16#7f,16#19,16#da,16#8e,16#94,16#5b,16#55,16#5a>>.


    



