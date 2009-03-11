
# Erlang implementation of the HOTP algoritm (RFC-4226)

The RFC-4226 describes HOTP:

    An HMAC-Based One-Time Password Algoritm

*ehotp* implements the algorithm described in the RFC-4226 plus
a supporting framework for building an authentication system
based on One-Time Passwords (OTPs).

The basic idea is to authenticate a user with a personal
PIN code + an OTP that the user has generated.


# INSTALL

Clone it and run: make.


# RUN

    erl -pa ./ebin -s ehotp

