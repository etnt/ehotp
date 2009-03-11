%%% -*- mode:erlang -*-
{application, ehotp,
  [{description, "Erlang Hash based One Time Password system."},
   {vsn, "0.1.0"},
   {mod, {ehotp_app, []}},
   {env, [{backend, ehotp_ets}    % ehotp_(ets | mnesia | couchdb)
         ]},
   {modules, [ehotp
              ,ehotp_app
              ,ehotp_sup
              ,ehotp_srv
              ,ehotp_ets
             ]},
   {applications, [kernel, stdlib, crypto]}
  ]
}.

