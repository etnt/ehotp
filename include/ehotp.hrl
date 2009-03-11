-ifndef(_EHOTP).
-define(_EHOTP, true).

-record(ehotp, {
          uid,             % User ID
          lkey,            % Locked Key
          cnt = 0,         % The counter
          fail = 0         % Number of failed attempts
         }).

-endif.
