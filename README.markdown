# Mochijson3 json lib #

## Usage: ##

```erlang
% Json data
Body = "{\"a\":\"handoff\",\"croupier\":\"game1@demo.pokerbroker.org\"}".

> "{\"a\":\"handoff\",\"croupier\":\"game1@demo.pokerbroker.org\"}"

% Decode json
Json = mochijson3:decode(Body).

> {struct,[{<<"a">>,<<"handoff">>},
         {<<"croupier">>,<<"game1@demo.pokerbroker.org">>}]}

% Get croupier field
Result = mochijson3_helper:get_path_value([{1, <<"croupier">>}], Json).

> <<"game1@demo.pokerbroker.org">>

```

## Licence ##

MIT - http://en.wikipedia.org/wiki/MIT_License
