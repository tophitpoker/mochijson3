# Mochijson3 json lib #

## Usage: ##

```erlang
% Json data
Body = "{\"a\":\"handoff\",\"croupier\":\"game1@demo.pokerbroker.org\"}".

Result: "{\"a\":\"handoff\",\"croupier\":\"game1@demo.pokerbroker.org\"}"

% Decode json
Json = mochijson3:decode(Body).

Result: {struct,[{<<"a">>,<<"handoff">>},
         {<<"croupier">>,<<"game1@demo.pokerbroker.org">>}]}

% Get croupier field
Result = mochijson3_helper:get_path_value([<<"croupier">>], Json).

Result: <<"game1@demo.pokerbroker.org">>

```

## Licence ##

MIT - http://en.wikipedia.org/wiki/MIT_License
