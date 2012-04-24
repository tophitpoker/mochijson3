# Mochijson3 json lib #

## Usage: ##

```erlang
% Json data
Body = <<"{\"key1\":\"value1\",\"key2\":\"value2\"}">>.

% Decode json
Json = mochijson3:decode(Body).

% Get croupier field
Result = mochijson3_helper:get_path_value([{1, <<"key1">>}], Json).

% Now Result is equal to <<"value1">>

```

## Licence ##

MIT - http://en.wikipedia.org/wiki/MIT_License
