%%%---------------------------------------------------------------------------------------
%%% File    : jsonh.erl
%%% Author  : Max Burinov <bourinov@gmail.com>
%%% Description : JSON Helper. Designed to provide complimentary functions for mochijson library.
%%%
%%% Created :  09.01.2012
%%%---------------------------------------------------------------------------------------
-module(mochijson3_helper).
-author('bourinov@gmail.com').

-export([get_path_value/2]).

%%----------------------------------------------------------------------------------------
%% @doc Allows extract data from structure from mochijson:decode
-spec get_path_value(Path :: [binary() | pos_integer()], RD :: any()) -> any() | error.
get_path_value(Path, RD) ->
	[Key | Tail] = Path,
	case RD of
		{struct, List} when is_binary(Key) ->
			case get_value(Key, List) of
				error -> % no such value
					error;
				Value ->
					if
						Tail == [] ->
							Value; % this is what we are looking for
						true -> % path is not empty, we have to continue
							get_path_value(Tail, Value) % go really deep
					end
			end;
		{array, XList} when is_integer(Key), Key =< length(XList), Key > 0 -> % array case
			get_path_value(Tail, lists:nth(Key, XList));
		_ -> % wrong input
			error
	end.

%%----------------------------------------------------------------------------------------
%% @doc Used to get values from POST requests
-spec get_value(Key :: string(), RD :: [{string(), string()}]) -> error | term().
get_value(Key, RD) when is_binary(Key) -> % string
    case lists:keyfind(Key, 1, RD) of
        false -> error;
        {Key, Value} -> Value
    end.

%% =======================================================================================
%% unit tests
%% =======================================================================================
%% Tests disabled until they can be prevented from running when included
%% as a dependency.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_json_path_value1_test() ->

	Fields = {struct,
				[{<<"algorithm">>,<<"HMAC-SHA256">>},
				{<<"credits">>,
					{struct,
						[{<<"buyer">>,1027419793},
						{<<"receiver">>,1027419793},
						{<<"order_id">>,163932157040089},
						{<<"order_info">>, <<"{\"title\":\"BFF Locket\",\"description\":\"This is a BFF Locket...\",\"price\":\"10\",\"image_url\":\"http://www.facebook.com/images/gifts/21.png\",\"item_id\":12345,\"product_url\":\"http://www.facebook.com/images/gifts/21.png\"}">>},
						{<<"test_mode">>,1}]
					}
				},
				{<<"expires">>,1324566000},
				{<<"issued_at">>,1324558806},
				{<<"oauth_token", "AAACZAFCZBTVWIBAItMZAwXXrYstGHwkA3winVuj8NUZC5eYYRx2JPME5HsZAyd7yorQrERPElmJwQaSGnq2tg6FZCO0Tqb4EOcL1ZBzQpTrlKXzyhqJg265">>},
				{<<"user">>,
					{struct,
						[{<<"country">>, <<"ru">>},
						{<<"locale">>,   <<"en_US">>},
						{<<"age">>,
							{struct,
								[{<<"min">>,21}]
							}
						}]
					}
				},
				{<<"user_id">>, <<"1027419793">>}]
			},

	?assertEqual(<<"HMAC-SHA256">>, get_path_value([<<"algorithm">>], Fields)),
	?assertEqual(1027419793, get_path_value([<<"credits">>, <<"buyer">>], Fields)),
	?assertEqual(<<"en_US">>, get_path_value([<<"user">>, <<"locale">>], Fields)),
	?assertEqual(error, get_path_value([<<"user">>, <<"locale1">>], Fields)),

	ok.

get_json_path_value2_test() ->

	Fields = {struct1,
				[{<<"algorithm">>,<<"HMAC-SHA256">>},
				{<<"credits">>,
					{struct3,
						[{<<"buyer">>,1027419793},
						{<<"receiver">>,1027419793},
						{<<"order_id">>,163932157040089},
						{<<"order_info">>, <<"{\"title\":\"BFF Locket\",\"description\":\"This is a BFF Locket...\",\"price\":\"10\",\"image_url\":\"http://www.facebook.com/images/gifts/21.png\",\"item_id\":12345,\"product_url\":\"http://www.facebook.com/images/gifts/21.png\"}">>},
						{<<"test_mode">>,1}]
					}
				},
				{<<"expires">>,1324566000},
				{<<"issued_at">>,1324558806},
				{<<"oauth_token">>, <<"AAACZAFCZBTVWIBAItMZAwXXrYstGHwkA3winVuj8NUZC5eYYRx2JPME5HsZAyd7yorQrERPElmJwQaSGnq2tg6FZCO0Tqb4EOcL1ZBzQpTrlKXzyhqJg265">>},
				{<<"user">>,
					{struct2,
						[{<<"country">>,<<"ru">>},
						{<<"locale">>,<<"en_US">>},
						{<<"age">>,
							{struct,
								[{<<"min">>,21}]
							}
						}]
					}
				},
				{<<"user_id">>, <<"1027419793">>}]
			},

	?assertEqual(error, get_path_value([<<"algorithm">>], Fields)),
	?assertEqual(error, get_path_value([<<"credits">>, <<"buyer">>], Fields)),
	?assertEqual(error, get_path_value([<<"user">>, <<"locale1">>], Fields)),

	ok.

get_json_path_value3_test() ->

	Fields = {struct,
					[{<<"content">>,
						{array,
							[{struct,
								[{<<"title">>,<<"BFF Locket">>},
				                {<<"price">>,<<"10">>},
				                {<<"description">>,<<"This is a BFF Locket...">>},
				                {<<"image_url">>,<<"http://www.facebook.com/images/gifts/21.png">>},
				                {<<"product_url">>, <<"http://www.facebook.com/images/gifts/21.png">>},
				                {<<"player_id">>,<<"f37c13d279ca301a7e1809175e4b700b">>},
				                {<<"item_id">>,<<"ITEM_ID_XSCRES">>}]
				            }]
				        }
				    },
				    {<<"method">>,<<"payments_get_items">>}]
				},

	?assertEqual(<<"10">>, get_path_value([<<"content">>, 1, <<"price">>], Fields)),
	?assertEqual(error, get_path_value([<<"content">>, 0, <<"price">>], Fields)),
	?assertEqual(error, get_path_value([<<"content">>, 2, <<"price">>], Fields)),
	?assertEqual(error, get_path_value([<<"content">>, 1, <<"price1">>], Fields)),
	?assertEqual(<<"payments_get_items">>, get_path_value([<<"method">>], Fields)),

	ok.
	
-endif.
