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

%% @doc Find value from mochijson3 output
%% @end
-spec get_path_value(Request :: [{pos_integer(), binary()}] | [], Data :: [{atom(), [{binary(), binary()}, ...]}]) -> binary().
get_path_value([], Data) ->
    % if we have no [], we return Data
    Data;

%% @doc Find value from mochijson3 output
%% @end       
get_path_value(Path, RD) when is_list(RD) ->
    % Check request data length
    % [{Num, RequestBody}, ...]
    case length(Path) of
        1 ->
            % Get current request body
            [{Num, NeedToFind}] = Path,
            % Find in the data structure
            FindedData = lists:nth(Num, RD),
            % Get finded data
            {struct, NeedToFindValue} = FindedData,    
            % Get value
            TryGetValue = lists:keyfind(NeedToFind, 1, NeedToFindValue),
            case TryGetValue of
                false ->
                   error;
                _ ->
                  {_, Value} = TryGetValue,
                  Value
            end;
        _ ->
           % Find further
           get_path_value(Path, lists:nth(1, RD))
    end;

%% @doc Find value from mochijson3 output
%% @end  
get_path_value(Path, RD) when is_tuple(RD) ->
   % Get data struct body
   {struct, DataList} = RD,
   % Get first request body
   [H | T] = Path,
   % Find value
   {Num, NeedFindValue} = H,   
   List = lists:filter(fun(X) -> 
              {Value, _} = X,
              Value == NeedFindValue
          end,
          DataList),
   case List of
      [] ->
         error;
      _ ->
          % Return value
          FindedValue = lists:nth(Num, List),
          {NeedFindValue, OtherValue} = FindedValue,
          get_path_value(T, OtherValue)
   end.

%% =======================================================================================
%% unit tests
%% =======================================================================================
%% Tests disabled until they can be prevented from running when included
%% as a dependency.
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_path_value_test() ->
    Data1 =
        {struct, [
            {<<"resp1">>, <<"resp6">>},
            {<<"resp2">>, <<"resp4">>},
            {<<"resp3">>, <<"resp5">>}
        ]},
        
    ?assertEqual(<<"resp4">>, get_path_value([{1, <<"resp2">>}], Data1)),
    
    Data2 =
        {struct, [
            {<<"resp1">>, <<"resp6">>},
            {<<"resp2">>, <<"resp4">>},
            {<<"resp3">>, <<"resp5">>},
            {<<"resp2">>, <<"aaaaaaaaaaaaaa">>}   
        ]},
    
    ?assertEqual(<<"aaaaaaaaaaaaaa">>, get_path_value([{2, <<"resp2">>}], Data2)),
            
    Data3 =
        {struct,[
            {<<"1">>, <<"2">>},
            {<<"response">>, [
                {struct,[
                    {<<"uid">>,1103656},
                    {<<"first_name">>,<<"Max">>},
                    {<<"last_name">>,<<"Bourinov">>},
                    {<<"response2">>, [
                        {struct,[
                            {<<"uid">>,1103656},
                            {<<"first_name">>,<<"Max">>},
                            {<<"last_name">>,<<"Bourinov">>}
                        ]},
                        {struct,[
                            {<<"uid">>,1103656},
                            {<<"first_name">>,<<"Sasha">>},
                            {<<"last_name">>,<<"Kuleshov">>}
                        ]}
                   ]}     
                 ]},
                 {struct,[
                     {<<"uid">>,1103656},
                     {<<"first_name">>,<<"Max">>},
                     {<<"last_name">>,<<"Bourinov">>}
                 ]}
             ]}
        ]},
        
    ?assertEqual(<<"Max">>, get_path_value([{1, <<"response">>}, {1, <<"response2">>}, {1, <<"first_name">>}], Data3)),
        
    ok.
-endif.
