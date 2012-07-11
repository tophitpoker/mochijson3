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
-spec get_path_value(Request :: [{pos_integer(), binary()}] | [], 
                        Data :: [{atom(), [{binary(), binary()}, ...]}] | {atom(), [{binary(), binary()}]}) -> 
                                                                                        Value :: binary() | error.
get_path_value([], Data) ->
    % if we have no [], we return Data
    Data;

%% @doc Find value from mochijson3 output
%% @end         
get_path_value(Path, RD) when is_list(RD) ->
    % Check request data length
    case length(Path) of
        1 ->
            % Get current request body
            [{Num, NeedToFind}] = Path,
            % Find in the data structure
            FindedData = lists:nth(Num, RD),
            % Get finded data
            {MaybeStruct, NeedToFindValue} = FindedData,
            case MaybeStruct of
                struct ->
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
                        MaybeStruct
           end;
        _ ->
           case RD of
               [] ->
                   error;
               _ ->
                   % Find further
                   get_path_value(Path, {struct, RD})
           end
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
              {Value, JSonValue} = X,
              if
                  Value == struct ->
                     TryToFindNext = lists:keyfind(NeedFindValue, 1, JSonValue),
                     case TryToFindNext of
                        false ->
                            false;
                        _ ->
                            true  
                     end;   
                  true ->
                     Value == NeedFindValue
              end
          end,
          DataList),
      
   case List of
      [] ->
         error;
      _ ->
          if
              Num > length(List) ->
                  error;
              true ->
                  % Return value
                  FindedValue = lists:nth(Num, List),
                  {IsStruct, OtherValue} = FindedValue,
                  case IsStruct of
                      struct ->
                          {struct, FindNextValue} = FindedValue,
                          TryToFindNextValue = lists:keyfind(NeedFindValue, 1, FindNextValue),
                          case TryToFindNextValue of
                              false ->
                                  error;
                              _ ->
                                  {_, Value} = TryToFindNextValue,
                                  get_path_value(T, Value)  
                          end;
                      _ ->
                          get_path_value(T, OtherValue)
                  end
         end
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
        
    Data4 =
        {struct, [
            {<<"resp1">>, <<"resp6">>},
            {<<"resp2">>, <<"resp4">>},
            {<<"resp3">>, <<"resp5">>},
            {<<"resp2">>, <<"aaaaaaaaaaaaaa">>}   
        ]},
   
    ?assertEqual(error, get_path_value([{1, <<"response">>}], Data4)),
   
    Data5 =        
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
        
    ?assertEqual(error, get_path_value([{2, <<"response">>}], Data5)),
    
    Data6 = 
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
        
    ?assertEqual(1103656, get_path_value([{1, <<"response">>}, {1, <<"uid">>}], Data6)),
    
    Data7 = 
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
        
     ?assertEqual(error, get_path_value([{1, <<"uid">>}], Data7)),
     
     Data8 = 
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
                 ]}
             ]}
        ]},
        
    ?assertEqual(error, get_path_value([{1, <<"uid">>}], Data8)),
        
    Data9 =
        {struct,[
            {<<"content">>,
            {struct,[{<<"status">>,<<"placed">>},
                     {<<"order_id">>,240753069327930}]}},
                     {<<"method">>,<<"payments_status_update">>}]},
                     
    ?assertEqual(240753069327930, get_path_value([{1, <<"content">>}, {1, <<"order_id">>}], Data9)),
     
    ok.
-endif.
