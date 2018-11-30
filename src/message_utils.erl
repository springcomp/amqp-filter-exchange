-module(message_utils).

-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-export([create_message/2, get_message_header/1]).

create_message(#delivery{message = #basic_message{content = Content}} = Message, Args) ->
    Actions = get_actions(Args),
    Headers = get_message_header(Content),
    NewHeaders = add_new_headers(Headers, Actions),
    set_delivery_headers(Message, NewHeaders).

get_message_header(Content) ->     
    Headers = rabbit_basic:extract_headers(Content),
    case Headers of
        undefined -> [];
        _ -> Headers        
    end.

%%====================================================================
%% Internal functions

get_actions([]) -> [];
get_actions(Args) ->
    ActionAsString = get_actions_as_string(Args),
    case ActionAsString of
        [] -> [];
        _ -> 
            Actions = action_reader:getList(ActionAsString),
            convert_to_binary(Actions)            
    end.

get_actions_as_string(Args) -> 
    Result = search_actions_argument(Args),
    case Result of
        [ActionAsBinary] -> binary_to_list(ActionAsBinary);
        _ -> []            
    end.
    
search_actions_argument(Args) ->
    lists:foldl(fun({Key, longstr, Value}, Acc) when Key == <<"actions">> -> [Value] ++ Acc;(_, Acc) -> Acc end, [], Args).

set_delivery_headers(Delivery, Headers) ->
    Msg = get_msg(Delivery),
    Content = get_content(Msg),
    Props = get_props(Content),
    
    #content{payload_fragments_rev = PFR} = Content,
    NewContent = rabbit_basic:build_content(Props#'P_basic'{headers = Headers}, PFR),
    
    NewMessage = Msg#basic_message{content = NewContent},
    Delivery#delivery{message = NewMessage}.

get_msg(#delivery{message = Msg}) -> Msg.
get_content(#basic_message{content = Content}) -> Content.
get_props(#content{properties = Props}) -> Props.

add_new_headers(Headers, []) -> sort_headers(Headers);
add_new_headers(Headers, [H|Tail]) ->
    NewHeaders = add_new_header(Headers, H),
    add_new_headers(NewHeaders, Tail).

add_new_header(Table, { Key, string_constant, Value}) ->
    lists:keystore(Key, 1, Table, {Key, longstr, Value});
add_new_header(Table, { Key, approximate_number_constant, Value}) ->
    lists:keystore(Key, 1, Table, {Key, longstr, Value});
add_new_header(Table, { Key, decimal_constant, Value}) ->
    lists:keystore(Key, 1, Table, {Key, long, Value});
add_new_header(Table, { Key, integer_constant, Value}) ->
    lists:keystore(Key, 1, Table, {Key, long, Value});
add_new_header(Table, { Key, boolean_constant, Value}) ->
    lists:keystore(Key, 1, Table, {Key, bool, Value}).

sort_headers(Headers) ->
  lists:keysort(1, Headers).

convert_to_binary([Action | Tail]) ->
    [convert_to_binary(Action)] ++ convert_to_binary(Tail);
convert_to_binary([]) -> [];
convert_to_binary({ Key, { Type, Value }}) ->
    { list_to_binary(Key), Type, list_to_binary(Value) }.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

get_message_with_undefined_header_returns_empty_array_test() ->           
        Content = #content{properties = #'P_basic'{headers = undefined}},
        ?assertEqual([], get_message_header(Content)).

get_message_with_empty_header_test() ->    
    Content = #content{properties = #'P_basic'{headers = []}},
    ?assertEqual([], get_message_header(Content)).

get_message_header_test() ->   
        Headers = [{ <<"Name">>, longstr, <<"Nick">> }], 
        Content = #content{properties = #'P_basic'{headers = Headers}},
        ?assertEqual(Headers, get_message_header(Content)).

convert_to_binary_one_action_test() ->
    Action = { "Name", { string_constant, "Nick" } },
    BinaryAction = {<<"Name">>,string_constant,<<"Nick">>},
    ?assertEqual(BinaryAction, convert_to_binary(Action)).

convert_to_binary_several_actions_test() ->
    Actions = [ { "Name", { string_constant, "Nick" } }, { "LastName", { string_constant, "Carter" } }],
    BinaryActions = [ {<<"Name">>,string_constant,<<"Nick">>}, {<<"LastName">>,string_constant,<<"Carter">>} ],
    ?assertEqual(BinaryActions, convert_to_binary(Actions)).


add_new_headers_empty_test() ->
    ?assertEqual([], add_new_headers([], [])).

add_new_headers_with_one_action_test() ->
    Actions = [{ <<"Name">>, string_constant, <<"Nick">> }],
    Expected = [{ <<"Name">>, longstr, <<"Nick">> }],
    ?assertEqual(Expected, add_new_headers([], Actions)).

add_new_headers_with_two_actions_test() ->
        Actions = [{ <<"Name">>, string_constant, <<"Nick">> }, { <<"LastName">>, string_constant, <<"Carter">> }],
        Expected = [{ <<"LastName">>, longstr, <<"Carter">> }, { <<"Name">>, longstr, <<"Nick">> }],
        ?assertEqual(Expected, add_new_headers([], Actions)).
    
add_new_headers_update_value_if_exist_test() ->
    Actions = [{ <<"Name">>, string_constant, <<"NewName">> }],
    Headers = [{ <<"Name">>, longstr, <<"Nick">> }],
    Expected = [{ <<"Name">>, longstr, <<"NewName">> }],
    ?assertEqual(Expected, add_new_headers(Headers, Actions)).

-endif.

print(Name, Value) -> io:format(Name ++ ": ~p~n", [Value]).

%%====================================================================