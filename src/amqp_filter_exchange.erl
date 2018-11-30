%% The contents of this file are subject to the Mozilla Public License
%% Version 1.1 (the "License"); you may not use this file except in
%% compliance with the License. You may obtain a copy of the License
%% at http://www.mozilla.org/MPL/
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and
%% limitations under the License.
%%
%% The Original Code is RabbitMQ Consistent Hash Exchange.
%%
%% The Initial Developer of the Original Code is GoPivotal, Inc.
%% Copyright (c) 2007-2018 Pivotal Software, Inc.  All rights reserved.
%%

-module(amqp_filter_exchange).
-include_lib("rabbit_common/include/rabbit.hrl").
-include_lib("rabbit_common/include/rabbit_framing.hrl").

-behaviour(rabbit_exchange_type).

-export([description/0, serialise_events/0, route/2]).
-export([validate/1, validate_binding/2,
         create/2, delete/3, policy_changed/2,
         add_binding/3, remove_bindings/3, assert_args_equivalence/2]).
-export([info/1, info/2]).

-rabbit_boot_step(
   {?MODULE,
    [{description, "Amqp Filter Exchange"},
     {mfa,         {rabbit_registry, register,
                    [exchange, <<"x-filter">>, ?MODULE]}},
     {requires,    rabbit_registry},
     {enables,     kernel_ready},
     {cleanup,     {rabbit_registry, unregister,
                    [exchange, <<"x-filter">>]}}]}).

info(_X) -> [].
info(_X, _) -> [].

description() ->
    [{description, <<"Filter Exchange">>}].

serialise_events() -> false.

route(#exchange{name = Name}, #delivery{message = #basic_message{content = Content}} = Delivery) ->
    
    Routes =  match_exchange(Name),
    Headers = message_utils:get_message_header(Content),
    Properties = header_converter:convert(Headers),
    Subscriptions = filter(Routes, Properties),
    update_and_send(Delivery, Subscriptions),
    [].

validate(_X) -> ok.
validate_binding(_Exchange, _Binding) -> ok.
create(_Tx, _X) -> ok.
delete(_Tx, _X, _Bs) -> ok.
policy_changed(_X1, _X2) -> ok.
add_binding(_Tx, _X, _B) -> ok.
remove_bindings(_Tx, _X, _Bs) -> ok.
assert_args_equivalence(X, Args) ->
    rabbit_exchange:assert_args_equivalence(X, Args).


%%====================================================================
%% Internal functions

update_and_send(_, []) -> [];
update_and_send(Message, [Subscription | Tail]) ->
    update_and_send(Message, Subscription),
    update_and_send(Message, Tail);
update_and_send(Message, { Destination, Args }) ->
    NewMessage = message_utils:create_message(Message, Args),
    send_message(NewMessage, Destination)
    .

send_message(NewMessage, Destination) -> 
    Qs = rabbit_amqqueue:lookup([Destination]),
    rabbit_amqqueue:deliver(Qs, NewMessage).

match_exchange(Name) ->  
    MatchHead = #route{binding = #binding{
                                    source      = Name,
                                    destination = '$1',
                                    key         = '$2',
                                    args        = '$3'}},
    ets:select(rabbit_route, [{MatchHead, [], ['$$']}]).

filter(Routes, Properties) -> 
    [{ Destination, Args } || [Destination, Key, Args] <- Routes, evaluate(Key, Properties) ].

evaluate(Key, Properties) ->
    Response = amqp_filter:evaluate(binary_to_list(Key), Properties),
    case Response of
        unknown -> false;
        _ -> Response            
    end.

print(Name, Value) -> io:format(Name ++ ": ~p~n", [Value]).

%%====================================================================