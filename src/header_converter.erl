-module(header_converter).
-export([convert/1]).

convert([]) -> [];
convert([H|T]) -> 
    [convert_item(H)] ++ convert(T).

convert_item({ Property_Name, array, Values }) -> 
    Items = lists:foldl(fun({ Type, Value }, Acc) -> [get_value(Type, Value)] ++ Acc end, [], Values),
    { binary_to_list(Property_Name), Items };
convert_item({ Property_Name, Type, Value }) -> 
    { binary_to_list(Property_Name), get_value(Type, Value) }.

get_value(Type, Value) when Type == long orelse Type == bool -> 
    Value;
get_value(longstr, Value) -> 
    binary_to_list(Value).