-module(action_reader).

%% API exports
-export([getList/1]).

%%====================================================================
%% API functions
getList(Text) ->
    case ar_expression_lexer:string(Text) of
        { ok, AST, _} -> 
                case ar_expression_parser:parse(AST) of
                    { ok, ActionList } -> ActionList;
                    Error              -> Error
                end;
        Error         -> Error
    end.
%%====================================================================


%%====================================================================
%% Internal functions
%%====================================================================
