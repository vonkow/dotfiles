-module(kata_solution).
-export([add/1]).
-import(string, [replace/4, split/3]).
-include_lib("eunit/include/eunit.hrl").

add("") -> 0;
add("//" ++ CustomDelim ++ "\n" ++ Input) ->
    CustomDelimsReplaced = replace(Input, CustomDelim, ",", all),
    add(CustomDelimReplaced);
add (Input) ->
    NewlinesReplaced = replace(Input, "\n", ",", all),
    addNums(NewlinesReplaced).

addNums(Nums) ->
    NumList = split(NewlinesReplaced, ",", all),
    addNums(0, NumList).

addNums(Total, []) ->
    Total;
addNums(Total, [Num | Nums]) ->
    addNums(Total + Num, Nums).

add_test_() ->
    [?assert(add("") =:= 0),
     ?assert(add("10") =:= 10),
     ?assert(add("5,6") =:= 11),
     ?assert(add("5,6,7") =:= 18),
     ?assert(add("5,6\n7,8\n9") =:= 35),
     ?assert(add("//;\n1;2;3") =:= 6)
    ].
