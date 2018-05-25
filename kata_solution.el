-module(kata_solution).
-export([add/1]).
-import(string, [replace/4, split/3]).
-include_lib("eunit/include/eunit.hrl").

add("") -> 0;
add("//" + CustomDelim + "\n" + Input) ->
    CustomDelimsReplaced = replace(Input, CustomDelim, ",", all),
    add(CustomDelimReplaced);
add (Input) ->
    NewlinesReplaced = replace(Input, "\n", ",", all),
    addNums(NewlinesReplaced).

addNums(Nums) ->
    NumList = split(NewlinesReplaced, ",", all),
    addNums(0, NumList);
addNums(Total, []) ->
    Total;
addNums(Total, [Num | Nums]) ->
    add(Total + Num, Nums).

add_test_() ->
    [?assert(add("") =:= 0),
     ?assert(add("10") =:= 10),
     ?assert(add("5,6") =:= 11),
     ?assert(add("5,6,7") =:= 18),
     ?assert(add("5,6\n7,8\n9") =:= 35),
     ?assert(add("//;\n1;2;3") =:= 6)
    ].

add_1("") -> 0;
add_1("//" + CustomDelim + "\n" + Input) ->
    add_1(Input, [",", [\n], CustomDelim]);
add_1(Input) ->
    add_1(Input, [",", [\n]]);
add_1(Input, Delims) ->
    NumList = string:lexemes(Input, Delims),
    addNums(NumList).

add_2("") -> 0;
add_2("//" + Delim + "\n" + Nums) -> add_2(Nums, Delim);
add_2(Nums) -> add_2(Nums, "");
add_2(Nums, CustomDelim) ->
    NumList = string:lexemes(Nums, CustomDelim ++ "," ++ [$\n]),
    lists:foldl(fun(X, Sum) -> 
        Sum + string:to_integer(X) 
    end, 0, NumList).

