-module(kata_solution).
-export([add/1]).
-include_lib("eunit/include/eunit.hrl").

add("") -> 0;
add("//" + Delim + "\n" + Nums) -> add(Nums, Delim);
add(Nums) -> add(Nums, "");
add(Nums, CustomDelim) ->
    NumList = string:lexemes(Nums, CustomDelim ++ "," ++ [$\n]),
    lists:foldl(fun(X, Sum) -> 
        Sum + string:to_integer(X) 
    end, 0, NumList).

add_test_() ->
    [?assert(add("") =:= 0),
     ?assert(add("10") =:= 10),
     ?assert(add("5,6") =:= 11),
     ?assert(add("5,6,7") =:= 18),
     ?assert(add("5,6\n7,8\n9") =:= 35),
     ?assert(add("//;\n1;2;3") =:= 6)
    ].
