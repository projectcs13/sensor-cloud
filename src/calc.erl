-module(calc).
-export([calculate/1, calculate/2]).
-import(string,[chr/2]).



calculate_use_variables(Expr, Variables) ->
    TokExp = lists:reverse(tokenize(Expr)),

    calculate_use_variables_(TokExp, Variables, []).

calculate_use_variables_(_TokExp, [], Acc) -> Acc;
calculate_use_variables_(TokExp, Variables, Acc) ->
    Res = eval_tokens(TokExp, Variables),
    NumVar = length(Variables),
    PopVar = pop_variables(Variables, []),
    NumPopVar = length(PopVar),

    if
        NumPopVar < NumVar ->  [Res|Acc];
        true -> calculate_use_variables_(TokExp, PopVar, [Res|Acc])
    end.


calculate(Expr) -> 
    eval_tokens(lists:reverse(tokenize(Expr)), []).
calculate(Expr, []) ->
	eval_tokens(lists:reverse(tokenize(Expr)), []);
calculate(Expr, Variables) ->
    calculate_use_variables(Expr, Variables).


eval_tokens([], _) -> 0;
eval_tokens([Single|[]], Variables) -> eval(Single, Variables);
eval_tokens([First, Second|[]], Variables) -> 
    case operator(First) of
        true -> unary(First, eval(Second, Variables));
        false-> unknown_token_set
    end
;
eval_tokens([One, Two, Three|Expr], Variables) ->
    case {operator(One), operator(Two), function(One)} of
        {true, _, _} -> 
            U = unary(One, eval(Two, Variables)),
            eval_tokens([U, Three|Expr], Variables);
        {false, true, _} ->
            B = binary(Two, eval(One, Variables), eval(Three, Variables)),
            eval_tokens([B|Expr], Variables)
    end.

pop_variables([], Acc) -> Acc;
pop_variables([{_Name, []}| Rest], Acc) -> pop_variables(Rest, Acc);
pop_variables([{_Name, [_H|[]]}| Rest], Acc) -> pop_variables(Rest, Acc);
pop_variables([{Name, [_H|T]}| Rest], Acc) -> pop_variables(Rest, [{Name, T}|Acc]).
   



eval(Tok, Variables) -> 
    case api_help:any_to_float(Tok)  of
        error ->
            case eval_variable(Tok, Variables) of
                error -> eval_tokens(Tok, Variables);
                Val -> Val 
            end;
        Val -> Val
    end.


tokenize([]) -> [];
tokenize(Expr) ->
	tokenize(Expr, []).

tokenize([], Acc) -> Acc;
tokenize(Expr, Acc) ->
	{T, Ex} = next_token(Expr),
	tokenize(Ex, [T|Acc]).


tokenize_parenthesis(["("|Expr]) -> tokenize_parenthesis(Expr, []);
tokenize_parenthesis(Expr) -> tokenize_parenthesis(Expr, []).

tokenize_parenthesis([], Acc) -> {lists:reverse(Acc), []};
tokenize_parenthesis([41], Acc) -> {lists:reverse(Acc), []};
tokenize_parenthesis([41|Expr], Acc) -> {lists:reverse(Acc), Expr};
tokenize_parenthesis(Expr, Acc) ->
    {T, Ex} = next_token(Expr),
    tokenize_parenthesis(Ex, [T|Acc]).


next_token([]) -> {0, []};
next_token([One|[]]) -> {[One], []};
next_token([First|Expr]) when First == 40 -> 
   tokenize_parenthesis(Expr);
next_token(Expr) ->
	Next = hd(Expr),

	case operator([Next]) of
		true ->
			{[Next], tl(Expr)};
		false -> 
		case get_chars(Expr)  of
			%{_, []} -> error;
			Reslut -> Reslut
		end
	end.


binary("+", Left, Right) -> Left + Right; 
binary("-", Left, Right) -> Left - Right;
binary("*", Left, Right) -> Left * Right;
binary("/", Left, Right) -> Left / Right;
binary("^", Left, Right) -> math:pow(Left, Right);
binary(_Op, _Left, _Right) -> 0.

unary("-", Arg) -> -Arg;
unary("+", Arg) -> Arg;
unary(_Op, _Arg) -> 0.

get_chars(Expr) -> get_chars(Expr, []).
get_chars([], Acc) -> 
    {Acc, []};
get_chars(Expr, Acc) ->
	First = hd(Expr),
	case operator([First]) of
		false ->
			get_chars(tl(Expr), lists:append(Acc, [First]));
		true -> {Acc, Expr}
	end.


function(_F) -> false. % lets start with the simple stuff first


% I was not as drunk when i wrote this as it seems (only one small glass of whiskey). It just sort of magically resolves all possible cases.
% On second thought, i might be slightly drunk. It works anyway.
% You may be tempted to 'beautify' this; save yourself the trouble and just don't.
operator([_,_|_]) -> false;
operator([T|_]) -> 
    case string:chr("+-*/^()", hd([T])) of
        0 -> false;
        _ -> true
    end;
operator(_) -> false.



eval_variable(_Tok, []) -> error;
eval_variable(Tok, [{Name, [Val|_More]}|Variables]) -> 
    case Name == Tok of
        true -> Val;
        false -> eval_variable(Tok, Variables)
    end.


