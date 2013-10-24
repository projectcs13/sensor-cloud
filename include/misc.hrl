-ifndef(IF).
-define(IF(Cond, E1, E2), case Cond of 
			      true ->
				  E1;
			      false ->
				  E2
			  end).
-endif.

-ifndef(TO_STRING).
-define(TO_STRING(Arg), fun(X) when is_atom(X)      -> atom_to_list(X);
			   (X) when is_binary(X)    -> binary_to_list(X);
			   (X) when is_float(X)     -> float_to_list(X);			   
			   (X) when is_integer(X)   -> integer_to_list(X);
			   (X) when is_list(X)      -> X;
			   (X) when is_bitstring(X) -> bitstring_to_list(X)
			end(Arg)).
-endif.
