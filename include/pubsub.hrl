-ifndef(__PUBSUB_HRL__).
-define(__PUBSUB_HRL__, 1).


	%% Definition of timeout to wait for new messages,
	%% primarily used for virtual streams. 
	-define(PUB_SUB_TIMEOUT, 1000).

	%% Takes an erlang:localtime() and converts it to the format:
	%% YYYY-MM-DDTHH:MM:SS.000
	-define(TIME_NOW(TIME),
		integer_to_list(element(1, element(1, TIME))) ++ "-" ++
			case element(2, element(1, TIME)) < 10 of
                true -> "0" ++ integer_to_list(element(2, element(1, TIME)));
                _ -> "" ++ integer_to_list(element(2, element(1, TIME)))
			end ++ "-" ++
			case element(3, element(1, TIME)) < 10 of
                true -> "0" ++ integer_to_list(element(3, element(1, TIME)));
                _ -> "" ++ integer_to_list(element(3, element(1, TIME)))
        	end ++
			"T" ++
			case element(1, element(2, TIME)) < 10 of
                true -> "0" ++ integer_to_list(element(1, element(2, TIME)));
                _ -> "" ++ integer_to_list(element(1, element(2, TIME)))
        	end ++ ":" ++
			case element(2, element(2, TIME)) < 10 of
                true -> "0" ++ integer_to_list(element(2, element(2, TIME)));
                _ -> "" ++ integer_to_list(element(2, element(2, TIME)))
        	end ++ ":" ++
			case element(3, element(2, TIME)) < 10 of
                true -> "0" ++ integer_to_list(element(3, element(2, TIME)));
                _ -> "" ++ integer_to_list(element(3, element(2, TIME)))
        	end ++ ".000"
		).


-endif.
