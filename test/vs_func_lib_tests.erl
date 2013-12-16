%% @author Tomas S�vstr�m <tosa7943@student.uu.se>
%% [www.csproj13.student.it.uu.se]
%% @version 1.0
%% @copyright [Copyright information]
%%
%% @doc == vs_func_lib_tests ==
%% This module contains several tests to test the functionallity
%% in the module vs_func_lib to test the data functions
%%
%% @end
-module(vs_func_lib_tests).

%% ====================================================================
%% API functions
%% ====================================================================
-export([]).
-include_lib("eunit/include/eunit.hrl").


%% ====================================================================
%% Internal functions
%% ====================================================================


%% @doc
%% Function: min_test/0
%% Purpose: Used test the min function
%% Returns: ok | {error, term()}
%%
%% @end
-spec min_test() -> ok | {error, term()}.

min_test() ->
	Data = [["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":4.0}",
			 "{\"timestamp\":\"2014-11-21T11:15:56.000\",\"value\":3.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":2.0}",
			 "{\"timestamp\":\"2014-11-24T10:15:56.000\",\"value\":1.0}"],
			["{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":-4.0}",
			 "{\"timestamp\":\"2014-11-25T10:15:56.000\",\"value\":-3.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":-2.0}",
			 "{\"timestamp\":\"2014-11-21T10:15:59.000\",\"value\":-1.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}"]],
	Result = vs_func_lib:min(Data,<<"adjkkvcj--sdffs">>),
	?assertEqual(["{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":1.0}"],Result).

%% @doc
%% Function: max_test/0
%% Purpose: Used test the max function
%% Returns: ok | {error, term()}
%%
%% @end
-spec max_test() -> ok | {error, term()}.

max_test() ->
	Data = [["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":4.0}",
			 "{\"timestamp\":\"2014-11-21T11:15:56.000\",\"value\":3.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":2.0}",
			 "{\"timestamp\":\"2014-11-24T10:15:56.000\",\"value\":1.0}"],
			["{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":-4.0}",
			 "{\"timestamp\":\"2014-11-25T10:15:56.000\",\"value\":-3.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":-2.0}",
			 "{\"timestamp\":\"2014-11-21T10:15:59.000\",\"value\":-1.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}"]],
	Result = vs_func_lib:max(Data,<<"adjkkvcj--sdffs">>),
	?assertEqual(["{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":5.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-1.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":5.0}"],Result).

%% @doc
%% Function: avg_test/0
%% Purpose: Used test the avg function
%% Returns: ok | {error, term()}
%%
%% @end
-spec avg_test() -> ok | {error, term()}.

avg_test() ->
	Data = [["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":4.0}",
			 "{\"timestamp\":\"2014-11-21T11:15:56.000\",\"value\":3.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":2.0}",
			 "{\"timestamp\":\"2014-11-24T10:15:56.000\",\"value\":1.0}"],
			["{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":-4.0}",
			 "{\"timestamp\":\"2014-11-25T10:15:56.000\",\"value\":-3.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":-2.0}",
			 "{\"timestamp\":\"2014-11-21T10:15:59.000\",\"value\":-1.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}"]],
	Result = vs_func_lib:mean(Data,<<"adjkkvcj--sdffs">>),
	?assertEqual(["{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-3.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":3.0}"],Result).

%% @doc
%% Function: sum_test/0
%% Purpose: Used test the total(sum) function
%% Returns: ok | {error, term()}
%%
%% @end
-spec sum_test() -> ok | {error, term()}.

sum_test() ->
	Data = [["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":4.0}",
			 "{\"timestamp\":\"2014-11-21T11:15:56.000\",\"value\":3.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":2.0}",
			 "{\"timestamp\":\"2014-11-24T10:15:56.000\",\"value\":1.0}"],
			["{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}",
			 "{\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":-4.0}",
			 "{\"timestamp\":\"2014-11-25T10:15:56.000\",\"value\":-3.0}",
			 "{\"timestamp\":\"2014-11-21T10:16:56.000\",\"value\":-2.0}",
			 "{\"timestamp\":\"2014-11-21T10:15:59.000\",\"value\":-1.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}"],
			["{\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":5.0}",
			 "{\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-5.0}"]],
	Result = vs_func_lib:total(Data,<<"adjkkvcj--sdffs">>),
	?assertEqual(["{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-11-21T10:15:56.000\",\"value\":0.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2015-11-21T10:15:56.000\",\"value\":-15.0}",
				  "{\"stream_id\":\"adjkkvcj--sdffs\",\"timestamp\":\"2014-12-21T10:15:56.000\",\"value\":15.0}"],Result).