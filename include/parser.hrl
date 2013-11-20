%% Not finished.. needs update
%% current parser record only contains the information needed for parsing json data

%% input_parser is the path to the value we want to extract
%% for instance, streams/temperature/value

%% input_type is the datatype to which this datatype is designed.
%% for instance, application/json

-record(parser, {stream_id,
				 input_parser,
				 input_type}).