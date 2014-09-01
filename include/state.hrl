%%here comes the state`s information of the gen_server

-record(state, {stream_id,
				uri,
				parser,
				data_type,
				channel,
				exchange,
				connection}).