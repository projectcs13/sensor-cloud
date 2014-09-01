%%here comes the definition of the structure of poller`s information

-record(pollerInfo, {stream_id,
					 name,
					 uri,
					 frequency,
					 data_type,
					 parser,
					 pid,
					 timer_ref}).


