%%here comes the definition of the structure of poller`s information

-record(pollerInfo, {resourceid,
					 name,
					 url,
					 frequency,
					 pid}).
