{application,webmachine_engine,
             [{description,"webmachine_engine"},
              {vsn,"1"},
              {modules,[restmachine_resource,static_resource,
                        webmachine_engine,webmachine_engine_app,
                        webmachine_engine_resource,webmachine_engine_sup]},
              {registered,[]},
              {applications,[kernel,stdlib,inets,crypto,mochiweb,webmachine]},
              {mod,{webmachine_engine_app,[]}},
              {env,[]}]}.