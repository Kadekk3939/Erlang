-module(pollution_supervisor).
-behaviour(supervisor).

-export([start/0, 
        init/1]).

start() ->
    supervisor:start_link(
      {local, ?MODULE},
      ?MODULE,
      pollution:createMonitor()).

init(InitValue) ->
    {ok, {
          {one_for_one, 3, 5},
          [{pollution_server_state,
            {pollution_gen_state_server, start, [InitValue]},
            permanent, 5000, worker, [pollution_gen_server]},

           {pollution_server,
             {pollution_gen_server, start, [InitValue]},
            permanent, 5000, worker, [pollution_gen_server]}
          ]

}
    }.