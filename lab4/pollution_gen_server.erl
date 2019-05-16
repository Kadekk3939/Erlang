-module(pollution_gen_server).
-behaviour(gen_server).

-export([getMonitor/0,
        addStation/2,
        addValue/4,
        removeValue/3,
        getOneValue/3,
        getStationMean/2,
        getDailyMean/2,
        getHourlyStationData/3
        ]).

-export([init/1,
        start/1,
        stop/0,
        crash/0, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2]).


% start/stop
start(InitValue) ->
    {ok, Pid} = gen_server:start_link(?MODULE, InitValue, []),
    register(pollution_server, Pid),
    {ok, Pid}.
stop() ->
    gen_server:call(pollution_server, terminate).
crash() ->
    gen_server:call(pollution_server, crash).


% callbacks
init(Monitor) ->
    {ok, Monitor}.

handle_call({get_Monitor}, _, Monitor) ->
    {reply, Monitor, Monitor};

handle_call({add_station, Name, Coords}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    NewMonitor = pollution:addStation(Monitor, Name, Coords),
    pollution_gen_state_server:set(NewMonitor),
    {reply, NewMonitor, NewMonitor};

handle_call({add_value, Input, Type, Value, Time}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    NewMonitor = pollution:addValue(Monitor, Input, Type, Value, Time),
    pollution_gen_state_server:set(NewMonitor),
    {reply, NewMonitor, NewMonitor};

handle_call({remove_value, Input, Type, Time}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    NewMonitor = pollution:removeValue(Monitor, Input, Type, Time),
    pollution_gen_state_server:set(NewMonitor),
    {reply, NewMonitor, NewMonitor};

handle_call({get_one_value, Input, Type, Time}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    Value = pollution:getOneValue(Monitor, Input, Type, Time),
    {reply, Value, Monitor};

handle_call({get_station_mean, Input, Type2}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    Value = pollution:getStationMean(Monitor, Input, Type2),
    {reply, Value, Monitor};

handle_call({get_daily_mean, Type2, Day2}, _, Monitor) ->
    Monitor = pollution_gen_state_server:get(),
    Value = pollution:getDailyMean(Monitor, Type2, Day2),
    {reply, Value, Monitor};

handle_call({get_hourly_station_data, Input, Type2, Hour2}, _, _) ->
    Monitor = pollution_gen_state_server:get(),
    Value = pollution:getHourlyStationData(Monitor, Input, Type2, Hour2),
    {reply, Value, Monitor};

handle_call(crash, _, Monitor) ->
    1/0,
    {reply, Monitor, Monitor};

handle_call(terminate, _, Monitor) -> {stop, normal, ok, Monitor}.

handle_cast(_, Monitor) ->
    {noreply, Monitor}.

handle_info(_, Monitor) ->
    io:format("Error~n"),
    {noreply, Monitor}.

terminate(normal, _) -> io:format("Terminated~n"), ok.


% 
getMonitor() ->
    gen_server:call(pollution_server, {get_Monitor}).

addStation(Name, Coords) ->
    gen_server:call(pollution_server, {add_station, Name, Coords}).

addValue(Input, Type, Value, Time) ->
    gen_server:call(pollution_server, {add_value, Input, Type, Value, Time}).

removeValue(Input, Type, Time) ->
    gen_server:call(pollution_server, {remove_value, Input, Type, Time}).

getOneValue(Input, Type, Time) ->
    gen_server:call(pollution_server, {get_one_value, Input, Type, Time}).

getStationMean(Input, Type2) ->
    gen_server:call(pollution_server, {get_station_mean, Input, Type2}).

getDailyMean(Type2, Day2) ->
    gen_server:call(pollution_server, {get_daily_mean, Type2, Day2}).

getHourlyStationData(Input, Type2, Hour2) ->
    gen_server:call(pollution_server, {get_hourly_station_data, Input, Type2, Hour2}).