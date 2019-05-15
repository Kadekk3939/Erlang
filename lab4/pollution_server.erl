-module(pollution_server).
-export([createMonitor/0,
        addStation/2,
        addValue/4,
        removeValue/3,
        getOneValue/3,
        getStationMean/2,
        getDailyMean/2,
        getHourlyStationData/3
        ]).

-export([init/0,
        start/0,
        stop/0,
        crash/0]).

% server
%-record(station, {name,coordinates,measurements}).
%-record(measurement, {type, value, time}).

createMonitor() -> [].

start() ->
    Pid = spawn(pollution_server,init,[]),
    register(pollServer, Pid),
    Pid.

stop() ->
    pollServer ! {stop}.

init() ->
    loop(createMonitor()).

crash() ->
    pollServer ! {crash}.


%main server loop
loop(Monitor) -> 
    io:format("~p ~n ~n", [Monitor]),

    receive
        {crash} ->
            1/0,
            ok;

        {stop} ->
            io:format("Stopped", []),
            ok;
        
        {Pid, addStation, Name, Coords} ->
            P = pollution:addStation(Monitor, Name, Coords),
            Pid ! P,
            loop(P);

        {Pid, addValue, Input, Type, Value, Time} ->
            P = pollution:addValue(Monitor, Input, Type, Value, Time),
            Pid ! P,
            loop(P);

        {Pid, removeValue, Input, Type, Time} ->
            P = pollution:removeValue(Monitor, Input, Type, Time),
            Pid ! P,
            loop(P);

        {Pid, getOneValue, Input, Type, Time} ->
            P = pollution:getOneValue(Monitor, Input, Type, Time),
            Pid ! P,
            loop(Monitor);

        {Pid, getStationMean, Input, Type2} ->
            P = pollution:getStationMean(Monitor, Input, Type2),
            Pid ! P,
            loop(Monitor);

        {Pid, getDailyMean, Type2, Day2} ->
            P = pollution:getDailyMean(Monitor, Type2, Day2),
            Pid ! P,
            loop(Monitor);

        {Pid, getHourlyStationData, Input, Type2, Hour2} ->
            P = pollution:getHourlyStationData(Monitor, Input, Type2, Hour2),
            Pid ! P,
            loop(Monitor)

    end.

addStation(Name, Coords) ->
    pollServer ! {self(), addStation, Name, Coords},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.

addValue(Input, Type, Value, Time) ->
    pollServer ! {self(), addValue, Input, Type, Value, Time},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.

removeValue(Input, Type, Time) ->
    pollServer ! {self(), removeValue, Input, Type, Time},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.

getOneValue(Input, Type, Time) ->
    pollServer ! {self(), getOneValue, Input, Type, Time},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.

getStationMean(Input, Type2) ->
    pollServer ! {self(), getStationMean, Input, Type2},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.


getDailyMean(Type2, Day2) ->
    pollServer ! {self(), getDailyMean, Type2, Day2},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.

getHourlyStationData(Input, Type2, Hour2) ->
    pollServer ! {self(), getHourlyStationData, Input, Type2, Hour2},
    receive
        X -> X
    after 1000 ->
        error(timeouted)
    end.
