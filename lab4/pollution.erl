-module(pollution).
-export([createMonitor/0,
         addStation/3,
         addValue/5,
         removeValue/4,
         getOneValue/4,
         getStationMean/3,
         getDailyMean/3,
         getHourlyStationData/4
        ]).

 %-record(station, {name,coordinates,measurements}).
 %-record(measurement, {type, value, time}).


createMonitor() -> [].

addStation([], Name, Coords) ->  [#{name => Name, coordinates => Coords, measurements => [] }];
addStation([#{name := Name}|_], Name, _) -> {error, station_exists};
addStation([#{coordinates := Coords}|_], _, Coords) -> {error, station_exists};
addStation([H|T], Name, Coords) -> [H|addStation(T,Name, Coords)].

addValue(Monitor, Input, Type, Value, Time) -> 
    {[Station],Monitor2} = getStation(Monitor, Input),
    #{measurements := Values} = Station,
    {[],_} = getValue(Values,Type,Time),
    NewValue = #{type => Type, value => Value, time => Time},
    NewStation = Station#{measurements := [NewValue | Values]},
    [NewStation | Monitor2].

removeValue(Monitor, Input, Type, Time) -> 
    {[Station],Monitor2} = getStation(Monitor, Input),
    #{measurements := Values} = Station,
    {[_],Values2} = getValue(Values, Type, Time),
    NewStation = Station#{values := Values2},
    [NewStation | Monitor2].

getOneValue(Monitor, Input, Type, Time) -> 
    {[Station],_} = getStation(Monitor, Input),
    #{measurements := Values} = Station,
    {[Value],_} = getValue(Values,Type,Time),
    Value.

getStationMean(Monitor, Input, Type2) ->
    {[Station], _} = getStation(Monitor, Input),
    #{measurements := Values} = Station,
    List = [Value || #{type := Type, value := Value} <- Values, 
    Type == Type2],
    mean(List).

getDailyMean(Monitor, Type2, Day2) ->
    List = [Value || #{measurements := Values} <- Monitor,
    #{type := Type, value := Value, time := {{_,_,Day},_}} <- Values,
    Type == Type2, Day == Day2],
    mean(List).

getHourlyStationData(Monitor, Input, Type2, Hour2) ->
    {[Station], _} = getStation(Monitor, Input),
    #{measurements := Values} = Station,
    List = [Value || #{type := Type, value := Value, time := {_,{Hour,_,_}} } <- Values, 
    Type == Type2, Hour == Hour2],
    mean(List).


 %pomocnicze funkcje
getStation(Monitor, Input) -> 
    Station = lists:filter(fun(#{name := Name, coordinates := Coords}) -> (Name == Input) or (Coords == Input) end, Monitor),
    {Station, Monitor -- Station}.

getValue(Input, Type2, Time2) -> 
    Value = lists:filter(fun(#{type := Type, time := Time}) -> (Type2 == Type) and (Time2 == Time) end, Input),
    {Value, Input -- Value}.

mean(List) ->
    Sum = lists:foldr(fun (X, Acc) -> Acc + X end, 0.0, List),
    Size =  lists:foldr(fun (_, Acc) -> Acc + 1 end, 0.0, List),
    case Size of
        0.0 -> 0;
        _ -> Sum/Size
    end.
%end
