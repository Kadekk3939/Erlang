-module(pollution_gen_state_server).
-behaviour(gen_server).

-export([start/1,
        stop/0,
        get/0,
        set/1, 
        init/1, 
        handle_call/3, 
        handle_cast/2, 
        handle_info/2, 
        terminate/2]).


start(InitValue) ->
    {ok, Pid} = gen_server:start_link(?MODULE, InitValue, []),
    register(pollution_server_state, Pid),
    {ok, Pid}.
stop() ->
    gen_server:call(pollution_server_state, terminate).


% ======= CALLBACKS
init(State) ->
    {ok, State}.

handle_call({get}, _, State) ->
    {reply, State, State};
handle_call({set, NewState}, _, _) ->
    {reply, NewState, NewState};

handle_call(terminate, _, State) -> {stop, normal, ok, State}.
handle_cast(_, State) -> {noreply, State}.
handle_info(_, _) -> io:format("Error~n").
terminate(normal, _) -> io:format("Terminated~n"), ok.

% ======= API
get() ->
    gen_server:call(pollution_server_state, {get}).
set(NewState) ->
    gen_server:call(pollution_server_state, {set, NewState}).
