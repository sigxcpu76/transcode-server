%%% -------------------------------------------------------------------
%%% Author  : sigxcpu
%%% Description :
%%%
%%% Created : Jan 24, 2011
%%% -------------------------------------------------------------------
-module(logger).

-behaviour(gen_server).

-compile({no_auto_import, [{error, 2}]}).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("logger.hrl").

%% --------------------------------------------------------------------
%% External exports
-export([start_link/0, start_link/1]).

-export([dummy/1, dummy/2, level_to_integer/1, log/3,
	 log/5, set_logging_level/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {logging_level}).

%% ====================================================================
%% External functions
%% ====================================================================

start_link() -> start_link(debug).

start_link(LoggingLevel) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE,
			  [LoggingLevel], []).

set_logging_level(Level) ->
    gen_server:call(?SERVER, {set_logging_level, Level}).

dummy(_) -> ok.

dummy(_, _) -> ok.

log(Module, Line, Level, LogMsg, LogData) ->
    gen_server:call(?SERVER,
		    {log_line, Module, Line, Level, LogMsg, LogData}).

log(Level, LogMsg, LogData) ->
    Caller = get_caller(),
    gen_server:call(?SERVER,
		    {log, Caller, Level, LogMsg, LogData}).

%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------
init([Level]) ->
    %?LOG(info, "Started logger"),
    io:format("Started logger~n"),
    {ok, #state{logging_level = Level}}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call({log, Caller, Level, LogMsg, LogValues},
	    _From, State) ->
    case is_loggable(Level, State) of
      true ->
	  io:format("~s ~7.7s ~s ",
		    [iso_8601_fmt(erlang:localtime()),
		     string:to_upper(atom_to_list(Level)), Caller]),
	  io:format(LogMsg, LogValues),
	  io:format("~n");
      _Other -> ok
    end,
    {reply, ok, State};
handle_call({log_line, Module, Line, Level, LogMsg,
	     LogData},
	    _From, State) ->
    case is_loggable(Level, State) of
      true ->
	  io:format("~s ~7.7s ~p:~p ",
		    [iso_8601_fmt(erlang:localtime()),
		     string:to_upper(atom_to_list(Level)), Module, Line]),
	  io:format(LogMsg, LogData),
	  io:format("~n");
      _Other -> ok
    end,
    {reply, ok, State};
handle_call({set_logging_level, LoggingLevel}, _From,
	    State) ->
    {reply, ok, State#state{logging_level = LoggingLevel}};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%% --------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%% --------------------------------------------------------------------
%%% Internal functions
%% --------------------------------------------------------------------

iso_8601_fmt(DateTime) ->
    {{Year, Month, Day}, {Hour, Min, Sec}} = DateTime,
    {_, _, Microseconds} = erlang:now(),
    Msec = Microseconds div 1000,
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:"
		  "~2.10.0B.~3.10.0B",
		  [Year, Month, Day, Hour, Min, Sec, Msec]).

get_caller() ->
    Trace = get_trace(),
    [{M, F, A} | _Rest] = [V1
			   || V1 <- Trace, get_caller_1(V1)],
    io_lib:format("~s:~s/~p", [M, F, A]).

get_caller_1(X) ->
    case {_M, _F, _A} = X of
      {?MODULE, _, _} -> false;
      _Other -> true
    end.

level_to_integer(error) -> 0;
level_to_integer(warn) -> 1;
level_to_integer(info) -> 2;
level_to_integer(debug) -> 3;
level_to_integer(trace) -> 4;
level_to_integer(_Other) -> 0.

is_loggable(Level, State) ->
    IntLevel = level_to_integer(Level),
    CurrentLevel =
	level_to_integer(State#state.logging_level),
    IntLevel =< CurrentLevel.

get_trace() ->
    _Trace = try throw(42) catch
	       42 -> erlang:get_stacktrace()
	     end.
