%%%-------------------------------------------------------------------
%%% File : gen_server_template.erl
%%% Author : my name <yourname@localhost.localdomain>
%%% Description :
%%%
%%% Created : 2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(transcode_job).

-behaviour(gen_server).

-include("commons.hrl").

-include_lib("kernel/include/file.hrl").

-define(SERVER, ?MODULE).

%% API
-export([get_progress/1, get_status/1, start_job/2,
	 start_link/1, stop_job/1]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link(Filename) ->
    % check if file exists
    case file:read_file_info(util:find_file(Filename)) of
      {ok, #file_info{}} ->
	  gen_server:start_link(?MODULE, [Filename], []);
      {error, Reason} -> {error, Reason}
    end.

start_job(TranscodeJob, Params) ->
    gen_server:cast(TranscodeJob, {start_job, Params}).

stop_job(TranscodeJob) ->
    gen_server:cast(TranscodeJob, stop_job).

get_progress(TranscodeJob) ->
    gen_server:call(TranscodeJob, get_progress).

get_status(TranscodeJob) ->
    gen_server:call(TranscodeJob, get_status).

%%====================================================================
%% gen_server callbacks
%%====================================================================
%%--------------------------------------------------------------------
%% Function: init(Args) -> {ok, State} |
%% {ok, State, Timeout} |
%% ignore |
%% {stop, Reason}
%% Description: Initiates the server
%%--------------------------------------------------------------------
init([Filename]) ->
    % move the file to "running" folder
    ok = file:rename(util:find_file(Filename),
		     (?RUNNING_FOLDER) ++ "/" ++ Filename),
    % get the clip info for duration
    {ok,
     #tj_state{file_name = Filename, state = init,
	       progress = #transcode_progress{}}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call(get_progress, _From, State) ->
    {reply, {ok, State#tj_state.progress}, State};
handle_call(get_status, _From, State) ->
    {reply,
     {ok,
      {State#tj_state.state, State#tj_state.exit_status}},
     State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({start_job, Params}, State) ->
    InFile = State#tj_state.file_name,
    OutFile = util:set_extension(State#tj_state.file_name,
				 "flv"),
    ?INFO("Starting FLV encoding of ~s -> ~s",
	  [InFile, OutFile]),
    clip:transcode((?RUNNING_FOLDER) ++ "/" ++ InFile,
		   (?RUNNING_FOLDER) ++ "/" ++ OutFile, Params),
    {noreply, State#tj_state{state = running}};
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

handle_info({_Port, {data, {eol, Line}}}, State) ->
    case re:run(Line, "^frame=") of
      {match, _} ->
	  NewState = update_progress(State, Line),
	  % send updated progress to transcode server
	  transcode_server:update_progress(State#tj_state.file_name,
					   clip:transcode_progress(Line));
      nomatch -> NewState = State
    end,
    {noreply, NewState};
handle_info({_Port, {exit_status, Status}}, State) ->
    % rename the resulted files
    OriginalFile = State#tj_state.file_name,
    ok = file:rename((?RUNNING_FOLDER) ++
		       "/" ++ OriginalFile,
		     (?OLDJOB_FOLDER) ++ "/" ++ OriginalFile),
    FlvFile = util:set_extension(OriginalFile, "flv"),
    ok = file:rename((?RUNNING_FOLDER) ++ "/" ++ FlvFile,
		     (?DONE_FOLDER) ++ "/" ++ FlvFile),
    {stop, normal,
     State#tj_state{state = done, exit_status = Status}};
handle_info(_Info, State) ->
    %?INFO("Received info: ~p", [_Info]),
    {noreply, State}.

%%--------------------------------------------------------------------
%% Function: terminate(Reason, State) -> void()
%% Description: This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any necessary
%% cleaning up. When it returns, the gen_server terminates with Reason.
%% The return value is ignored.
%%--------------------------------------------------------------------
terminate(_Reason, _State) -> ok.

%%--------------------------------------------------------------------
%% Func: code_change(OldVsn, State, Extra) -> {ok, NewState}
%% Description: Convert process state when code is changed
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) -> {ok, State}.

%%--------------------------------------------------------------------
%%% Internal functions
%%--------------------------------------------------------------------

update_progress(State, ProgressLine) ->
    Progress = clip:transcode_progress(ProgressLine),
    State#tj_state{progress = Progress}.
