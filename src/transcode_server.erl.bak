%%%-------------------------------------------------------------------
%%% File : gen_server_template.erl
%%% Author : my name <yourname@localhost.localdomain>
%%% Description :
%%%
%%% Created : 2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(transcode_server).

-behaviour(gen_server).

-include("commons.hrl").

-define(SERVER, ?MODULE).

%% API
-export([get_clip_info/1, get_job_info/1, get_jobs/0,
	 start_job/2, start_link/0, update_progress/2]).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(state,
	{transcode_jobs, job_descriptors, clip_info_cache}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    ok = filelib:ensure_dir((?TRANSCODE_FOLDER) ++ "/"),
    ok = filelib:ensure_dir((?RUNNING_FOLDER) ++ "/"),
    ok = filelib:ensure_dir((?OLDJOB_FOLDER) ++ "/"),
    ok = filelib:ensure_dir((?DONE_FOLDER) ++ "/"),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [],
			  []).

update_progress(Filename, Progress) ->
    gen_server:cast(?SERVER,
		    {update_progress, Filename, Progress}).

start_job(File, Params) ->
    gen_server:call(?SERVER, {start_job, File, Params}).

get_clip_info(File) ->
    {ok, ClipInfo} = gen_server:call(?SERVER,
				     {get_clip_info, File}),
    ClipInfo.

get_job_info(File) ->
    {ok, JobInfo} = gen_server:call(?SERVER,
				    {get_job_info, File}),
    JobInfo.

get_jobs() ->
    AllClips = util:list_all_clips(),
    gen_server:call(?SERVER, {clear_caches, AllClips}),
    {ok, AllClips}.

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
init([]) ->
    process_flag(trap_exit, true),
    {ok,
     #state{transcode_jobs = map:new(jobs_by_pid),
	    job_descriptors = map:new(job_descriptors),
	    clip_info_cache = map:new(clip_info_cache)}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({start_job, Filename, Params}, _From,
	    State) ->
    case transcode_job:start_link(Filename) of
      {ok, Pid} ->
	  map:put(State#state.transcode_jobs, Pid, Filename),
	  map:put(State#state.transcode_jobs, Filename, Pid),
	  JobDescriptor = #job_descriptor{job_id = Filename,
					  job_pid = Pid, file_name = Filename,
					  clip_info =
					      get_cached_clip_info((?RUNNING_FOLDER)
								     ++
								     "/" ++
								       Filename,
								   State#state.clip_info_cache),
					  status = running,
					  start_date = util:current_time()},
	  map:put(State#state.job_descriptors, Filename,
		  JobDescriptor),
	  transcode_job:start_job(Pid, Params),
	  Reply = {ok, Pid};
      {error, Reason} -> Reply = {error, Reason}
    end,
    {reply, Reply, State};
handle_call({get_clip_info, Filename}, _From, State) ->
    {reply,
     {ok,
      get_cached_clip_info(Filename,
			   State#state.clip_info_cache)},
     State};
handle_call({get_job_info, Filename}, _From, State) ->
    case map:get(State#state.job_descriptors, Filename) of
      undefined ->
	  ResultFile = util:make_file(?DONE_FOLDER,
				      util:set_extension(Filename, "flv")),
	  case util:file_exists(ResultFile) of
	    true ->
		JobStatus = done,
		ResultSize = filelib:file_size(ResultFile);
	    false -> JobStatus = available, ResultSize = 0
	  end,
	  Descriptor = #job_descriptor{job_id = Filename,
				       file_name = util:file_name(Filename),
				       status = JobStatus,
				       clip_info =
					   get_cached_clip_info(Filename,
								State#state.clip_info_cache),
				       progress = #transcode_progress{},
				       result_file = util:file_name(ResultFile),
				       result_size = ResultSize};
      JobDescriptor ->
	  ResultFile = util:make_file(?DONE_FOLDER,
				      util:set_extension(Filename, "flv")),
	  case util:file_exists(ResultFile) of
	    true ->
		JobStatus = done,
		ResultSize = filelib:file_size(ResultFile);
	    false ->
		JobStatus = case JobDescriptor#job_descriptor.status of
			      running -> running;
			      _Other -> available
			    end,
		ResultSize = JobDescriptor#job_descriptor.result_size
	  end,
	  Descriptor = JobDescriptor#job_descriptor{status =
							JobStatus,
						    result_size = ResultSize}
    end,
    Descriptor,
    {reply, {ok, Descriptor}, State};
handle_call({clear_caches, ActiveClips}, _From,
	    State) ->
    StaleJobEntries =
	stale_entries(State#state.job_descriptors, ActiveClips),
    % remove stale job descriptors
    JobDescriptors = State#state.job_descriptors,
    [handle_call_1(V1, JobDescriptors)
     || V1 <- StaleJobEntries],
    % remove stale clip information
    ClipInfoCache = State#state.clip_info_cache,
    StaleClipEntries = stale_entries(ClipInfoCache,
				     ActiveClips),
    [handle_call_2(V2, ClipInfoCache)
     || V2 <- StaleClipEntries],
    {reply, ok, State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

handle_call_1(X, JobDescriptors) ->
    case map:get(JobDescriptors, X) of
      undefined -> ok;
      _Other ->
	  ?INFO("Removed stale entry ~s in job descriptors.",
		[X]),
	  map:remove(JobDescriptors, X)
    end.

handle_call_2(X, ClipInfoCache) ->
    case map:get(ClipInfoCache, X) of
      undefined -> ok;
      _Other ->
	  ?INFO("Removed stale entry ~s in clip info "
		"cache.",
		[X]),
	  map:remove(ClipInfoCache, X)
    end.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast({update_progress, Filename, Progress},
	    State) ->
    JobDescriptor = map:get(State#state.job_descriptors,
			    Filename),
    map:put(State#state.job_descriptors, Filename,
	    JobDescriptor#job_descriptor{progress = Progress}),
    {noreply, State};
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------

% a child has exited
handle_info({'EXIT', Pid, _Reason}, State) ->
    Filename = map:get(State#state.transcode_jobs, Pid),
    % update job descriptor
    JobDescriptor = map:get(State#state.job_descriptors,
			    Filename),
    CurrentTime = util:current_time(),
    ResultFile = util:make_file(?DONE_FOLDER,
				util:set_extension(Filename, "flv")),
    % change the ownership
    os:cmd("chown emisiuni_website:wwwdata \"" ++
	     ResultFile ++ "\""),
    ResultSize = filelib:file_size(ResultFile),
    map:put(State#state.job_descriptors, Filename,
	    JobDescriptor#job_descriptor{end_date = CurrentTime,
					 transcode_time =
					     CurrentTime -
					       JobDescriptor#job_descriptor.start_date,
					 result_file =
					     util:file_name(ResultFile),
					 result_size = ResultSize,
					 status = done}),
    case map:get(State#state.transcode_jobs, Pid) of
      Filename ->
	  map:remove(State#state.transcode_jobs, Filename),
	  map:remove(State#state.transcode_jobs, Pid);
      undefined -> ok
    end,
    {noreply, State};
handle_info(_Info, State) ->
    ?INFO("Received info: ~p", [_Info]), {noreply, State}.

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

get_cached_clip_info(Filename, ClipInfoCache) ->
    case map:get(ClipInfoCache, util:file_name(Filename)) of
      undefined ->
	  ClipInfo = clip:get_clip_info(Filename),
	  map:put(ClipInfoCache, util:file_name(Filename),
		  ClipInfo),
	  ClipInfo;
      ClipInfo -> ClipInfo
    end.

stale_entries(Cache, ActiveJobs) ->
    util:lists_diff(map:keys(Cache), ActiveJobs).
