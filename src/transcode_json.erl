%%%-------------------------------------------------------------------
%%% File : gen_server_template.erl
%%% Author : my name <yourname@localhost.localdomain>
%%% Description :
%%%
%%% Created : 2 Mar 2007 by my name <yourname@localhost.localdomain>
%%%-------------------------------------------------------------------
-module(transcode_json).

-behaviour(gen_server).

%% API
-export([start_link/0]).

-include("commons.hrl").

-include_lib("rfc4627_jsonrpc/include/rfc4627_jsonrpc.hrl").

-define(SERVER, ?MODULE).

-define(SPROC(Name),
	#service_proc{name = Name, idempotent = true,
		      params = []}).

-define(SPROC(Name, Param1, Type1),
	#service_proc{name = Name, idempotent = true,
		      params =
			  [#service_proc_param{name = Param1, type = Type1}]}).

%% gen_server callbacks
-export([code_change/3, handle_call/3, handle_cast/2,
	 handle_info/2, init/1, terminate/2]).

-record(state, {}).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: start_link() -> {ok,Pid} | ignore | {error,Error}
%% Description: Starts the server
%%--------------------------------------------------------------------
start_link() ->
    {ok, Pid} = gen_server:start_link({local, ?SERVER},
				      ?MODULE, [], []),
    rfc4627_jsonrpc:register_service(Pid,
				     rfc4627_jsonrpc:service(<<"transcode">>,
							     <<"urn:uuid:afe1ccb5-23b0-49dd-a74a-9155535c96b2">>,
							     <<"1.0">>,
							     [?SPROC(<<"start_lq_job">>,
								     <<"name">>,
								     <<"str">>),
							      ?SPROC(<<"start_hq_job">>,
								     <<"name">>,
								     <<"str">>),
							      ?SPROC(<<"get_clip_info">>,
								     <<"name">>,
								     <<"str">>),
							      ?SPROC(<<"get_job_info">>,
								     <<"name">>,
								     <<"str">>),
							      ?SPROC(<<"get_jobs">>)])),
    {ok, Pid}.

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
init([]) -> {ok, #state{}}.

%%--------------------------------------------------------------------
%% Function: %% handle_call(Request, From, State) -> {reply, Reply, State} |
%% {reply, Reply, State, Timeout} |
%% {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, Reply, State} |
%% {stop, Reason, State}
%% Description: Handling call messages
%%--------------------------------------------------------------------

handle_call({jsonrpc, <<"get_clip_info">>, _ModData,
	     [Filename]},
	    _From, State) ->
    ClipFile = binary_to_list(Filename),
    ClipInfo = transcode_server:get_clip_info(ClipFile),
    ClipInfoForJson = {obj,
		       clip_info_to_json_array(ClipInfo)},
    ClipInfoJson = rfc4627:encode(ClipInfoForJson),
    {reply, {result, list_to_binary(ClipInfoJson)}, State};
handle_call({jsonrpc, <<"get_job_info">>, _ModData,
	     [Filename]},
	    _From, State) ->
    ClipFile = binary_to_list(Filename),
    JobInfo = transcode_server:get_job_info(ClipFile),
    JobInfoForJson = {obj, job_info_to_json_array(JobInfo)},
    JobInfoJson = rfc4627:encode(JobInfoForJson),
    {reply, {result, list_to_binary(JobInfoJson)}, State};
handle_call({jsonrpc, <<"start_hq_job">>, _ModData,
	     [Filename]},
	    _From, State) ->
    ClipFile = binary_to_list(Filename),
    ?INFO("Started high-quality encoding of ~s "
	  ": ~p",
	  [ClipFile,
	   transcode_server:start_job(ClipFile, ?HQ_PARAMS)]),
    {reply, {result, <<"ok">>}, State};
handle_call({jsonrpc, <<"start_lq_job">>, _ModData,
	     [Filename]},
	    _From, State) ->
    ClipFile = binary_to_list(Filename),
    ?INFO("Started low-quality encoding of ~s : ~p",
	  [ClipFile,
	   transcode_server:start_job(ClipFile, ?LQ_PARAMS)]),
    {reply, {result, <<"ok">>}, State};
handle_call({jsonrpc, <<"get_jobs">>, _ModData, []},
	    _From, State) ->
    %?INFO("Module data: ~p", [_ModData]),
    {ok, Jobs} = transcode_server:get_jobs(),
    JobsWithInfo = [{obj,
		     job_info_to_json_array(transcode_server:get_job_info(X))}
		    || X <- Jobs],
    JobsJson = rfc4627:encode(JobsWithInfo),
    {reply, {result, list_to_binary(JobsJson)}, State};
handle_call({jsonrpc, Method, _ModData, _Params}, _From,
	    State) ->
    {reply,
     {result, <<"Unhandled method ", Method/binary>>},
     State};
handle_call(_Request, _From, State) ->
    Reply = ok, {reply, Reply, State}.

%%--------------------------------------------------------------------
%% Function: handle_cast(Msg, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling cast messages
%%--------------------------------------------------------------------
handle_cast(_Msg, State) -> {noreply, State}.

%%--------------------------------------------------------------------
%% Function: handle_info(Info, State) -> {noreply, State} |
%% {noreply, State, Timeout} |
%% {stop, Reason, State}
%% Description: Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) -> {noreply, State}.

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

progress_to_json_array(undefined) ->
    progress_to_json_array(#transcode_progress{});
progress_to_json_array(Progress) ->
    [{frame,
      util:list_to_number(Progress#transcode_progress.frame)},
     {fps,
      util:list_to_number(Progress#transcode_progress.fps)},
     {q, util:list_to_number(Progress#transcode_progress.q)},
     {size,
      util:list_to_number(Progress#transcode_progress.size)},
     {time,
      util:list_to_number(Progress#transcode_progress.time)},
     {bitrate,
      util:list_to_number(Progress#transcode_progress.bitrate)}].

clip_info_to_json_array(undefined) ->
    clip_info_to_json_array(#clip_info{});
clip_info_to_json_array(ClipInfo) ->
    [%{file_name, list_to_binary(ClipInfo#clip_info.file_name)},
     {duration,
      util:list_to_number(ClipInfo#clip_info.duration)},
     {vcodec,
      util:list_to_binary_safe(ClipInfo#clip_info.vcodec)},
     {acodec,
      util:list_to_binary_safe(ClipInfo#clip_info.acodec)},
     {vb, util:list_to_number(ClipInfo#clip_info.vb)},
     {ab, util:list_to_number(ClipInfo#clip_info.ab)},
     {vsize,
      util:list_to_binary_safe(ClipInfo#clip_info.vsize)},
     {afps,
      util:list_to_binary_safe(ClipInfo#clip_info.afps)},
     {vfps,
      util:list_to_binary_safe(ClipInfo#clip_info.vfps)},
     {file_size,
      util:list_to_number(ClipInfo#clip_info.file_size)}].

job_info_to_json_array(JobInfo) ->
    [{job_id,
      list_to_binary(JobInfo#job_descriptor.job_id)},
     {file_name,
      list_to_binary(util:file_name(JobInfo#job_descriptor.job_id))},
     {start_date,
      util:list_to_number(JobInfo#job_descriptor.start_date)},
     {end_date,
      util:list_to_number(JobInfo#job_descriptor.end_date)},
     {transcode_time,
      util:list_to_number(JobInfo#job_descriptor.transcode_time)},
     {status, JobInfo#job_descriptor.status},
     {result_file,
      list_to_binary(JobInfo#job_descriptor.result_file)},
     {result_size,
      util:list_to_number(JobInfo#job_descriptor.result_size)},
     {time_now, util:current_time()}]
      ++
      progress_to_json_array(JobInfo#job_descriptor.progress)
	++
	clip_info_to_json_array(JobInfo#job_descriptor.clip_info).
