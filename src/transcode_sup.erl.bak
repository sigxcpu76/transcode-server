-module(transcode_sup).

-behaviour(supervisor).

-include("commons.hrl").

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type),
	{I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Logger = (?CHILD(logger, worker)),
    TranscodeServer = (?CHILD(transcode_server, worker)),
    TranscodeJsonServer = (?CHILD(transcode_json, worker)),
    {ok,
     {{one_for_one, 5, 10},
      [Logger, TranscodeServer, TranscodeJsonServer]}}.
