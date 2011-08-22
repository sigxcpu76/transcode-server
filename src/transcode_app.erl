-module(transcode_app).

-behaviour(application).

-include("commons.hrl").

%% Application callbacks
-export([start/0, start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start() -> (?APP_SUP_NAME):start_link().

start(_StartType, _StartArgs) ->
    (?APP_SUP_NAME):start_link().

stop(_State) -> ok.
