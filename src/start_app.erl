-module(start_app).

-include("commons.hrl").

-export([start/0]).

start() ->
    application:load(?APP_NAME),
    application:start(?APP_NAME).
