-define(LOG (Level, Message), logger:log(?MODULE, ?LINE, Level, Message, [])).
-define(LOG (Level, Message, LogData), logger:log(?MODULE, ?LINE, Level, Message, LogData)).

-ifdef(DEBUGRUN).

-define(DEBUG (Message), ?LOG(debug, Message)).
-define(DEBUG (Message, LogData), ?LOG(debug, Message, LogData)).

-else.

-define(DEBUG (Message), logger:dummy(Message)).
-define(DEBUG (Message, LogData), logger:dummy(Message, LogData)).

-endif. % ifdef DEBUGRUN


-define(WARN (Message), ?LOG(warn, Message)).
-define(WARN (Message, LogData), ?LOG(warn, Message, LogData)).

-define(INFO (Message), ?LOG(info, Message)).
-define(INFO (Message, LogData), ?LOG(info, Message, LogData)).

-define(ERROR (Message), ?LOG(error, Message)).
-define(ERROR (Message, LogData), ?LOG(error, Message, LogData)).
