-define (APP_NAME, transcode).
-define (APP_SUP_NAME, transcode_sup).

-define (TRANSCODE_FOLDER, "/Volumes/STORAGE/transcoding").
-define (RUNNING_FOLDER, ?TRANSCODE_FOLDER ++ "/" ++ "running").
-define (OLDJOB_FOLDER, ?TRANSCODE_FOLDER ++ "/" ++ "oldjobs").
-define (DONE_FOLDER, ?TRANSCODE_FOLDER ++ "/" ++ "done").

-define(ALL_FOLDERS, [?TRANSCODE_FOLDER, ?RUNNING_FOLDER, ?OLDJOB_FOLDER, ?DONE_FOLDER]).

-define (INPUT_TYPES, ["avi", "mpeg", "mpg", "wmv"]).

-define (FFMPEG_BIN, "/usr/bin/nice -n 15 /opt/local/bin/ffmpeg").
-define (SED_BIN, "/usr/bin/sed").
-define (GREP_BIN, "/usr/bin/grep").
-define (TR_BIN, "/usr/bin/tr").

-define (HQ_PARAMS, " -threads 4 -vf \"yadif=1:-1,crop=in_w-24:in_h-24,scale=640:480\" -vcodec libx264 -vpre veryfast -b 768k -async 9600 -vsync 1 -dts_delta_threshold 1 -strict experimental -acodec libfaac -ab 96k -f flv ").

-define (LQ_PARAMS, " -threads 4 -vf \"yadif=1:-1,crop=in_w-24:in_h-24,scale=480:360\" -vcodec libx264 -vpre hq -b 480k -async 9600 -vsync 1 -dts_delta_threshold 1 -strict experimental -acodec libfaac -ab 96k -f flv ").



