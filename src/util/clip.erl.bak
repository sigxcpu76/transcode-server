-module(clip).

-export([duration_to_seconds/1, get_clip_info/1,
	 get_progress/2, transcode/3, transcode_progress/1]).

-include("commons.hrl").

-include_lib("kernel/include/file.hrl").

get_clip_info(File) ->
    Filename = util:find_file(File),
    %OsInfoCmd = io_lib:format("/usr/bin/env ffmpeg -i \"~s\" 2>&1| /usr/bin/env sed -e 's/^[ \t]*//' | /usr/bin/env ggrep -E '^(Duration|Stream.*)' | tr '\n' ' '", [Filename]),
    OsInfoCmd = lists:flatten([?FFMPEG_BIN, " -i \"",
			       Filename, "\" 2>&1|", ?SED_BIN,
			       " -e 's/^[ \t]*//' |", ?GREP_BIN,
			       " -E '^(Duration|Stream.*)'|", ?TR_BIN,
			       " '\n' ' '"]),
    %?INFO("Info cmd: ~s", [OsInfoCmd]),
    ?INFO("Retrieving clip information for ~s", [Filename]),
    FileInfo = os:cmd(OsInfoCmd),
    %?INFO("Got clip info for ~s : ~p", [Filename, FileInfo]),
    Size = filelib:file_size(Filename),
    % now retrieve file information
    #clip_info{file_name = Filename,
	       duration =
		   duration_to_seconds(get_info(duration, FileInfo)),
	       vcodec = get_info(vcodec, FileInfo),
	       acodec = get_info(acodec, FileInfo),
	       vb = util:list_to_number(get_info(vb, FileInfo)),
	       ab = util:list_to_number(get_info(ab, FileInfo)),
	       afps = get_info(arate, FileInfo),
	       vfps = get_info(fps, FileInfo),
	       vsize = get_info(size, FileInfo), file_size = Size,
	       last_modified =
		   calendar:datetime_to_gregorian_seconds(filelib:last_modified(Filename))}.

transcode(Input, Output, Params) ->
    OsCommandList = [?FFMPEG_BIN, " -y -i \"", Input, "\" ",
		     Params, " \"", Output, "\" 2>&1 | ", ?TR_BIN,
		     " -u '\\r' '\\n'"],
    OsCommand = lists:flatten(OsCommandList),
    %io:format("Executing ~s~n", [OsCommand]),
    open_port({spawn, OsCommand},
	      [stream, exit_status, {line, 200}]).

transcode_progress(ProgressString) ->
    #transcode_progress{frame =
			    get_progress(frame, ProgressString),
			fps = get_progress(fps, ProgressString),
			q = get_progress(q, ProgressString),
			size = get_progress(size, ProgressString),
			time =
			    duration_to_seconds(get_progress(time,
							     ProgressString)),
			bitrate = get_progress(bitrate, ProgressString)}.

% internals

get_info(duration, Info) ->
    do_regex("Duration: ([0-9:.]+)", Info);
get_info(vb, Info) ->
    do_regex("bitrate: ([0-9]+)", Info);
get_info(vcodec, Info) ->
    do_regex("Video: ([a-zA-Z0-9_]+)", Info);
get_info(acodec, Info) ->
    do_regex("Audio: ([a-zA-Z0-9_]+)", Info);
get_info(size, Info) ->
    do_regex(" ([0-9]+x[0-9]+)[, ]", Info);
get_info(fps, Info) ->
    case do_regex(" ([0-9\\.]+) fps", Info) of
      undefined -> do_regex(" ([0-9\\.]+) tbr", Info);
      Other -> Other
    end;
get_info(arate, Info) ->
    do_regex(" ([a-zA-Z0-9_]+) Hz", Info);
get_info(ab, Info) ->
    do_regex("Audio:.* ([0-9]+) kb", Info).

get_progress(frame, Progress) ->
    do_regex("frame=\\s*(\\d+) ", Progress);
get_progress(fps, Progress) ->
    do_regex("fps=\\s*(\\d+) ", Progress);
get_progress(q, Progress) ->
    do_regex("q=\\s*([\\d\\.]+) ", Progress);
get_progress(size, Progress) ->
    do_regex("size=\\s*(\\d+)kB", Progress);
get_progress(time, Progress) ->
    do_regex("time=\\s*([\\d:\\.]+) ", Progress);
get_progress(bitrate, Progress) ->
    do_regex("bitrate=\\s*([\\d\\.]+)kbit", Progress).

do_regex(R, String) ->
    case do_regex(R, String, [1]) of
      {match, [Value]} -> Value;
      nomatch -> undefined
    end.

do_regex(R, String, Capture) ->
    re:run(String, R, [{capture, Capture, list}]).

duration_to_seconds(undefined) -> 0;
duration_to_seconds(D) ->
    {match, [SHours, SMinutes, SSeconds, SHundreds]} =
	re:run(D, "(\\d+):(\\d+):(\\d+)\\.(\\d+)",
	       [{capture, [1, 2, 3, 4], list}]),
    Hours = list_to_integer(SHours),
    Minutes = list_to_integer(SMinutes),
    Seconds = list_to_integer(SSeconds),
    _Hundreds = list_to_integer(SHundreds),
    Hours * 3600 + Minutes * 60 + Seconds.
