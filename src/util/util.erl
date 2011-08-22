-module(util).

-include("commons.hrl").

-export([current_time/0, dir_list/2,
	 dir_list_reversed/2, file_exists/1, file_exists/2,
	 file_name/1, find_file/1, find_file/2, list_all_clips/0,
	 list_all_clips/1, list_to_binary_safe/1,
	 list_to_number/1, lists_diff/2, make_file/2,
	 set_extension/2]).

current_time() ->
    {MS, S, _} = erlang:now(), 1000000 * MS + S.

list_to_number(undefined) -> 0;
list_to_number(N) when is_integer(N) -> N;
list_to_number(N) when is_float(N) ->
    round(N * 1000) / 1000;
list_to_number(S) when is_list(S) ->
    try A = list_to_float(S), round(A * 1000) / 1000 catch
      error:_Reason -> list_to_integer(S)
    end.

list_to_binary_safe(undefined) -> <<"undefined">>;
list_to_binary_safe([]) -> <<"undefined">>;
list_to_binary_safe(X) -> erlang:list_to_binary(X).

file_name(Filepath) ->
    case re:run(Filepath, ".*/(.*)", [{capture, [1], list}])
	of
      {match, [Filename]} -> Filename;
      nomatch -> Filepath
    end.

set_extension(Filename, Extension) ->
    case re:run(Filename, "(.*)\\.(.*)$") of
      {match, _} ->
	  re:replace(Filename, "(.*)\\.(.*)$",
		     "\\1." ++ Extension, [{return, list}]);
      nomatch -> Filename ++ "." + Extension
    end.

dir_list(Dir, Extensions) ->
    % convert extensions to lowercase
    Wildcard = Dir ++ "/" ++ "*",
    LowerExtensions = [string:to_lower(X)
		       || X <- Extensions],
    Result = [V1
	      || V1 <- filelib:wildcard(Wildcard),
		 dir_list_1(V1, LowerExtensions)],
    [util:file_name(X) || X <- Result].

dir_list_1(X, LowerExtensions) ->
    XLower = string:to_lower(X),
    filelib:is_regular(X) and
      lists:foldl(fun (X1, Acc) ->
			  Acc or (string:right(XLower, string:len(X1)) == X1)
		  end,
		  false, LowerExtensions).

dir_list_reversed(Dir, Extensions) ->
    FilteredFiles = dir_list(Dir, Extensions),
    lists:sort(fun (X, Y) ->
		       LastModifiedX =
			   calendar:datetime_to_gregorian_seconds(filelib:last_modified(Dir
											  ++
											  "/"
											    ++
											    X)),
		       LastModifiedY =
			   calendar:datetime_to_gregorian_seconds(filelib:last_modified(Dir
											  ++
											  "/"
											    ++
											    Y)),
		       LastModifiedY =< LastModifiedX
	       end,
	       FilteredFiles).

sort_entries_reversed_modified(Entries) ->
    FullEntries = [find_file(X) || X <- Entries],
    lists:sort(fun (X, Y) ->
		       LastModifiedX =
			   calendar:datetime_to_gregorian_seconds(filelib:last_modified(X)),
		       LastModifiedY =
			   calendar:datetime_to_gregorian_seconds(filelib:last_modified(Y)),
		       LastModifiedY =< LastModifiedX
	       end,
	       FullEntries).

find_file(File) -> find_file(File, ?ALL_FOLDERS).

find_file(File, Dirs) ->
    lists:foldl(fun (X, CurrentFile) ->
			case file_exists(X, File) of
			  true -> make_file(X, File);
			  _ -> CurrentFile
			end
		end,
		File, Dirs).

file_exists(File) -> filelib:is_regular(File).

file_exists(Dir, File) ->
    file_exists(make_file(Dir, File)).

make_file(Dir, Filename) -> Dir ++ "/" ++ Filename.

list_all_clips() -> list_all_clips(?INPUT_TYPES).

list_all_clips(Extensions) ->
    Entries = lists:usort(lists:foldl(fun (Entry, Acc) ->
					      Acc ++ dir_list(Entry, Extensions)
				      end,
				      [], ?ALL_FOLDERS)),
    FullEntriesSorted =
	sort_entries_reversed_modified(Entries),
    [file_name(X) || X <- FullEntriesSorted].

lists_diff(L1, L2) ->
    [V1 || V1 <- L1, lists_diff_1(V1, L2)].

lists_diff_1(X, L2) -> not lists:member(X, L2).

% test() ->
% 	A = ?record_to_json(job_descriptor, #job_descriptor{
% 		job_id = <<"Some job ID">>,
% 		file_name = <<"Some file name">>
% 	}),
% 	list_to_binary(A).

