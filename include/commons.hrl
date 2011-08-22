-include ("constants.hrl").
-include ("logger.hrl").
-include ("jsonerl.hrl").

-record(clip_info, {
	file_name,
	duration,
	vcodec,
	acodec,
	vb,
	ab,
	vsize,
	afps,
	vfps,
	file_size,
	last_modified
}).

-record(transcode_progress, {
	frame = 0,
	fps = 0,
	q = 0,
	size = 0,
	time = 0,
	bitrate = 0
}).

-record(tj_state, {
	file_name,
	out_file,
	duration,
	state,
	progress,
	exit_status
}).


-record(job_descriptor, {
	job_id,
	job_pid,
	file_name,
	start_date,
	end_date,
	transcode_time,
	progress,
	clip_info,
	status,
	result_file = "",
	result_size = 0
}).
