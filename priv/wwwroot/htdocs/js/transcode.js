var transcodeService;
var jobStatusCache = {};

function initialUpdate() {
	transcodeService = new JsonRpcService(document.location + "rpc/transcode", onServiceReady);
}

function onServiceReady() {
	transcodeService.get_jobs().addCallback(createJobsTable);
	new PeriodicalExecuter(progressRefresh, 1);
}

function createJobsTable(jsonString, x) {
	jobs = JSON.parse(jsonString);

	jobsTable = $('movies');
	for(i = 0; i < jobs.length; i++) {
		jobsTable.appendChild(makeJobChild(i, jobs[i]));
	}
}

function makeJobChild(i, jobInfo) {
	li = document.createElement('li');
	(i % 2 == 0) ? li.className = 'movie_entry even' : li.className = 'movie_entry odd';
	li.id = jobInfo.job_id;

	divTitle = document.createElement('div');
	li.appendChild(divTitle);

	divTitle.className = 'title';
	divTitle.appendChild(document.createTextNode(jobInfo.file_name));

	divDesc = document.createElement('div');
	li.appendChild(divDesc);
	divDesc.className = 'description';

	movieInfo = jobInfo.vsize
				+ ", " + durationToTime(jobInfo.duration)
				+ ", video: " + jobInfo.vfps + 'fps, ' + jobInfo.vcodec + ', ' + jobInfo.vb + 'kbit'
				+ ', audio: ' + jobInfo.afps + 'Hz, ' + jobInfo.acodec + ', ' + jobInfo.ab + 'kbit';

	divDesc.appendChild(document.createTextNode(movieInfo));

	var divStatusLight = document.createElement('div');
	li.appendChild(divStatusLight);

	divStatusLight.className = "status_light";
	divStatusLight.id = 'light_' + jobInfo.job_id;
	divStatusLight.innerHTML = '&nbsp;';

	divStatus = document.createElement('div');
	li.appendChild(divStatus);
	divStatus.className = 'status';
	divStatus.id = 'status_' + jobInfo.job_id;

	updateStatus(divStatus, divStatusLight, jobInfo);

	// cache this status
	jobStatusCache[jobInfo.job_id] = jobInfo.status;

	br = document.createElement('br');
	li.appendChild(br);
	br.style.clear = 'left';

	//alert(li.innerHTML);

	divTitle = null;
	movieInfo = null;
	divDesc = null;
	divStatusLight = null;
	jobInfo = null;


	return li;
}

function updateStatus(divStatus, divStatusLight, jobInfo) {

	var status = jobInfo.status;


	if(status == 'done') {
		divStatusLight.style.backgroundColor = 'green';

		var input = document.createElement('input');
		input.type = 'button';
		input.name = jobInfo.result_file;
		input.value = 'Download';
		divStatus.appendChild(input);
		Event.observe(input, 'click', downloadClicked);

		divStatus.appendChild(document.createElement('br'));
		divStatus.appendChild(document.createTextNode(fuzzySize(jobInfo.result_size)));

	} else if(status == 'available') {

		divStatusLight.style.backgroundColor = 'yellow';

		input = document.createElement('input');
		input.type = 'button';
		input.name = jobInfo.job_id;
		input.value = 'HQ';
		divStatus.appendChild(input);
		Event.observe(input, 'click', transcodeHQClicked);

		divStatus.appendChild(document.createTextNode(' '));

		input = document.createElement('input');
		input.type = 'button';
		input.name = jobInfo.job_id;
		input.value = 'LQ';
		divStatus.appendChild(input);
		Event.observe(input, 'click', transcodeLQClicked);

	} else if(status == 'running') {
		divStatusLight.style.backgroundColor = 'red';
		if((jobInfo.duration) && (jobInfo.duration > 0)) {
			donePercent = Math.round(100 * 100 * jobInfo.time / jobInfo.duration) / 100;
			runTime = jobInfo.time_now - jobInfo.start_date;
			totalTime = 100 * runTime / donePercent;
			timeLeft = totalTime - runTime;

			divStatus.appendChild(createProgressBar(donePercent, durationToTime(timeLeft)));
		}




		//divStatus.appendChild(document.createTextNode(status));
	} else {
		divStatus.appendChild(document.createTextNode(status));
	}

}

function transcodeHQClicked(event) {
	Event.stopObserving(event.element());
	jobName = event.element().name;
	divStatus = $('status_' + jobName);
	inputs = divStatus.getElementsByTagName('input');
	for(i = 0; i < inputs.length; i++) {
		inputs[i].disable();
	}
	transcodeService.start_hq_job(event.element().name);
}

function transcodeLQClicked(event) {
	Event.stopObserving(event.element());
	jobName = event.element().name;
	divStatus = $('status_' + jobName);
	inputs = divStatus.getElementsByTagName('input');
	for(i = 0; i < inputs.length; i++) {
		inputs[i].disable();
	}
	transcodeService.start_lq_job(event.element().name);
}

function downloadClicked(event) {
	Event.stopObserving(event.element());
	event.element().disable();
	//url = '/multimedia/done/' + event.element().name;
	url = 'http://localhost/~sigxcpu/' + event.element().name;
	window.open(url, 'Download');
	event.element().enable();
}


// function cbTranscodeSent(jsonString, x) {
// 	transcodeService.get_available_jobs().addCallback(cbUpdateAvailable);
// 	transcodeService.get_running_jobs().addCallback(cbUpdateRunning);
// 	transcodeService.get_done_jobs().addCallback(cbUpdateDone);
// }


function progressRefresh() {
	transcodeService.get_jobs().addCallback(updateProgress);
}

function updateProgress(jsonString, x) {
	jobs = JSON.parse(jsonString);
	jobsTable = $('movies');

	// received jobs
	gotJobs = [];
	for(i = 0; i < jobs.length; i++) {
		gotJobs.push(jobs[i].job_id);
	}

	// current displayed jobs
	displayedJobs = [];
	displayedElements = document.getElementsByClassName('movie_entry');
	for(i = 0; i < displayedElements.length; i++) {
		displayedJobs.push(displayedElements[i].id);
	}

	// remove not received jobs
	for(i = 0; i < displayedJobs.length; i++) {
		if(gotJobs.indexOf(displayedJobs[i]) < 0) {
			removeEntry(displayedJobs[i]);
		}
	}

	// add new jobs
	for(i = 0; i < jobs.length; i++) {
		mustUpdate = false;
		if(displayedJobs.indexOf(jobs[i].job_id) < 0) {
			li = makeJobChild(i, jobs[i]);
			jobsTable.insertBefore(li, jobsTable.children[0]);
			mustUpdate == true;
		}

		jobId = jobs[i].job_id;
		status = jobs[i].status;
		if(status == 'running') mustUpdate = true;

		if(mustUpdate || (jobStatusCache[jobId] != status)) {
			jobStatusCache[jobId] = status;
			// update progress
			divStatus = $('status_' + jobs[i].job_id);
			divStatusLight = $('light_' + jobs[i].job_id);
			if(divStatus) {
				descendants = divStatus.descendants();
				for(j = 0; j < descendants.length; j++) {
					//alert('Unobserving ' + descendants[j]);
					Event.stopObserving(descendants[j]);
				}

				divStatus.innerHTML = '';
				updateStatus(divStatus, divStatusLight, jobs[i]);
			} else {
				alert('No status div for ' + jobs[i].job_id + "!");
			}
		}

	}

	updateRowColors();

}

function updateRowColors() {
	movies = $('movies');
	for(i = 0; i < movies.children.length; i++) {
		li = movies.children[i];
		(i % 2 == 0) ? li.className = 'movie_entry even' : li.className = 'movie_entry odd';
	}
}

function removeEntry(entryName) {
	ul = $('movies');
	ul.removeChild($(entryName));
}

function updateProgressItem(jobInfo) {
	jobId = jobInfo.file_name;

	td = $(jobId);

	if(td) {
		td.innerHTML = '';
		if((jobInfo.duration) && (jobInfo.duration > 0)) {
			donePercent = Math.round(100 * 100 * jobInfo.time / jobInfo.duration) / 100;
			runTime = jobInfo.time_now - jobInfo.start_date;
			totalTime = 100 * runTime / donePercent;
			timeLeft = totalTime - runTime;

			td.appendChild(createProgressBar(donePercent, durationToTime(timeLeft)));

		}
	}
}


function durationToTime(x) {
	if(x == Infinity) {
		return '99:99:99';
	}
	if(x) {

		hours = Math.floor(x / 3600);
		minutes = Math.floor((x - hours * 3600) / 60);
		seconds = x - 3600 * hours - 60 * minutes;

		sHours = '' + hours;
		if(hours < 10) sHours = '0' + hours;
		sMinutes = '' + minutes;
		if(minutes < 10) sMinutes = '0' + minutes;
		sSeconds = '' + Math.round(seconds);
		if(seconds < 10) sSeconds = '0' + Math.round(seconds);

		return sHours + ':' + sMinutes + ':' + sSeconds;
	} else {
		 return '00:00:00';
	}
}

function twoDigits(x) {
	return Math.round(x * 100) / 100;

}

function fuzzySize(x) {
	if(x <= 0) return '0 KB';
	if(x <= 1024) return x + ' B';
	if(x <= 1048576) return twoDigits(x) + ' KB';
	if(x <= 1024 * 1048576) return twoDigits(x/1048576) + ' MB';
	return twoDigits(x/1024/1048576) + ' GB';
}

function createProgressBar(percent, text) {
	divProgress = document.createElement('div');
	divProgress.className = 'progressBar';

	divPercent = document.createElement('div');
	divPercent.className = 'progressPercent';
	divProgress.appendChild(divPercent);

	divText = document.createElement('div');
	divText.className = 'progressText';
	divProgress.appendChild(divText);

	divPercent.style.width = percent + '%';
	divText.innerHTML = text;
}
