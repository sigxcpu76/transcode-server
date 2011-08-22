function createProgressBar(percent, text) {
	div = document.createElement('div');

	div.className = 'progressBar';

	pDiv = document.createElement('div');
	pDiv.className = 'progressPercent';
	pDiv.style.width = percent + '%';

	div.appendChild(pDiv);

	tDiv = document.createElement('div');
	tDiv.className = 'progressText';
	tDiv.appendChild(document.createTextNode(text));

	div.appendChild(tDiv);

	return div;
}
