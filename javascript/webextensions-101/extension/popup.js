let links = document.getElementById('links');

chrome.tabs.executeScript(null, {
  file: 'extractor.js'
}, (results) => {
  let anchors = results[0];
  anchors.forEach((a) => {
    let tr = document.createElement('tr');
      let td_url = document.createElement('td');
        let link = document.createElement('a');
        link.href = a.href;
        link.target = '_blank';
        link.textContent = a.href;
        let br = document.createElement('br');
      td_url.append(link, br, a.text);

      let td_rel = document.createElement('td');
      td_rel.textContent = a.rel;
    tr.append(td_url, td_rel);
    links.append(tr);
  });
});
