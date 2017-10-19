(function() {
  let anchors = document.querySelectorAll('a[href]');
  let rv = [];
  anchors.forEach((a) => {
    rv.push({
      href: a.href,
      rel: a.rel,
      text: a.textContent
    });
  });
  return rv;
// sends the above `rv` as a return value...basically
})();
