[...document.querySelectorAll('.itemCnt')].filter(function(e) {
    var priceEl = e.querySelector('.priceVal');
    return priceEl == null || priceEl.innerText == '0.00';
}).forEach(function(e) {
    e.remove();
});
