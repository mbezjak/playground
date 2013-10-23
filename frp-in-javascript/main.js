// no FRP implementation
function doSearchPlain(value) {
    console.log('Searching for: %s', value);
}

function setupPlain() {
    $('#searchButton').click(function() {
        doSearch($('#searchInput').val());
    });

    $('#searchInput').keyup(function(e) {
        if (e.keyCode === 13) {
            doSearch($('#searchInput').val());
        }
    });
}

// FRP using Bacon.js
function doSearch(query) {
    var url = 'http://en.wikipedia.org/w/api.php?action=opensearch&format=json&search='
            + encodeURI(query);

    return Bacon.fromPromise($.ajax({ url: url, dataType: 'jsonp' }));
};

function setup() {
    var buttonStream = $('#searchButton').asEventStream('click');
    var enterStream  = $('#searchInput').asEventStream('keyup')
        .filter(function(e) { return e.keyCode === 13; });

    var searchStream = Bacon.mergeAll(buttonStream, enterStream)
        .map(function() { return $('#searchInput').val(); })
        .flatMapLatest(doSearch);

    searchStream.onValue(function(results) {
        console.log(results);
    });


    var totalSearches = searchStream.scan(0, function(value) {
        return value + 1;
    });
    totalSearches.onValue(function(value) {
        console.log('Total searches: %d', value);
    });

    var totalResults = searchStream.map(function(results) {
        return results[1].length;
    }).toProperty();
    totalResults.onValue(function(total) {
        console.log('Total results: %d', total);
    });
}

$(document).ready(setup);
