(function() {

    var ARRAY_SIZE = 20;
    var MAX_VALUE  = 100;

    var range = function(upto) {
        return Array.apply(null, Array(upto)).map(function(_,i) { return i; });
    };
    var randomValue = function() {
        return Math.round(MAX_VALUE * Math.random());
    };
    var randomArrayValues = function(size) {
        return range(size).map(randomValue);
    };


    var data = {
        labels   : range(ARRAY_SIZE),
        datasets : [{
            fillColor : "rgba(200,200,220,0.2)",
            data      : randomArrayValues(ARRAY_SIZE)
        }]
    };


    Chart.defaults.global.responsive = true;
    var canvas = document.getElementById("canvas");
    var ctx    = canvas.getContext("2d");
    var chart  = new Chart(ctx).Line(data);


    var updateAtEvent = function(event) {
        var points = chart.getPointsAtEvent(event);
        points[0].value = randomValue();

        chart.update();
    };


    var intervalId = null;
    var useDifferentValues = function() {
        chart.datasets[0].points.forEach(function(p) {
            p.value = randomValue();
        });
        chart.update();
    };
    var startDancing = function() {
        useDifferentValues();
        intervalId = setInterval(useDifferentValues, 3000);
    };
    var stopDancing = function() {
        clearInterval(intervalId);
        intervalId = null;
    };
    var toggleDancing = function() {
        (intervalId ? stopDancing : startDancing)();
    };

    canvas.addEventListener("click", updateAtEvent);
    document.getElementById("dance").addEventListener("click", toggleDancing);
}());
