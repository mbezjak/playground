window.yayQuery = function() {
    var yay = {};
    yay.sayHello = function(message) {
        console.log(message);
    };
    yay.getMessage = function() {
        return 'Hello, world!';
    };
    return yay;
};
