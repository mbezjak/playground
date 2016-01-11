require('jasmine-check').install();

describe('using testcheck', function() {

    check.it('accepts an int and a string', [gen.int, gen.string], function(x, y) {
        expect(x).toEqual(jasmine.any(Number));
        expect(y).toEqual(jasmine.any(String));
    });

});
