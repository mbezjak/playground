require('jasmine-check').install();

describe('using testcheck', function() {

    check.it('should fail', [gen.int], function(s) {
        expect(s).toEqual(jasmine.any(String));
    });

    check.it('should succeed but jasmine will complain about no expectations', [gen.string], function(s) {
        expect(s).toEqual(jasmine.any(String));
    });

    check.it('should test some non trivial property', [gen.array(gen.int), gen.strictPosInt], function(array, amount) {
        var added = array.map(function(x) {
            return x + amount;
        });
        var sumTwo = function(x, y) { return x + y; };
        var sumArray = function(a) { return a.reduce(sumTwo, 0); };

        expect(sumArray(added)).toBe(sumArray(array) + array.length * amount);
    });

});
