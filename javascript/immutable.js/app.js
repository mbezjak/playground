var person = Immutable.fromJS({
    firstName : 'John',
    lastName : 'Doe',
    age : {
        born : 1900,
        currentYears : 116
    },
    friends : [
        { name : 'Mark', age: 30 },
        { name : 'Jane' },
        { name : 'Susan' }
    ],
    address : {
        city : 'NY',
        country : 'USA',
        planet : 'Earth',
        galaxy : 'Milky Way',
        geolocation : [40.781388, -73.966143]
    }
});

console.log('Person as immutable = %o', person);
console.log('Person as javascript = %o', person.toJS());

var partial = Immutable.fromJS({
    friends : [
        { name : 'Philip' },
        { name : 'Mark', close: true }
    ],
    age : {
        currentYears : 123,
        alive : true
    },
    nickName : 'Johnny'
});

console.log('Merge with partial = %o', person.mergeDeep(partial).toJS());
