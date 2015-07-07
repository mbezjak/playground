Ext.onReady(function() {
    Ext.QuickTips.init();

    var sampleData = [
        ['3m Co',                               71.72, 'user'],
        ['Alcoa Inc',                           29.01, 'user'],
        ['Altria Group Inc',                    83.81, 'user'],
        ['American Express Company',            52.55, 'user'],
        ['American International Group, Inc.',  64.13, 'user'],
        ['AT&T Inc.',                           31.61, 'user'],
        ['Boeing Co.',                          75.43, 'user'],
        ['Caterpillar Inc.',                    67.27, 'admin'],
        ['Citigroup, Inc.',                     49.37, 'admin'],
        ['E.I. du Pont de Nemours and Company', 40.48, 'admin'],
        ['Exxon Mobil Corp',                    68.10, 'admin'],
        ['General Electric Company',            34.14, 'admin'],
        ['General Motors Corporation',          30.27, 'admin'],
        ['Hewlett-Packard Co.',                 36.53, 'admin'],
        ['Honeywell Intl Inc',                  38.77, 'admin'],
        ['Intel Corporation',                   19.88, 'admin'],
        ['International Business Machines',     81.41, 'admin'],
        ['Johnson & Johnson',                   64.72, 'admin'],
        ['JP Morgan & Chase & Co',              45.73, 'admin'],
        ['McDonald\'s Corporation',             36.76, 'admin'],
        ['Merck & Co., Inc.',                   40.96, 'admin'],
        ['Microsoft Corporation',               25.84, 'admin'],
        ['Pfizer Inc',                          27.96, 'admin'],
        ['The Coca-Cola Company',               45.07, 'admin'],
        ['The Home Depot, Inc.',                34.64, 'admin'],
        ['The Procter & Gamble Company',        61.91, 'admin'],
        ['United Technologies Corporation',     63.26, 'admin'],
        ['Verizon Communications',              35.57, 'admin'],
        ['Wal-Mart Stores, Inc.',               45.45, 'admin']
    ];

    var store = Ext.create('Ext.data.ArrayStore', {
        fields : ['company', { name: 'price', type: 'float' }, 'permission'],
        data   : sampleData
    });

    var grid = Ext.create('Ext.grid.Panel', {
        height     : 500,
        width      : 400,
        renderTo   : document.body,
        title      : 'Grid',
        selModel   : { mode : 'MULTI' },
        viewConfig : { stripeRows : true },
        store      : store,
        columns    : [{
            text         : 'Company',
            flex         : 1,
            sortable     : false,
            resizable    : false,
            draggable    : false,
            menuDisabled : true,
            dataIndex    : 'company'
        }, {
            text         : 'Price',
            width        : 75,
            sortable     : false,
            resizable    : false,
            draggable    : false,
            menuDisabled : true,
            renderer     : 'usMoney',
            dataIndex    : 'price'
        }]
    });

    // actions
    var removeAll  = function() { store.removeAll(); };
    var addBack    = function() { store.add(sampleData); };
    var change3mCo = function() { store.getAt(0).set('price', 20.01); };
    var info       = function() {
        var record = grid.getSelectionModel().getSelection()[0];
        Ext.Msg.alert('Info', 'Price: ' + record.get('price'));
    };

    // buttons
    var mkButton = function(text, disabled) {
        return Ext.create('Ext.Button', {
            text     : text,
            disabled : disabled,
            renderTo : document.body
        });
    };
    var removeAllButton  = mkButton('Remove all', false);
    var addBackButton    = mkButton('Add back', true);
    var change3mCoButton = mkButton('Change 3m Co', false);
    var infoButton       = mkButton('Info', true);

    // fn utils
    var constant   = function(val) { return function() { return val; }; };
    var not        = function(fn) { return function() { return !fn.apply(null, arguments); }; };
    var secondArg  = function(fst, snd) { return snd; };
    var arrayFirst = function(array) { return array[0]; };
    var truthy     = function(val) { return !!val; };
    var ifEqualRet = function(a, b) { return a === b ? a : false; };
    var isArrayOfOneElement = function(array)  { return array.length === 1; };
    var isPriceGt40         = function(record) { return record.get('price') > 40; };
    var getPermission       = function(record) { return record.get('permission'); };
    var permissionsMatch    = function(user, row) { return user === row; };
    var storeHasRecords     = function(store) { return store.getCount() !== 0; };
    var isKey = function(matchKey) {
        return function(event) {
            return event.getKey() === matchKey;
        };
    };
    var enableDisableButton = function(button) {
        return function(enable) {
            button.setDisabled(!enable);
        };
    };
    var focusGrid = function() { grid.focus(); };

    // stream construction
    var streamFromClick    = function(button) { return Bacon.fromEvent(button, 'click'); };
    var permissionUserHas  = 'user';
    var updateStream       = Bacon.fromEvent(store, 'update', secondArg);
    var multiSelectStream  = Bacon.fromEvent(grid.getSelectionModel(), 'selectionchange', secondArg);
    var permissionProperty = Bacon.constant(permissionUserHas);
    var dataChangedStream  = Bacon.fromEvent(store, 'datachanged', constant(store));
    var keyPressStream     = Bacon.fromEvent(grid.getEl(), 'keypress');
    var removeAllClicks    = streamFromClick(removeAllButton);
    var addBackClicks      = streamFromClick(addBackButton);
    var change3mCoClicks   = streamFromClick(change3mCoButton);
    var infoClicks         = streamFromClick(infoButton);

    // stream usage
    var oneRowSelected       = multiSelectStream.map(isArrayOfOneElement);
    var singleSelection      = multiSelectStream.filter(isArrayOfOneElement).map(arrayFirst);
    var updatedSelected      = updateStream.combine(singleSelection, ifEqualRet).filter(truthy);
    var gt40                 = singleSelection.merge(updatedSelected).map(isPriceGt40);
    var permissionOfSelected = singleSelection.map(getPermission);
    var hasValidPermission   = permissionProperty.sampledBy(permissionOfSelected, permissionsMatch);
    var storeHasSomething    = dataChangedStream.map(storeHasRecords);
    var deleteKey            = keyPressStream.filter(isKey(Ext.EventObject.DELETE));
    var enterKey             = keyPressStream.filter(isKey(Ext.EventObject.ENTER));
    var removeAllEvents      = deleteKey.merge(removeAllClicks).map(store).filter(storeHasRecords);
    var addBackEvents        = enterKey.merge(addBackClicks).map(store).filter(not(storeHasRecords));

    var enableDisableStream  = Bacon.combineWith(function(somethingSelected, isValidPermission, isGt40) {
        return somethingSelected && isValidPermission && isGt40;
    }, oneRowSelected, hasValidPermission, gt40);

    enableDisableStream.onValue(enableDisableButton(infoButton));
    storeHasSomething.onValue(enableDisableButton(change3mCoButton));
    storeHasSomething.onValue(enableDisableButton(removeAllButton));
    storeHasSomething.not().onValue(enableDisableButton(addBackButton));
    removeAllEvents.onValue(removeAll);
    addBackEvents.onValue(addBack);
    change3mCoClicks.onValue(change3mCo);
    infoClicks.onValue(info);
    [removeAllEvents, addBackEvents, change3mCoClicks, infoClicks].forEach(function(stream) {
        stream.onValue(focusGrid);
    });
});
