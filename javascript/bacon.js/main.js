Ext.onReady(function() {
    Ext.QuickTips.init();
    Ext.state.Manager.setProvider(Ext.create('Ext.state.Provider'));

    var sampleData = [
        ['3m Co',                               71.72,  0.02, 'user'],
        ['Alcoa Inc',                           29.01,  0.42, 'user'],
        ['Altria Group Inc',                    83.81,  0.28, 'user'],
        ['American Express Company',            52.55,  0.01, 'user'],
        ['American International Group, Inc.',  64.13,  0.31, 'user'],
        ['AT&T Inc.',                           31.61, -0.48, 'user'],
        ['Boeing Co.',                          75.43,  0.53, 'user'],
        ['Caterpillar Inc.',                    67.27,  0.92, 'admin'],
        ['Citigroup, Inc.',                     49.37,  0.02, 'admin'],
        ['E.I. du Pont de Nemours and Company', 40.48,  0.51, 'admin'],
        ['Exxon Mobil Corp',                    68.10, -0.43, 'admin'],
        ['General Electric Company',            34.14, -0.08, 'admin'],
        ['General Motors Corporation',          30.27,  1.09, 'admin'],
        ['Hewlett-Packard Co.',                 36.53, -0.03, 'admin'],
        ['Honeywell Intl Inc',                  38.77,  0.05, 'admin'],
        ['Intel Corporation',                   19.88,  0.31, 'admin'],
        ['International Business Machines',     81.41,  0.44, 'admin'],
        ['Johnson & Johnson',                   64.72,  0.06, 'admin'],
        ['JP Morgan & Chase & Co',              45.73,  0.07, 'admin'],
        ['McDonald\'s Corporation',             36.76,  0.86, 'admin'],
        ['Merck & Co., Inc.',                   40.96,  0.41, 'admin'],
        ['Microsoft Corporation',               25.84,  0.14, 'admin'],
        ['Pfizer Inc',                          27.96,  0.40, 'admin'],
        ['The Coca-Cola Company',               45.07,  0.26, 'admin'],
        ['The Home Depot, Inc.',                34.64,  0.35, 'admin'],
        ['The Procter & Gamble Company',        61.91,  0.01, 'admin'],
        ['United Technologies Corporation',     63.26,  0.55, 'admin'],
        ['Verizon Communications',              35.57,  0.39, 'admin'],
        ['Wal-Mart Stores, Inc.',               45.45,  0.73, 'admin']
    ];

    function change(val) {
        if (val > 0) {
            return '<span style="color:green;">' + val + '</span>';
        } else if (val < 0) {
            return '<span style="color:red;">' + val + '</span>';
        }
        return val;
    }

    var store = Ext.create('Ext.data.ArrayStore', {
        fields: [
            {name: 'company'},
            {name: 'price',     type: 'float'},
            {name: 'change',    type: 'float'},
            {name: 'permission'}
        ],
        data: sampleData
    });

    var grid = Ext.create('Ext.grid.Panel', {
        store: store,
        stateful: true,
        stateId: 'stateGrid',
        columns: [
            {
                text     : 'Company',
                flex     : 1,
                sortable : false,
                dataIndex: 'company'
            },
            {
                text     : 'Price',
                width    : 75,
                sortable : true,
                renderer : 'usMoney',
                dataIndex: 'price'
            },
            {
                text     : 'Change',
                width    : 75,
                sortable : true,
                renderer : change,
                dataIndex: 'change'
            }
        ],
        height: 500,
        width: 500,
        title: 'Grid',
        renderTo: document.body,
        selModel: {
            mode: 'MULTI'
        },
        viewConfig: {
            stripeRows: true
        }
    });

    var removeAllButton = Ext.create('Ext.Button', {
        text: "Remove all",
        renderTo: document.body,
        handler: function() {
            store.removeAll();
        }
    });
    var addBackButton = Ext.create('Ext.Button', {
        text: 'Add back',
        disabled: true,
        renderTo: document.body,
        handler: function() {
            store.add(sampleData);
        }
    });

    var change3mCoButton = Ext.create('Ext.Button', {
        text: 'Change 3m Co',
        renderTo: document.body,
        handler: function() {
            store.getAt(0).set('price', 20.01);
        }
    });

    var infoButton = Ext.create('Ext.Button', {
        text: 'Info',
        disabled: true,
        renderTo: document.body,
        handler: function() {
            var record = grid.getSelectionModel().getSelection()[0];
            Ext.Msg.alert('Info', 'Price: ' + record.get('price'));
        }
    });

    // fn utils
    var constant   = function(val) { return function() { return val; }; };
    var secondArg  = function(fst, snd) { return snd; };
    var arrayFirst = function(array) { return array[0]; };
    var truthy     = function(val) { return !!val; };
    var ifEqualRet = function(a, b) { return a === b ? a : false; };
    var isArrayOfOneElement = function(array)  { return array.length === 1; };
    var isPriceGt40         = function(record) { return record.get('price') > 40; };
    var getPermission       = function(record) { return record.get('permission'); };
    var permissionsMatch    = function(user, row) { return user === row; };
    var storeHasRecords     = function(store) { return store.getCount() !== 0; };
    var enableDisableButton = function(button) {
        return function(enable) {
            button.setDisabled(!enable);
        };
    };

    // stream construction
    var permissionUserHas  = 'user';
    var updateStream       = Bacon.fromEvent(store, 'update', secondArg);
    var multiSelectStream  = Bacon.fromEvent(grid.getSelectionModel(), 'selectionchange', secondArg);
    var permissionProperty = Bacon.constant(permissionUserHas);
    var dataChangedStream  = Bacon.fromEvent(store, 'datachanged', constant(store));

    // stream usage
    var oneRowSelected       = multiSelectStream.map(isArrayOfOneElement);
    var singleSelection      = multiSelectStream.filter(isArrayOfOneElement).map(arrayFirst);
    var updatedSelected      = updateStream.combine(singleSelection, ifEqualRet).filter(truthy);
    var gt40                 = singleSelection.merge(updatedSelected).map(isPriceGt40);
    var permissionOfSelected = singleSelection.map(getPermission);
    var hasValidPermission   = permissionProperty.sampledBy(permissionOfSelected, permissionsMatch);
    var storeHasSomething    = dataChangedStream.map(storeHasRecords);

    var enableDisableStream  = Bacon.combineWith(function(somethingSelected, isValidPermission, isGt40) {
        return somethingSelected && isValidPermission && isGt40;
    }, oneRowSelected, hasValidPermission, gt40);

    enableDisableStream.onValue(enableDisableButton(infoButton));
    storeHasSomething.onValue(enableDisableButton(change3mCoButton));
    storeHasSomething.onValue(enableDisableButton(removeAllButton));
    storeHasSomething.not().onValue(enableDisableButton(addBackButton));
});
