Ext.onReady(function() {
    Ext.QuickTips.init();
    Ext.state.Manager.setProvider(Ext.create('Ext.state.LocalStorageProvider'));

    var sampleData = [
        ['3m Co',                               71.72, 0.02,  0.03,  '9/1 12:00am', 'user'],
        ['Alcoa Inc',                           29.01, 0.42,  1.47,  '9/1 12:00am', 'user'],
        ['Altria Group Inc',                    83.81, 0.28,  0.34,  '9/1 12:00am', 'user'],
        ['American Express Company',            52.55, 0.01,  0.02,  '9/1 12:00am', 'user'],
        ['American International Group, Inc.',  64.13, 0.31,  0.49,  '9/1 12:00am', 'user'],
        ['AT&T Inc.',                           31.61, -0.48, -1.54, '9/1 12:00am', 'user'],
        ['Boeing Co.',                          75.43, 0.53,  0.71,  '9/1 12:00am', 'user'],
        ['Caterpillar Inc.',                    67.27, 0.92,  1.39,  '9/1 12:00am', 'admin'],
        ['Citigroup, Inc.',                     49.37, 0.02,  0.04,  '9/1 12:00am', 'admin'],
        ['E.I. du Pont de Nemours and Company', 40.48, 0.51,  1.28,  '9/1 12:00am', 'admin'],
        ['Exxon Mobil Corp',                    68.1,  -0.43, -0.64, '9/1 12:00am', 'admin'],
        ['General Electric Company',            34.14, -0.08, -0.23, '9/1 12:00am', 'admin'],
        ['General Motors Corporation',          30.27, 1.09,  3.74,  '9/1 12:00am', 'admin'],
        ['Hewlett-Packard Co.',                 36.53, -0.03, -0.08, '9/1 12:00am', 'admin'],
        ['Honeywell Intl Inc',                  38.77, 0.05,  0.13,  '9/1 12:00am', 'admin'],
        ['Intel Corporation',                   19.88, 0.31,  1.58,  '9/1 12:00am', 'admin'],
        ['International Business Machines',     81.41, 0.44,  0.54,  '9/1 12:00am', 'admin'],
        ['Johnson & Johnson',                   64.72, 0.06,  0.09,  '9/1 12:00am', 'admin'],
        ['JP Morgan & Chase & Co',              45.73, 0.07,  0.15,  '9/1 12:00am', 'admin'],
        ['McDonald\'s Corporation',             36.76, 0.86,  2.40,  '9/1 12:00am', 'admin'],
        ['Merck & Co., Inc.',                   40.96, 0.41,  1.01,  '9/1 12:00am', 'admin'],
        ['Microsoft Corporation',               25.84, 0.14,  0.54,  '9/1 12:00am', 'admin'],
        ['Pfizer Inc',                          27.96, 0.4,   1.45,  '9/1 12:00am', 'admin'],
        ['The Coca-Cola Company',               45.07, 0.26,  0.58,  '9/1 12:00am', 'admin'],
        ['The Home Depot, Inc.',                34.64, 0.35,  1.02,  '9/1 12:00am', 'admin'],
        ['The Procter & Gamble Company',        61.91, 0.01,  0.02,  '9/1 12:00am', 'admin'],
        ['United Technologies Corporation',     63.26, 0.55,  0.88,  '9/1 12:00am', 'admin'],
        ['Verizon Communications',              35.57, 0.39,  1.11,  '9/1 12:00am', 'admin'],
        ['Wal-Mart Stores, Inc.',               45.45, 0.73,  1.63,  '9/1 12:00am', 'admin']
    ];

    /**
     * Custom function used for column renderer
     * @param {Object} val
     */
    function change(val) {
        if (val > 0) {
            return '<span style="color:green;">' + val + '</span>';
        } else if (val < 0) {
            return '<span style="color:red;">' + val + '</span>';
        }
        return val;
    }

    /**
     * Custom function used for column renderer
     * @param {Object} val
     */
    function pctChange(val) {
        if (val > 0) {
            return '<span style="color:green;">' + val + '%</span>';
        } else if (val < 0) {
            return '<span style="color:red;">' + val + '%</span>';
        }
        return val;
    }

    var store = Ext.create('Ext.data.ArrayStore', {
        fields: [
            {name: 'company'},
            {name: 'price',      type: 'float'},
            {name: 'change',     type: 'float'},
            {name: 'pctChange',  type: 'float'},
            {name: 'lastChange', type: 'date', dateFormat: 'n/j h:ia'},
            {name: 'permission'}
        ],
        data: sampleData
    });

    var grid = Ext.create('Ext.grid.Panel', {
        store: store,
        stateful: true,
        stateId: 'stateGrid',
        selModel : {
            mode : 'MULTI'
        },
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
            },
            {
                text     : '% Change',
                width    : 75,
                sortable : true,
                renderer : pctChange,
                dataIndex: 'pctChange'
            },
            {
                text     : 'Last Updated',
                width    : 85,
                sortable : true,
                renderer : Ext.util.Format.dateRenderer('m/d/Y'),
                dataIndex: 'lastChange'
            },
            {
                xtype: 'actioncolumn',
                width: 50,
                items: [{
                    icon   : 'icons/delete.gif',  // Use a URL in the icon config
                    tooltip: 'Sell stock',
                    handler: function(grid, rowIndex, colIndex) {
                        var rec = store.getAt(rowIndex);
                        alert("Sell " + rec.get('company'));
                    }
                }, {
                    getClass: function(v, meta, rec) {          // Or return a class from a function
                        if (rec.get('change') < 0) {
                            this.items[1].tooltip = 'Hold stock';
                            return 'alert-col';
                        } else {
                            this.items[1].tooltip = 'Buy stock';
                            return 'buy-col';
                        }
                    },
                    handler: function(grid, rowIndex, colIndex) {
                        var rec = store.getAt(rowIndex);
                        alert((rec.get('change') < 0 ? "Hold " : "Buy ") + rec.get('company'));
                    }
                }]
            }
        ],
        height: 350,
        width: 600,
        title: 'Array Grid',
        renderTo: document.body,
        viewConfig: {
            stripeRows: true
        }
    });

    var recs;
    var removeButton = Ext.create('Ext.Button', {
        text: "Remove all",
        renderTo: document.body,
        handler: function() {
            recs = store.getRange();
            store.removeAll();
            addButton.enable();
        }
    });
    var addButton = Ext.create('Ext.Button', {
        text: 'Add back',
        disabled: true,
        renderTo: document.body,
        handler: function() {
            store.add(recs);
            removeButton.enable();
        }
    });

    Ext.create('Ext.Button', {
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

    var enableDisableInfoButton = function(enable) {
        infoButton.setDisabled(!enable);
    };

    // fn utils
    var secondArg  = function(fst, snd) { return snd; };
    var arrayFirst = function(array) { return array[0]; };
    var truthy     = function(val) { return !!val; };
    var ifEqualRet = function(a, b) { return a === b ? a : false; };
    var isArrayOfOneElement = function(array)  { return array.length === 1; };
    var isPriceGt40         = function(record) { return record.get('price') > 40; };
    var getPermission       = function(record) { return record.get('permission'); };
    var permissionsMatch    = function(user, row) { return user === row; };

    // stream construction
    var permissionUserHas  = 'user';
    var updateStream       = Bacon.fromEvent(store, 'update', secondArg);
    var multiSelectStream  = Bacon.fromEvent(grid.getSelectionModel(), 'selectionchange', secondArg);
    var permissionProperty = Bacon.constant(permissionUserHas);

    // stream usage
    var oneRowSelected       = multiSelectStream.map(isArrayOfOneElement);
    var singleSelection      = multiSelectStream.filter(isArrayOfOneElement).map(arrayFirst);
    var updatedSelected      = updateStream.combine(singleSelection, ifEqualRet).filter(truthy);
    var gt40                 = singleSelection.merge(updatedSelected).map(isPriceGt40);
    var permissionOfSelected = singleSelection.map(getPermission);
    var hasValidPermission   = permissionProperty.sampledBy(permissionOfSelected, permissionsMatch);

    var enableDisableStream  = Bacon.combineWith(function(somethingSelected, isValidPermission, isGt40) {
        return somethingSelected && isValidPermission && isGt40;
    }, oneRowSelected, hasValidPermission, gt40);

    enableDisableStream.onValue(enableDisableInfoButton);
});
