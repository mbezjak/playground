var React = require('react');
var DataGrid = require('react-datagrid');

var data = [
    { id: '1', firstName: 'John', lastName: 'Doe' },
    { id: '2', firstName: 'Jane', lastName: 'Doe' }
];
var columns = [
    { name: 'firstName' },
    { name: 'lastName' }
];

React.render(
    <DataGrid idProperty="id" dataSource={data} columns={columns} />,
    document.getElementById('example')
);
