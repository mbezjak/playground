const {h, render, Component} = require('ink');
const SelectInput = require('ink-select-input');
const process = require('process');

const Demo = () => {
      const handleSelect = item => {
            console.log('selected: ', item);
            unmount();
      };
      const items = [{
            label: 'First',
            value: 'first'
      }, {
            label: 'Second',
            value: 'second'
      }, {
            label: 'Third',
            value: 'third'
      }];

      return <SelectInput items={items} onSelect={handleSelect}/>
};

var unmount = render(<Demo/>);
