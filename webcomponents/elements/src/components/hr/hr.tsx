import {Component, h, ComponentInterface} from '@stencil/core';

@Component({
  tag: 'deckgo-hr',
  styleUrl: 'hr.scss',
  shadow: true
})
export class HrComponent implements ComponentInterface {
  render() {
    return <hr />;
  }
}
