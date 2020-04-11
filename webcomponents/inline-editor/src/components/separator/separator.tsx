import {Component, h} from '@stencil/core';

@Component({
  tag: 'deckgo-ie-separator',
  styleUrl: 'separator.scss',
  shadow: true
})
export class Separator {
  render() {
    return <div class="separator"></div>;
  }
}
