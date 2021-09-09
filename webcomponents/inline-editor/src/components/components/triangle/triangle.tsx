import {Component, Prop, h} from '@stencil/core';

@Component({
  tag: 'deckgo-ie-triangle',
  styleUrl: 'triangle.scss',
  shadow: true
})
export class Separator {
  @Prop()
  mobile: boolean;

  render() {
    if (this.mobile) {
      return undefined;
    }

    return <div class="triangle"></div>;
  }
}
