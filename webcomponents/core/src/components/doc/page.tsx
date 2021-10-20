import {Component, ComponentInterface, Host, h, Prop} from '@stencil/core';

@Component({
  tag: 'deckgo-page',
  styleUrl: 'page.scss',
  shadow: true
})
export class DeckGoPage implements ComponentInterface {
  @Prop({reflect: true})
  size: 'A4' | 'A3' | 'A5' = 'A4';

  @Prop({reflect: true})
  orientation: 'portrait' | 'landscape' = 'portrait';

  render() {
    return (
      <Host>
        <slot></slot>
      </Host>
    );
  }
}
