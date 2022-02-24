import {Component, ComponentInterface, h, Host, Prop} from '@stencil/core';
import i18n from '../../stores/i18n.store';
import readyStore from '../../stores/ready.store';

@Component({
  tag: 'deckgo-doc-indicator',
  styleUrl: 'indicator.scss',
  shadow: false
})
export class Indicator implements ComponentInterface {
  @Prop()
  busy: boolean = false;

  render() {
    return (
      <Host>
        {this.busy ? i18n.state.indicator.saving : ''}
        {!readyStore.state.docReady && !this.busy ? i18n.state.indicator.loading : ''}
      </Host>
    );
  }
}
