import {Component, ComponentInterface, h, Host} from '@stencil/core';
import busyStore from '../../stores/busy.store';
import i18n from '../../stores/i18n.store';

@Component({
  tag: 'deckgo-doc-indicator',
  styleUrl: 'indicator.scss',
  shadow: false
})
export class Indicator implements ComponentInterface {
  render() {
    return (
      <Host>
        {busyStore.state.busy ? i18n.state.indicator.saving : ''}
        {!busyStore.state.docReady && !busyStore.state.busy ? i18n.state.indicator.loading : ''}
      </Host>
    );
  }
}
