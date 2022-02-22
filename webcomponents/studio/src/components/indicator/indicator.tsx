import {Component, ComponentInterface, h, Host} from '@stencil/core';
import busyStore from '../../stores/busy.store';
import i18n from '../../stores/i18n.store';
import {busyBeforeUnload} from '../../utils/before-unload.utils';

@Component({
  tag: 'deckgo-indicator',
  styleUrl: 'indicator.scss',
  shadow: false
})
export class Indicator implements ComponentInterface {
  private destroyListener;

  componentWillLoad() {
    this.destroyListener = busyStore.onChange('busy', (busy: boolean) => {
      busyBeforeUnload(busy);
    });

    busyBeforeUnload(busyStore.state.busy);
  }

  disconnectedCallback() {
    this.destroyListener?.();
  }

  render() {
    return (
      <Host>
        {busyStore.state.busy ? i18n.state.indicator.saving : ''}
        {!busyStore.state.docReady && !busyStore.state.busy ? i18n.state.indicator.loading : ''}
      </Host>
    );
  }
}
