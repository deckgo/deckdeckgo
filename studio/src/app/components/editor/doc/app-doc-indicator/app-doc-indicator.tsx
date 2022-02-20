import {busyStore} from '@deckdeckgo/studio';
import {Component, ComponentInterface, h, Host} from '@stencil/core';
import i18n from '../../../../stores/i18n.store';
import {busyBeforeUnload} from '../../../../utils/core/before-unload.utils';

@Component({
  tag: 'app-doc-indicator',
  styleUrl: 'app-doc-indicator.scss',
  shadow: false
})
export class AppDocIndicator implements ComponentInterface {
  private destroyListener;

  componentWillLoad() {
    this.destroyListener = busyStore.default.onChange('busy', (busy: boolean) => {
      busyBeforeUnload(busy);
    });

    busyBeforeUnload(busyStore.default.state.busy);
  }

  disconnectedCallback() {
    this.destroyListener?.();
  }

  render() {
    return (
      <Host>
        {busyStore.default.state.busy ? i18n.state.editor.saving : ''}
        {!busyStore.default.state.docReady && !busyStore.default.state.busy ? i18n.state.core.loading : ''}
      </Host>
    );
  }
}
