import {Component, ComponentInterface, Host, h} from '@stencil/core';

import busyStore from '../../../../stores/busy.store';
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
        {busyStore.state.busy ? i18n.state.editor.saving : ''}
        {!busyStore.state.docReady && !busyStore.state.busy ? i18n.state.core.loading : ''}
      </Host>
    );
  }
}
