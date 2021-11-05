import {Component, ComponentInterface, Host, h} from '@stencil/core';

import busyStore from '../../../../stores/busy.store';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-doc-indicator',
  styleUrl: 'app-doc-indicator.scss',
  shadow: false
})
export class AppDocIndicator implements ComponentInterface {
  render() {
    return (
      <Host>
        {busyStore.state.deckBusy ? i18n.state.editor.saving : ''}
        {!busyStore.state.docReady && !busyStore.state.deckBusy ? i18n.state.core.loading : ''}
      </Host>
    );
  }
}
