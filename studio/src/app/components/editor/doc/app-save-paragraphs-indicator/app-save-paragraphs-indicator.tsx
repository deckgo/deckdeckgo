import {Component, ComponentInterface, Host, h} from '@stencil/core';

import busyStore from '../../../../stores/busy.store';
import i18n from '../../../../stores/i18n.store';

@Component({
  tag: 'app-save-paragraphs-indicator',
  styleUrl: 'app-save-paragraphs-indicator.scss',
  shadow: false
})
export class AppSaveParagraphsIndicator implements ComponentInterface {
  render() {
    return <Host>{busyStore.state.deckBusy ? i18n.state.editor.saving : ''}</Host>;
  }
}
