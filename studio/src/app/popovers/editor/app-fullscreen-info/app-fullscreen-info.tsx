import {Component, Element, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-fullscreen-info'
})
export class AppFullscreenInfo {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <h2>{i18n.state.editor.fullscreen_tips}</h2>
        <p>{i18n.state.editor.fullscreen_edit}</p>
        <p>{i18n.state.editor.fullscreen_typo}</p>
        <p>{i18n.state.editor.fullscreen_swipe}</p>
        <div class="ion-text-center ion-padding-top">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            {i18n.state.core.got_it}
          </ion-button>
        </div>
      </div>
    );
  }
}
