import {Component, Element, h} from '@stencil/core';

@Component({
  tag: 'app-fullscreen-info',
})
export class AppFullscreenInfo {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <h2>Tips about fullscreen 👋</h2>
        <p>You can edit your presentations in full screen mode too!</p>
        <p>Correct typo, add slides, change colors etc. all features are available.</p>
        <p>Swipe up the bottom sheet to enter the edit mode 🚀.</p>
        <div class="ion-text-center ion-padding-top">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            Got it
          </ion-button>
        </div>
      </div>
    );
  }
}
