import {Component, Element, h} from '@stencil/core';

@Component({
  tag: 'app-contrast-info',
})
export class AppContrastInfo {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <h2>Low contrast</h2>
        <p>We noticed that (a part of) the text color of this slide does not meet contrast ratio standards.</p>
        <p>
          Elements are compared according{' '}
          <a href="https://www.w3.org/TR/WCAG/#contrast-minimum" target="_blank" rel="noopener noreferrer">
            WCAG
          </a>{' '}
          Level AA.
        </p>

        <p>Note that if you are using semi-transparent background, the contrast ratio cannot be precise.</p>
        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            Got it
          </ion-button>
        </div>
      </div>
    );
  }
}
