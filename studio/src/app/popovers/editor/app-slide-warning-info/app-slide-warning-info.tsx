import {Component, Element, Fragment, h, Prop} from '@stencil/core';

@Component({
  tag: 'app-slide-warning-info',
})
export class AppSlideWarningInfo {
  @Element() el: HTMLElement;

  @Prop()
  lowContrast: boolean;

  @Prop()
  overflow: boolean;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        {this.renderLowContrast()}

        {this.renderOverflow()}

        <div class="ion-text-center">
          <ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>
            Got it
          </ion-button>
        </div>
      </div>
    );
  }

  private renderOverflow() {
    if (!this.overflow) {
      return undefined;
    }

    return (
      <Fragment>
        <h2>Overflow</h2>
        <p>Part of your content may be placed outside the slide's safe area (16/9).</p>
        <p>It may not be visible on all display sizes.</p>
      </Fragment>
    );
  }

  private renderLowContrast() {
    if (!this.lowContrast) {
      return undefined;
    }

    return (
      <Fragment>
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
      </Fragment>
    );
  }
}
