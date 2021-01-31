import {Component, Element, h} from '@stencil/core';

@Component({
  tag: 'app-copy-style',
})
export class AppCopyStyle {
  @Element() el: HTMLElement;

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  render() {
    return (
      <div class="ion-padding">
        <a onClick={() => this.closePopover()} aria-label="Copy style">
          <p>Copy style</p>
        </a>

        <a onClick={() => this.closePopover()} aria-label="Apply style">
          <p>Apply style</p>
        </a>
      </div>
    );
  }
}
