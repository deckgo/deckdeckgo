import {Component, Element, Prop, h} from '@stencil/core';

import {ImageAction} from '../../../utils/editor/image-action';

@Component({
  tag: 'app-image-element',
  styleUrl: 'app-image-element.scss',
})
export class AppImageElement {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slide: boolean = false;

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async onAction($event: CustomEvent<ImageAction>) {
    if ($event && $event.detail) {
      await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss($event.detail);
    }
  }

  render() {
    console.log('here');

    return [
      <ion-toolbar>
        <h2>{this.slide ? 'Slide background' : 'Image'}</h2>
        <button slot="end" class="close-options" onClick={() => this.closePopoverWithoutResults()} tabindex={0}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
      </ion-toolbar>,
      <app-image
        expander={false}
        expanded="open"
        selectedElement={this.selectedElement}
        slide={this.slide}
        onAction={($event: CustomEvent<ImageAction>) => this.onAction($event)}></app-image>,
    ];
  }
}
