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
    return [
      <ion-toolbar>
        <h2>{this.slide ? 'Slide background' : 'Image'}</h2>
        <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <app-image
        expander={false}
        selectedElement={this.selectedElement}
        slide={this.slide}
        onAction={($event: CustomEvent<ImageAction>) => this.onAction($event)}></app-image>,
    ];
  }
}
