import {Component, Element, Prop, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {ImageAction} from '../../../types/editor/image-action';

@Component({
  tag: 'app-image-element',
  styleUrl: 'app-image-element.scss'
})
export class AppImageElement {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

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
        <h2>{this.slide ? i18n.state.editor.slide_background : i18n.state.editor.image}</h2>
        <app-close-menu slot="end" onClose={() => this.closePopoverWithoutResults()}></app-close-menu>
      </ion-toolbar>,
      <app-image-choice
        selectedTarget={this.selectedTarget}
        slide={this.slide}
        onAction={($event: CustomEvent<ImageAction>) => this.onAction($event)}
      ></app-image-choice>
    ];
  }
}
