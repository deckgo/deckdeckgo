import {Component, Element, h, Prop} from '@stencil/core';

import {SlotType} from '../../../types/editor/slot-type';

@Component({
  tag: 'app-transform',
  styleUrl: 'app-transform.scss',
})
export class AppTransform {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  private async closePopover(type?: SlotType) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      type: type,
    });
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Transform element</h2>
        <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
      </ion-toolbar>,

      <app-slot-type selectedElement={this.selectedElement} onSelectType={($event: CustomEvent<SlotType>) => this.closePopover($event.detail)}></app-slot-type>,
    ];
  }
}
