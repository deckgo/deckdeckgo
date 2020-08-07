import {Component, Element, h, Prop} from '@stencil/core';

import {SlotType} from '../../../utils/editor/slot-type';

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
        <button slot="end" class="close-options" onClick={() => this.closePopover()} tabindex={0}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </button>
      </ion-toolbar>,

      <app-slot-type selectedElement={this.selectedElement} onSelectType={($event: CustomEvent<SlotType>) => this.closePopover($event.detail)}></app-slot-type>,
    ];
  }
}
