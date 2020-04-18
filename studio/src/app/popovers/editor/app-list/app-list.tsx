import {Component, Element, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../utils/editor/slot-type';
import {ListUtils} from '../../../utils/editor/list.utils';

@Component({
  tag: 'app-list',
  styleUrl: 'app-list.scss',
})
export class AppList {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private currentList: SlotType.OL | SlotType.UL | undefined;

  async componentWillLoad() {
    this.currentList = await ListUtils.isElementList(this.selectedElement);
  }

  private async closePopover(list: SlotType.OL | SlotType.UL) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      list: list,
    });
  }

  private async selectList(list: SlotType.OL | SlotType.UL) {
    await this.closePopover(list);
  }

  render() {
    return (
      <ion-list>
        <ion-item onClick={() => this.selectList(SlotType.UL)} class={this.currentList == SlotType.UL ? 'active' : undefined}>
          <ion-icon slot="start" src="/assets/icons/ionicons/list.svg"></ion-icon>
          <ion-label>Unordered list</ion-label>
        </ion-item>

        <ion-item onClick={() => this.selectList(SlotType.OL)} class={this.currentList == SlotType.OL ? 'active' : undefined}>
          <ion-icon slot="start" src="/assets/icons/list-ol.svg"></ion-icon>
          <ion-label>Ordered list</ion-label>
        </ion-item>
      </ion-list>
    );
  }
}
