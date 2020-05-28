import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../../../utils/editor/slot-type';
import {ListUtils} from '../../../../../utils/editor/list.utils';

@Component({
  tag: 'app-list',
  styleUrl: 'app-list.scss',
})
export class AppList {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private list: SlotType.OL | SlotType.UL | undefined;

  @Event() toggleList: EventEmitter<SlotType.OL | SlotType.UL>;

  async componentWillLoad() {
    this.list = await ListUtils.isElementList(this.selectedElement);
  }

  private async selectList(list: SlotType.OL | SlotType.UL) {
    this.list = list;
    this.toggleList.emit(list);
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">List</ion-label>
        <ion-list>
          <ion-item onClick={() => this.selectList(SlotType.UL)} class={`list ${this.list == SlotType.UL ? 'active' : undefined}`}>
            <ion-icon slot="start" src="/assets/icons/ionicons/list.svg"></ion-icon>
            <ion-label>Unordered list</ion-label>
          </ion-item>

          <ion-item onClick={() => this.selectList(SlotType.OL)} class={`list ${this.list == SlotType.OL ? 'active' : undefined}`}>
            <ion-icon slot="start" src="/assets/icons/list-ol.svg"></ion-icon>
            <ion-label>Ordered list</ion-label>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
