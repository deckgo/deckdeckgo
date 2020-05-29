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

  private async selectList($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.list = $event.detail.value;
    this.toggleList.emit($event.detail.value);
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">List</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>List</ion-label>

            <ion-select
              value={this.list}
              placeholder="Select a type of list"
              onIonChange={($event: CustomEvent) => this.selectList($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={SlotType.UL}>Unordered</ion-select-option>
              <ion-select-option value={SlotType.OL}>Ordered</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
