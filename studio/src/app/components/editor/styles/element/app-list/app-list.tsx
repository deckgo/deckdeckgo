import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../../../utils/editor/slot-type';
import {ListUtils} from '../../../../../utils/editor/list.utils';
import {ListStyle} from '../../../../../utils/editor/list-style-type';
import {SlotUtils} from '../../../../../utils/editor/slot.utils';

@Component({
  tag: 'app-list',
  styleUrl: 'app-list.scss',
})
export class AppList {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private listType: SlotType.OL | SlotType.UL | undefined;

  @State()
  private selectedStyle: ListStyle | undefined;

  @Event() toggleList: EventEmitter<SlotType.OL | SlotType.UL>;

  @Event() listStyleChanged: EventEmitter<ListStyle>;

  async componentWillLoad() {
    this.listType = await ListUtils.isElementList(this.selectedElement);
    this.selectedStyle = await ListUtils.getListElementType(this.selectedElement);
  }

  private async setListType($event: CustomEvent) {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    this.listType = $event.detail.value;

    await this.removeStyle();

    this.toggleList.emit(this.listType);
  }

  private async setListStyle($event: CustomEvent) {
    if (!this.selectedElement || !$event || !$event.detail) {
      return;
    }

    await this.updateStyle($event.detail.value);
  }

  private async updateStyle(style: ListStyle) {
    this.selectedStyle = style;

    if (SlotUtils.isNodeRevealList(this.selectedElement)) {
      this.selectedElement.style['--reveal-list-style'] = this.selectedStyle;
    } else {
      this.selectedElement.style.listStyleType = this.selectedStyle;
    }

    this.listStyleChanged.emit();
  }

  private async removeStyle() {
    if (SlotUtils.isNodeRevealList(this.selectedElement)) {
      this.selectedElement.style['--reveal-list-style'] = '';
    } else {
      this.selectedElement.style.listStyleType = '';
    }
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">List</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>List</ion-label>

            <ion-select
              value={this.listType}
              placeholder="Select a type of list"
              onIonChange={($event: CustomEvent) => this.setListType($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={SlotType.OL}>Ordered</ion-select-option>
              <ion-select-option value={SlotType.UL}>Unordered</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>

        <ion-list>
          <ion-item class="select">
            <ion-label>List Style</ion-label>
            <ion-select
              value={this.selectedStyle}
              placeholder="Select style"
              onIonChange={($event: CustomEvent) => this.setListStyle($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              {this.listType === SlotType.OL ? this.renderOrderedStyles() : this.renderUnorderedStyles()}
            </ion-select>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderOrderedStyles() {
    return [
      <ion-select-option value={ListStyle.DECIMAL}>Decimal</ion-select-option>,
      <ion-select-option value={ListStyle.DECIMAL_LEADING}>Decimal with Zero</ion-select-option>,
      <ion-select-option value={ListStyle.LATIN_LOWER}>Latin Lowercase</ion-select-option>,
      <ion-select-option value={ListStyle.LATIN_UPPER}>Latin Uppercase</ion-select-option>,
      <ion-select-option value={ListStyle.ROMAN_LOWER}>Roman Lowercase</ion-select-option>,
      <ion-select-option value={ListStyle.ROMAN_UPPER}>Roman Uppercase</ion-select-option>,
    ];
  }

  private renderUnorderedStyles() {
    return [
      <ion-select-option value={ListStyle.BULLET}>Bullet</ion-select-option>,
      <ion-select-option value={ListStyle.CIRCLE}>Circle</ion-select-option>,
      <ion-select-option value={ListStyle.SQUARE}>Square</ion-select-option>,
    ];
  }
}
