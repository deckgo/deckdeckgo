import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../../../utils/editor/slot-type';
import {ListUtils} from '../../../../../utils/editor/list.utils';

import {OrderedStyle, UnorderedStyle} from '../../../../../utils/editor/list-style-type';

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
  private selectedStyle: OrderedStyle | UnorderedStyle | undefined;

  @Event() toggleList: EventEmitter<SlotType.OL | SlotType.UL>;

  @Event() listStyleChanged: EventEmitter<OrderedStyle | UnorderedStyle>;

  async componentWillLoad() {
    this.listType = await ListUtils.isElementList(this.selectedElement);
    this.selectedStyle = await ListUtils.getListElementType(this.selectedElement);
  }

  private listStyleToSlotType(listStyle: OrderedStyle | UnorderedStyle) {
    switch (listStyle) {
      case OrderedStyle.DECIMAL:
      case OrderedStyle.DECIMAL_LEADING:
      case OrderedStyle.LATIN_LOWER:
      case OrderedStyle.LATIN_UPPER:
      case OrderedStyle.ROMAN_LOWER:
      case OrderedStyle.ROMAN_UPPER:
        return SlotType.OL;
      case UnorderedStyle.BULLET:
      case UnorderedStyle.CIRCLE:
      case UnorderedStyle.SQUARE:
        return SlotType.UL;
    }
  }

  private async emitSelection() {
    this.listStyleChanged.emit(this.selectedStyle);
    this.toggleList.emit(this.listType);
  }

  private async setListType($event: CustomEvent) {
    this.listType = $event.detail.value;
  }

  private async setListStyle($event: CustomEvent) {
    this.listType = this.listStyleToSlotType($event.detail.value);
    this.selectedStyle = $event.detail.value;
    this.emitSelection();
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">List</ion-label>

        <ion-segment onIonChange={($event: CustomEvent) => this.setListType($event)} value={this.listType}>
          <ion-segment-button value={SlotType.OL}>
            <ion-label>Ordered</ion-label>
          </ion-segment-button>
          <ion-segment-button value={SlotType.UL}>
            <ion-label>Unordered</ion-label>
          </ion-segment-button>
        </ion-segment>
        <br></br>
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
      <ion-select-option value={OrderedStyle.DECIMAL}>Decimal</ion-select-option>,
      <ion-select-option value={OrderedStyle.DECIMAL_LEADING}>Decimal with Zero</ion-select-option>,
      <ion-select-option value={OrderedStyle.LATIN_LOWER}>Latin Lowercase</ion-select-option>,
      <ion-select-option value={OrderedStyle.LATIN_UPPER}>Latin Uppercase</ion-select-option>,
      <ion-select-option value={OrderedStyle.ROMAN_LOWER}>Roman Lowercase</ion-select-option>,
      <ion-select-option value={OrderedStyle.ROMAN_UPPER}>Roman Uppercase</ion-select-option>,
    ];
  }

  private renderUnorderedStyles() {
    return [
      <ion-select-option value={UnorderedStyle.BULLET}>Bullet</ion-select-option>,
      <ion-select-option value={UnorderedStyle.CIRCLE}>Circle</ion-select-option>,
      <ion-select-option value={UnorderedStyle.SQUARE}>Square</ion-select-option>,
    ];
  }
}
