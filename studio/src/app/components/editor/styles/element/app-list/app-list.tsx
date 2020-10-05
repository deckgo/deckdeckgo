import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {SlotType} from '../../../../../utils/editor/slot-type';
import {ListUtils} from '../../../../../utils/editor/list.utils';

import {ListStyle} from '../../../../../utils/editor/list-style-type';

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

  private listStyleToSlotType(listStyle: ListStyle) {
    switch (listStyle) {
      case ListStyle.DECIMAL:
      case ListStyle.DECIMAL_LEADING:
      case ListStyle.LATIN_LOWER:
      case ListStyle.LATIN_UPPER:
      case ListStyle.ROMAN_LOWER:
      case ListStyle.ROMAN_UPPER:
        return SlotType.OL;
      case ListStyle.BULLET:
      case ListStyle.CIRCLE:
      case ListStyle.SQUARE:
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
    // If this condition evaluates to true, that simply means
    // the tabs for the list types (ordered/unordered) was switched.
    // No actual style change was made.
    if ($event.detail.value === '' || $event.detail.value === this.selectedStyle.toString()) {
      return;
    }

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
              value={this.listStyleToSlotType(this.selectedStyle) === this.listType ? this.selectedStyle : ''}
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
