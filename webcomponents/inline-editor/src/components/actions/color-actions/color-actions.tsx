import {Component, Event, EventEmitter, h, Prop, Host} from '@stencil/core';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

import {ExecCommandAction} from '../../../interfaces/interfaces';

@Component({
  tag: 'deckgo-ie-color-actions',
  styleUrl: 'color-actions.scss',
  shadow: true,
})
export class ColorActions {
  @Prop()
  selection: Selection;

  @Prop()
  action: 'color' | 'background-color';

  @Prop()
  palette: DeckdeckgoPalette[];

  @Prop()
  mobile: boolean;

  @Event()
  execCommand: EventEmitter<ExecCommandAction>;

  private async selectColor($event: CustomEvent) {
    if (!this.selection || !$event || !$event.detail) {
      return;
    }

    if (!this.action) {
      return;
    }

    if (!this.selection || this.selection.rangeCount <= 0 || !document) {
      return;
    }

    const text: string = this.selection.toString();

    if (!text || text.length <= 0) {
      return;
    }

    this.execCommand.emit({
      style: this.action,
      value: $event.detail.hex,
    });
  }

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <deckgo-color label={false} onColorChange={($event: CustomEvent) => this.selectColor($event)} more={false} palette={this.palette}>
          <div slot="more"></div>
        </deckgo-color>
      </Host>
    );
  }
}
