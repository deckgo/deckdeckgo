import {Component, Event, EventEmitter, h, Prop, Host} from '@stencil/core';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

@Component({
  tag: 'deckgo-ie-color-actions',
  styleUrl: 'color-actions.scss',
  shadow: true
})
export class ColorActions {
  @Prop()
  selection: Selection;

  @Prop()
  color: string;

  @Prop()
  palette: DeckdeckgoPalette[];

  @Prop()
  mobile: boolean;

  @Event()
  colorModified: EventEmitter<void>;

  private async selectColor($event: CustomEvent) {
    if (!this.selection || !$event || !$event.detail) {
      return;
    }

    this.color = $event.detail.hex;

    if (!this.selection || this.selection.rangeCount <= 0 || !document) {
      return;
    }

    const text: string = this.selection.toString();

    if (!text || text.length <= 0) {
      return;
    }

    document.execCommand('foreColor', false, this.color);

    await this.colorModified.emit();
  }

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <deckgo-color onColorChange={($event: CustomEvent) => this.selectColor($event)} more={false} palette={this.palette}>
          <div slot="more"></div>
        </deckgo-color>
      </Host>
    );
  }
}
