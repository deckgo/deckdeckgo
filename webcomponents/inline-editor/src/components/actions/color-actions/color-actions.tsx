import {Component, Event, EventEmitter, h, Prop, Host} from '@stencil/core';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

@Component({
  tag: 'deckgo-ie-color-actions',
  styleUrl: 'color-actions.scss',
  shadow: true,
})
export class ColorActions {
  @Prop()
  selection: Selection;

  @Prop()
  action: 'foreColor' | 'backColor';

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

    document.execCommand(this.action, false, $event.detail.hex);

    await this.colorModified.emit();
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
