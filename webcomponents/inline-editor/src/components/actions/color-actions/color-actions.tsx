import {Component, Event, EventEmitter, h, Prop, Host, State} from '@stencil/core';

import {DeckdeckgoPalette} from '@deckdeckgo/color';

import {hexToRgb} from '@deckdeckgo/utils';

import {getSelection} from '../../../utils/selection.utils';
import {findStyleNode, getAnchorNode} from '../../../utils/node.utils';

import {ExecCommandAction} from '../../../interfaces/interfaces';

@Component({
  tag: 'deckgo-ie-color-actions',
  styleUrl: 'color-actions.scss',
  shadow: true
})
export class ColorActions {
  @Prop()
  action: 'color' | 'background-color';

  @Prop()
  palette: DeckdeckgoPalette[];

  @Prop()
  mobile: boolean;

  @Prop()
  containers: string;

  @State()
  private colorRgb: string | undefined;

  @Event()
  private execCommand: EventEmitter<ExecCommandAction>;

  private range: Range | undefined;

  async componentWillLoad() {
    await this.initColor();
  }

  private async initColor() {
    const selection: Selection | undefined = await getSelection();

    this.range = selection?.getRangeAt(0);

    const container: HTMLElement | undefined = getAnchorNode(selection);

    if (!container) {
      return;
    }

    const style: Node | null = await findStyleNode(container, this.action === 'color' ? 'color' : 'background-color', this.containers);

    if (!style) {
      return;
    }

    const css: CSSStyleDeclaration = window?.getComputedStyle(style as HTMLElement);

    this.colorRgb = (this.action === 'color' ? css.color : css.backgroundColor).replace('rgb(', '').replace(')', '');
  }

  private async selectColor($event: CustomEvent) {
    const selection: Selection | undefined = await getSelection();

    if (!selection || !$event || !$event.detail) {
      return;
    }

    if (!this.action) {
      return;
    }

    selection?.removeAllRanges();
    selection?.addRange(this.range);

    const observer: MutationObserver = new MutationObserver((_mutations: MutationRecord[]) => {
      observer.disconnect();

      // No node were added so the style was modified
      this.range = selection?.getRangeAt(0);
    });

    const anchorNode: HTMLElement | undefined = getAnchorNode(selection);

    observer.observe(anchorNode, {childList: true});

    this.execCommand.emit({
      cmd: 'style',
      detail: {
        style: this.action,
        value: $event.detail.hex,
        initial: (element: HTMLElement | null) => {
          return new Promise<boolean>(async (resolve) => {
            const rgb: string = await hexToRgb($event.detail.hex);
            resolve(element && (element.style[this.action] === $event.detail.hex || element.style[this.action] === `rgb(${rgb})`));
          });
        }
      }
    });
  }

  render() {
    const cssClass = this.mobile ? 'deckgo-tools-mobile' : undefined;

    return (
      <Host class={cssClass}>
        <deckgo-color
          color-rgb={this.colorRgb}
          onColorChange={($event: CustomEvent) => this.selectColor($event)}
          palette={this.palette}></deckgo-color>
      </Host>
    );
  }
}
