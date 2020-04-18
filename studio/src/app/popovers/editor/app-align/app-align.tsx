import {Component, Element, h, Prop, State} from '@stencil/core';

import {AlignUtils, TextAlign} from '../../../utils/editor/align.utils';

@Component({
  tag: 'app-align',
  styleUrl: 'app-align.scss',
})
export class AppAlign {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private currentAlign: TextAlign | undefined;

  async componentWillLoad() {
    this.currentAlign = await AlignUtils.getAlignment(this.selectedElement);
  }

  private async closePopover(align: TextAlign) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      align: align,
    });
  }

  private async align(align: TextAlign) {
    await this.closePopover(align);
  }

  render() {
    return (
      <ion-list>
        {this.renderAlign(TextAlign.LEFT, 'Align left')}
        {this.renderAlign(TextAlign.CENTER, 'Align center')}
        {this.renderAlign(TextAlign.RIGHT, 'Align right')}
      </ion-list>
    );
  }

  private renderAlign(align: TextAlign, text: string) {
    return (
      <ion-item onClick={() => this.align(align)} class={this.currentAlign == align ? 'active' : undefined}>
        <ion-icon slot="start" src={`/assets/icons/align-${align}.svg`} role="presentation"></ion-icon>
        <ion-label>{text}</ion-label>
      </ion-item>
    );
  }
}
