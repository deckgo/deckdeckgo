import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {AlignUtils, TextAlign} from '../../../utils/editor/align.utils';

@Component({
  tag: 'app-align',
  styleUrl: 'app-align.scss',
})
export class AppAlign {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private align: TextAlign | undefined;

  @Event() alignChange: EventEmitter<void>;

  async componentWillLoad() {
    this.align = await AlignUtils.getAlignment(this.selectedElement);
  }

  private async updateAlign(align: TextAlign): Promise<void> {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.textAlign = align;
    this.align = align;

    this.alignChange.emit();
  }

  render() {
    if (this.align === undefined) {
      return undefined;
    }

    return (
      <app-expansion-panel>
        <ion-label slot="title">Alignment</ion-label>
        <ion-list>
          {this.renderAlign(TextAlign.LEFT, 'Align left')}
          {this.renderAlign(TextAlign.CENTER, 'Align center')}
          {this.renderAlign(TextAlign.RIGHT, 'Align right')}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderAlign(align: TextAlign, text: string) {
    return (
      <ion-item onClick={() => this.updateAlign(align)} class={`align ${this.align == align ? 'active' : undefined}`}>
        <ion-icon slot="start" src={`/assets/icons/align-${align}.svg`} role="presentation"></ion-icon>
        <ion-label>{text}</ion-label>
      </ion-item>
    );
  }
}
