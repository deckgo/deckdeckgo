import {Component, EventEmitter, h, Host, Prop, Event} from '@stencil/core';

import {DeckdeckgoInlineEditorUtils} from '../../../utils/utils';

@Component({
  tag: 'deckgo-ie-style-actions',
  styleUrl: 'style-actions.scss',
  shadow: true,
})
export class StyleActions {
  @Prop()
  disabledTitle: boolean = false;

  @Prop()
  selection: Selection;

  @Prop()
  mobile: boolean;

  @Prop()
  bold: boolean;

  @Prop()
  italic: boolean;

  @Prop()
  underline: boolean;

  @Event()
  private initStyle: EventEmitter;

  private async styleBold($event: UIEvent): Promise<void> {
    $event.stopPropagation();

    await DeckdeckgoInlineEditorUtils.execCommand(this.selection, 'bold');

    this.initStyle.emit();
  }

  private async styleItalic($event: UIEvent): Promise<void> {
    $event.stopPropagation();

    await DeckdeckgoInlineEditorUtils.execCommand(this.selection, 'italic');

    this.initStyle.emit();
  }

  private async styleUnderline($event: UIEvent): Promise<void> {
    $event.stopPropagation();

    await DeckdeckgoInlineEditorUtils.execCommand(this.selection, 'underline');

    this.initStyle.emit();
  }

  render() {
    return (
      <Host>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.styleBold($event.detail)}
          disableAction={this.disabledTitle}
          cssClass={this.bold ? 'bold active' : 'bold'}>
          <span>B</span>
        </deckgo-ie-action-button>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.styleItalic($event.detail)}
          cssClass={this.italic ? 'italic active' : 'italic'}>
          <span>I</span>
        </deckgo-ie-action-button>
        <deckgo-ie-action-button
          mobile={this.mobile}
          onAction={($event: CustomEvent<UIEvent>) => this.styleUnderline($event.detail)}
          cssClass={this.underline ? 'underline active' : 'underline'}>
          <span>U</span>
        </deckgo-ie-action-button>
      </Host>
    );
  }
}
