import {Component, EventEmitter, h, Prop, Event} from '@stencil/core';

import {ContentAlign} from '../../../types/enums';

import {DeckdeckgoInlineEditorUtils} from '../../../utils/utils';

@Component({
  tag: 'deckgo-ie-align-actions',
  styleUrl: 'align-actions.scss',
  shadow: true,
})
export class AlignActions {
  @Prop()
  selection: Selection;

  @Prop()
  mobile: boolean;

  @Prop()
  contentAlign: ContentAlign;

  @Event()
  private initStyle: EventEmitter;

  private justifyContent(e: UIEvent, align: ContentAlign): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await DeckdeckgoInlineEditorUtils.execCommand(this.selection, align.toString());

      await this.initStyle.emit();

      resolve();
    });
  }

  render() {
    return [
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.LEFT)}
        cssClass={this.contentAlign === ContentAlign.LEFT ? 'active' : undefined}>
        <deckgo-ie-action-image cssClass={'left-align'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.CENTER)}
        cssClass={this.contentAlign === ContentAlign.CENTER ? 'active' : undefined}>
        <deckgo-ie-action-image cssClass={'center-align'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
      <deckgo-ie-action-button
        mobile={this.mobile}
        onAction={($event: CustomEvent<UIEvent>) => this.justifyContent($event.detail, ContentAlign.RIGHT)}
        cssClass={this.contentAlign === ContentAlign.RIGHT ? 'active' : undefined}>
        <deckgo-ie-action-image cssClass={'right-align'}></deckgo-ie-action-image>
      </deckgo-ie-action-button>,
    ];
  }
}
