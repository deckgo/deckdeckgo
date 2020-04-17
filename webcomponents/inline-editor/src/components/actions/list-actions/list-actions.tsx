import {Component, Event, EventEmitter, h, Host, Prop} from '@stencil/core';
import {DeckdeckgoInlineEditorUtils} from '../../../utils/utils';
import {ContentList} from '../../../types/enums';

@Component({
  tag: 'deckgo-ie-list-actions',
  styleUrl: 'list-actions.scss',
  shadow: true,
})
export class AlignActions {
  @Prop()
  selection: Selection;

  @Prop()
  disabledTitle: boolean = false;

  @Prop()
  mobile: boolean;

  @Prop()
  sticky: boolean;

  @Prop()
  contentList: ContentList;

  @Event()
  listModified: EventEmitter<void>;

  private toggleList(e: UIEvent, cmd: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      await DeckdeckgoInlineEditorUtils.execCommand(this.selection, cmd);

      await this.listModified.emit();

      resolve();
    });
  }

  render() {
    return (
      <Host class={this.sticky ? 'deckgo-tools-sticky' : undefined}>
        <deckgo-ie-action-button
          mobile={this.mobile}
          disableAction={this.disabledTitle}
          onAction={($event: CustomEvent<UIEvent>) => this.toggleList($event.detail, 'insertOrderedList')}
          class={this.contentList === ContentList.ORDERED ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'ordered-list'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>

        <deckgo-ie-action-button
          mobile={this.mobile}
          disableAction={this.disabledTitle}
          onAction={($event: CustomEvent<UIEvent>) => this.toggleList($event.detail, 'insertUnorderedList')}
          class={this.contentList === ContentList.UNORDERED ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'unordered-list'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
      </Host>
    );
  }
}
