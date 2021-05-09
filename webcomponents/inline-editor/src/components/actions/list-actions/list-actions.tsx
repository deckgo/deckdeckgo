import {Component, Event, EventEmitter, h, Host, Prop} from '@stencil/core';

import {ContentList} from '../../../types/enums';
import {ExecCommandAction} from '../../../interfaces/interfaces';

@Component({
  tag: 'deckgo-ie-list-actions',
  styleUrl: 'list-actions.scss',
  shadow: true,
})
export class AlignActions {
  @Prop()
  disabledTitle: boolean = false;

  @Prop()
  mobile: boolean;

  @Prop()
  sticky: boolean;

  @Prop()
  contentList: ContentList;

  @Event()
  private execCommand: EventEmitter<ExecCommandAction>;

  private toggleList(e: UIEvent, type: 'ol' | 'ul'): Promise<void> {
    return new Promise<void>(async (resolve) => {
      e.stopPropagation();

      this.execCommand.emit({
        cmd: 'list',
        detail: {
          type,
        },
      });

      resolve();
    });
  }

  render() {
    return (
      <Host class={this.sticky ? 'deckgo-tools-sticky' : undefined}>
        <deckgo-ie-action-button
          mobile={this.mobile}
          disableAction={this.disabledTitle}
          onAction={($event: CustomEvent<UIEvent>) => this.toggleList($event.detail, 'ol')}
          class={this.contentList === ContentList.ORDERED ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'ordered-list'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>

        <deckgo-ie-action-button
          mobile={this.mobile}
          disableAction={this.disabledTitle}
          onAction={($event: CustomEvent<UIEvent>) => this.toggleList($event.detail, 'ul')}
          class={this.contentList === ContentList.UNORDERED ? 'active' : undefined}>
          <deckgo-ie-action-image cssClass={'unordered-list'}></deckgo-ie-action-image>
        </deckgo-ie-action-button>
      </Host>
    );
  }
}
