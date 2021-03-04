import {Component, Event, EventEmitter, h, Host} from '@stencil/core';

import store from '../../../stores/deck.store';
import i18n from '../../../stores/i18n.store';

import {MoreAction} from '../../../types/editor/more-action';

@Component({
  tag: 'app-share-options',
  styleUrl: 'app-share-options.scss',
  shadow: true,
})
export class AppMoreShareOptions {
  @Event() selectedOption: EventEmitter<MoreAction>;

  render() {
    return (
      <Host>
        {this.renderUpdate()}
        {this.renderEmbed()}
        {this.renderShareLink()}
      </Host>
    );
  }

  private renderUpdate() {
    if (store.state.published) {
      return (
        <a onClick={() => this.selectedOption.emit(MoreAction.PUBLISH)}>
          <p>{i18n.state.share.update_share}</p>
        </a>
      );
    } else {
      return undefined;
    }
  }

  private renderEmbed() {
    if (store.state.published) {
      return (
        <a onClick={() => this.selectedOption.emit(MoreAction.EMBED)}>
          <p>{i18n.state.share.embed}</p>
        </a>
      );
    } else {
      return undefined;
    }
  }

  private renderShareLink() {
    if (store.state.published) {
      return (
        <a onClick={() => this.selectedOption.emit(MoreAction.SHARE)}>
          <p>{i18n.state.share.link}</p>
        </a>
      );
    } else {
      return (
        <a onClick={() => this.selectedOption.emit(MoreAction.PUBLISH)}>
          <p>{i18n.state.editor.share}</p>
        </a>
      );
    }
  }
}
