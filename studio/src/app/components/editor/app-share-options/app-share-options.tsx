import {Component, Event, EventEmitter, h, Host} from '@stencil/core';

import store from '../../../stores/deck.store';

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
          <p>Publish to update share</p>
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
          <p>Embed</p>
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
          <p>Share link</p>
        </a>
      );
    } else {
      return (
        <a onClick={() => this.selectedOption.emit(MoreAction.PUBLISH)}>
          <p>Share</p>
        </a>
      );
    }
  }
}
