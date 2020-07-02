import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import store from '../../../../../stores/busy.store';

@Component({
  tag: 'app-action-busy',
  styleUrl: 'app-action-busy.scss',
  shadow: false,
})
export class AppActionBusy {
  @Event() private actionReady: EventEmitter<UIEvent>;

  @Prop()
  iconSrc: string;

  private action($event: UIEvent) {
    this.actionReady.emit($event);
  }

  render() {
    return (
      <ion-tab-button onClick={(e: UIEvent) => this.action(e)} color="primary" disabled={store.state.deckBusy} mode="md">
        <ion-icon src={this.iconSrc}></ion-icon>
        <slot></slot>
      </ion-tab-button>
    );
  }
}
