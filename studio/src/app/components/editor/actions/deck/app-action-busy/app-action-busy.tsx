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
      <button onClick={(e: UIEvent) => this.action(e)} disabled={store.state.deckBusy} class="ion-activatable">
        <ion-ripple-effect></ion-ripple-effect>
        <ion-icon aria-hidden="true" src={this.iconSrc}></ion-icon>
        <slot></slot>
      </button>
    );
  }
}
