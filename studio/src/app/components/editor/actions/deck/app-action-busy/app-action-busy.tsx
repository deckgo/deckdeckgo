import {Component, Event, EventEmitter, State, h, Prop} from '@stencil/core';

import {Subscription} from 'rxjs';

import {BusyService} from '../../../../../services/editor/busy/busy.service';

@Component({
  tag: 'app-action-busy',
  styleUrl: 'app-action-busy.scss',
  shadow: false
})
export class AppActionBusy {
  @Event() private actionReady: EventEmitter<UIEvent>;

  private subscription: Subscription;
  private busyService: BusyService;

  @State()
  private deckBusy: boolean = false;

  @Prop()
  iconName: string;

  constructor() {
    this.busyService = BusyService.getInstance();
  }

  private action($event: UIEvent) {
    this.actionReady.emit($event);
  }

  async componentWillLoad() {
    this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
      this.deckBusy = busy;
    });
  }

  async componentDidUnload() {
    if (this.subscription) {
      this.subscription.unsubscribe();
    }
  }

  render() {
    return (
      <ion-tab-button onClick={(e: UIEvent) => this.action(e)} color="primary" disabled={this.deckBusy} mode="md">
        <ion-icon name={this.iconName}></ion-icon>
        <slot></slot>
      </ion-tab-button>
    );
  }
}
