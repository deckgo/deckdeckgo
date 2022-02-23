import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';
import {AppIcon} from '../../../../../core/app-icon/app-icon';
import busyStore from '../../../../../../stores/busy.store';

@Component({
  tag: 'app-action-busy',
  styleUrl: 'app-action-busy.scss',
  shadow: false
})
export class AppActionBusy {
  @Event() private actionReady: EventEmitter<UIEvent>;

  @Prop()
  iconName: string;

  private action($event: UIEvent) {
    this.actionReady.emit($event);
  }

  render() {
    return (
      <button
        onClick={(e: UIEvent) => this.action(e)}
        disabled={busyStore.state.busy}
        class="ion-activatable"
        onMouseDown={($event) => $event.stopPropagation()}
        onTouchStart={($event) => $event.stopPropagation()}
      >
        <ion-ripple-effect></ion-ripple-effect>
        <AppIcon name={this.iconName} ariaLabel="" ariaHidden={true}></AppIcon>
        <slot></slot>
      </button>
    );
  }
}
