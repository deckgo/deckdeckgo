import {Component, Event, EventEmitter, Fragment, h} from '@stencil/core';
import i18n from '../../../stores/i18n.store';

@Component({
  tag: 'app-popover-confirm',
  styleUrl: 'app-popover-confirm.scss'
})
export class AppPopoverConfirm {
  @Event()
  confirm: EventEmitter<boolean>;

  render() {
    return (
      <Fragment>
        <button class="no ion-activatable" onClick={() => this.confirm.emit(false)}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-label>{i18n.state.core.no}</ion-label>
        </button>

        <button class="yes ion-activatable" onClick={() => this.confirm.emit(true)}>
          <ion-ripple-effect></ion-ripple-effect>
          <ion-label>
            <strong>{i18n.state.core.yes}</strong>
          </ion-label>
        </button>
      </Fragment>
    );
  }
}
