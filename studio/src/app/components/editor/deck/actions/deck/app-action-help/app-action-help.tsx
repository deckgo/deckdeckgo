import {popoverController} from '@ionic/core';
import {Component, Element, Event, EventEmitter, h, Prop} from '@stencil/core';
import i18n from '../../../../../../stores/i18n.store';
import {AppIcon} from '../../../../../core/app-icon/app-icon';

@Component({
  tag: 'app-action-help'
})
export class AppActionHelp {
  @Element() el: HTMLElement;

  @Prop()
  link: boolean = false;

  @Event()
  private helpSelected: EventEmitter<void>;

  private async openGetHelp($event?: UIEvent) {
    this.helpSelected.emit();

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-get-help',
      mode: 'ios',
      event: $event,
      cssClass: 'info'
    });

    await popover.present();
  }

  render() {
    if (this.link) {
      return (
        <a onClick={() => this.openGetHelp()} aria-label="Help">
          <p>{i18n.state.editor.help}</p>
        </a>
      );
    } else {
      return (
        <button
          onMouseDown={($event) => $event.stopPropagation()}
          onTouchStart={($event) => $event.stopPropagation()}
          aria-label={i18n.state.editor.help}
          onClick={($event: UIEvent) => this.openGetHelp($event)}
          class="get-help-action ion-activatable">
          <ion-ripple-effect></ion-ripple-effect>
          <AppIcon name="help" ariaLabel="" ariaHidden={true}></AppIcon>
          <ion-label aria-hidden="true">{i18n.state.editor.help}</ion-label>
        </button>
      );
    }
  }
}
