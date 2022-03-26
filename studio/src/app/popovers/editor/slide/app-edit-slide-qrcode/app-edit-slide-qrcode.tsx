import {alertController} from '@ionic/core';
import {Component, Element, Event, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';
import {AppIcon} from '../../../../components/core/app-icon/app-icon';
import {publishUrl} from '../../../../providers/publish/publish.provider';
import editorStore from '../../../../stores/editor.store';
import i18n from '../../../../stores/i18n.store';
import offlineStore from '../../../../stores/offline.store';
import {EditAction} from '../../../../types/editor/edit-action';

@Component({
  tag: 'app-edit-slide-qrcode'
})
export class AppEditSlideQRCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private customQRCode: boolean = false;

  @State()
  private customContent: string = undefined;

  @Event()
  private action: EventEmitter<EditAction>;

  async componentWillLoad() {
    this.customQRCode = this.selectedTarget && this.selectedTarget.hasAttribute('custom-qrcode');
    this.customContent = this.customQRCode && this.selectedTarget ? this.selectedTarget.getAttribute('content') : undefined;
  }

  private async onRadioCustomLink($event: CustomEvent) {
    if ($event && $event.detail && $event.detail.value !== this.customQRCode) {
      this.customQRCode = $event.detail.value;

      await this.setDefaultQRCodeLink();
    }
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.customContent = ($event.target as InputTargetEvent).value;
  }

  private onInputCustomUrlChange(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      const valid: boolean = await this.validateCustomUrl();

      if (!valid) {
        resolve();
        return;
      }

      this.selectedTarget.setAttribute('content', this.customContent);
      this.selectedTarget.setAttribute('custom-qrcode', `${true}`);

      this.slideDidChange.emit(this.selectedTarget);

      resolve();
    });
  }

  private validateCustomUrl(): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      try {
        if (!this.customContent) {
          resolve(false);
          return;
        }

        new URL(this.customContent);

        // https://stackoverflow.com/a/14582229/5404186
        const pattern: RegExp = new RegExp(
          '^(https?:\\/\\/)?' + // protocol
            '((([a-z\\d]([a-z\\d-]*[a-z\\d])*)\\.?)+[a-z]{2,}|' + // domain name
            '((\\d{1,3}\\.){3}\\d{1,3}))' + // OR ip (v4) address
            '(\\:\\d+)?(\\/[-a-z\\d%_.~+]*)*' + // port and path
            '(\\?[;&a-z\\d%_.~+=-]*)?' + // query string
            '(\\#[-a-z\\d_]*)?$',
          'i'
        ); // fragment locator

        resolve(pattern.test(this.customContent));
      } catch (err) {
        resolve(false);
      }
    });
  }

  private setDefaultQRCodeLink(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (this.customQRCode) {
        resolve();
        return;
      }

      const url: string = await publishUrl(editorStore.state.deck.data.meta);

      this.selectedTarget.setAttribute('content', url);
      this.selectedTarget.removeAttribute('custom-qrcode');

      this.customContent = undefined;

      this.slideDidChange.emit(this.selectedTarget);

      resolve();
    });
  }

  // prettier-ignore
  private async presentQRCodeInfo() {
    const alert: HTMLIonAlertElement = await alertController.create({
      message: i18n.state.editor.qr_code_explanation,
      buttons: ['Ok']
    });

    return await alert.present();
  }

  render() {
    return [
      <ion-radio-group onIonChange={($event) => this.onRadioCustomLink($event)} value={this.customQRCode}>
        <ion-item-divider class="ion-padding-top">
          <ion-label>{i18n.state.editor.target}</ion-label>
          <button slot="end" class="info" onClick={() => this.presentQRCodeInfo()}>
            <AppIcon name="help" ariaLabel="" ariaHidden={true}></AppIcon>
          </button>
        </ion-item-divider>

        <ion-item>
          <ion-label>{i18n.state.editor.your_presentation}</ion-label>
          <ion-radio slot="start" value={false} mode="md"></ion-radio>
        </ion-item>
        <ion-item>
          <ion-label>{i18n.state.editor.custom_url}</ion-label>
          <ion-radio slot="start" value={true} mode="md"></ion-radio>
        </ion-item>
      </ion-radio-group>,

      <ion-item class="with-padding">
        <ion-input
          value={this.customContent}
          placeholder="https://..."
          debounce={500}
          disabled={!this.customQRCode}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
          onIonChange={() => this.onInputCustomUrlChange()}></ion-input>
      </ion-item>,

      this.renderLogoOptions()
    ];
  }

  // For simplicity reason, currently logo on QR code are limited to signed in and online users.
  // PR Welcome:
  // It would need an improvement in the QR-code slide so that it do not use an image tag but, a custom deckgo lazy loading image tag.
  // In addition, handlers would have to be registered to handle the loading of images on and off line.
  // In the sync process, when images are uploaded to the cloud, their new URLs should be updated on the slide attributes.
  // Finally, instead of opening a "restricted custom modal", it should open the standard modal.
  private renderLogoOptions() {
    if (!offlineStore.state.online) {
      return undefined;
    }

    return (
      <Fragment>
        <ion-item class="action-button ion-margin-top">
          <ion-button shape="round" onClick={() => this.action.emit(EditAction.OPEN_CUSTOM_LOGO)} color="tertiary">
            <ion-label>{i18n.state.editor.your_logo}</ion-label>
          </ion-button>
        </ion-item>

        <ion-item class="action-button ion-margin-bottom">
          <ion-button shape="round" onClick={() => this.action.emit(EditAction.DELETE_LOGO)} fill="outline" class="delete">
            <ion-label>{i18n.state.editor.delete_logo}</ion-label>
          </ion-button>
        </ion-item>
      </Fragment>
    );
  }
}
