import {Component, Element, EventEmitter, h, Prop, State, Event} from '@stencil/core';

import {alertController} from '@ionic/core';

import i18n from '../../../../stores/i18n.store';

import store from '../../../../stores/deck.store';

import {QRCodeUtils} from '../../../../utils/editor/qrcode.utils';
import {EditAction} from '../../../../types/editor/edit-action';

@Component({
  tag: 'app-edit-slide-qrcode',
})
export class AppEditSlideQRCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private customQRCode: boolean = false;

  @State()
  private customContent: string = undefined;

  @Event()
  private action: EventEmitter<EditAction>;

  async componentWillLoad() {
    this.customQRCode = this.selectedElement && this.selectedElement.hasAttribute('custom-qrcode');
    this.customContent = this.customQRCode && this.selectedElement ? this.selectedElement.getAttribute('content') : undefined;
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
      if (!this.selectedElement) {
        resolve();
        return;
      }

      const valid: boolean = await this.validateCustomUrl();

      if (!valid) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('content', this.customContent);
      this.selectedElement.setAttribute('custom-qrcode', `${true}`);

      this.slideDidChange.emit(this.selectedElement);

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
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (this.customQRCode) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('content', QRCodeUtils.getPresentationUrl(store.state.deck));
      this.selectedElement.removeAttribute('custom-qrcode');

      this.customContent = undefined;

      this.slideDidChange.emit(this.selectedElement);

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
            <ion-icon name="help"></ion-icon>
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

      <ion-item class="action-button ion-margin-top">
        <ion-button shape="round" onClick={() => this.action.emit(EditAction.OPEN_CUSTOM_LOGO)} color="tertiary">
          <ion-label>{i18n.state.editor.your_logo}</ion-label>
        </ion-button>
      </ion-item>,

      <ion-item class="action-button ion-margin-bottom">
        <ion-button shape="round" onClick={() => this.action.emit(EditAction.DELETE_LOGO)} fill="outline" class="delete">
          <ion-label>{i18n.state.editor.delete_logo}</ion-label>
        </ion-button>
      </ion-item>,
    ];
  }
}
