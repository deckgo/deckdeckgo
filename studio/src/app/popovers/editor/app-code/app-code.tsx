import {Component, Element, EventEmitter, Prop, State, h} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import i18n from '../../../stores/i18n.store';

import {getCodeLanguage} from '../../../utils/editor/prism.utils';

import {PrismLanguage} from '../../../types/editor/prism-language';

@Component({
  tag: 'app-code',
  styleUrl: 'app-code.scss',
})
export class AppCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  codeDidChange: EventEmitter<HTMLElement>;

  @State()
  private currentLanguage: PrismLanguage | undefined;

  @State()
  private lineNumbers: boolean = false;

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async initCurrent() {
    await this.initCurrentLanguage();

    this.lineNumbers = this.selectedElement && this.selectedElement.hasAttribute('line-numbers');
  }

  private async initCurrentLanguage() {
    const language: string =
      this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';
    this.currentLanguage = await getCodeLanguage(language);
  }

  private emitCodeDidChange() {
    this.codeDidChange.emit(this.selectedElement);
  }

  private toggleLineNumbers($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('line-numbers', $event.detail.checked);

      this.emitCodeDidChange();

      resolve();
    });
  }

  private async openCodeLanguage() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-code-languages',
      componentProps: {
        selectedElement: this.selectedElement,
        codeDidChange: this.codeDidChange,
        currentLanguage: this.currentLanguage,
      },
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        this.currentLanguage = detail.data;
      }
    });

    await modal.present();
  }

  render() {
    return [
      <ion-toolbar>
        <h2>{i18n.state.editor.code_options}</h2>
        <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
      </ion-toolbar>,
      <ion-list class="article">
        <ion-item-divider>
          <ion-label>{i18n.state.editor.language}</ion-label>
        </ion-item-divider>

        <ion-item onClick={() => this.openCodeLanguage()} class="select-language">
          <div>
            <ion-label>{this.currentLanguage ? this.currentLanguage.title : ''}</ion-label>

            <div class="select-icon" role="presentation">
              <div class="select-icon-inner"></div>
            </div>
          </div>
        </ion-item>

        <ion-item>
          <ion-label>{i18n.state.editor.display_line_number}</ion-label>
          <ion-checkbox slot="end" checked={this.lineNumbers} onIonChange={($event: CustomEvent) => this.toggleLineNumbers($event)}></ion-checkbox>
        </ion-item>
      </ion-list>,
    ];
  }
}
