import {Component, Element, EventEmitter, Prop, State, h, Fragment} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';
import {alertController, modalController} from '@ionic/core';

import i18n from '../../../stores/i18n.store';

import {getCodeLanguage} from '../../../utils/editor/prism.utils';

import {PrismLanguage} from '../../../types/editor/prism-language';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-code',
  styleUrl: 'app-code.scss'
})
export class AppCode {
  @Element() el: HTMLElement;

  @Prop()
  selectedTarget: HTMLElement;

  @Prop()
  codeDidChange: EventEmitter<HTMLElement> | undefined;

  @State()
  private currentLanguage: PrismLanguage | undefined;

  @State()
  private lineNumbers: boolean = false;

  @State()
  private highlightLines: string;

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async initCurrent() {
    await this.initCurrentLanguage();

    this.lineNumbers = this.selectedTarget && this.selectedTarget.hasAttribute('line-numbers');

    this.highlightLines = this.selectedTarget?.getAttribute('highlight-lines') ?? null;
  }

  private async initCurrentLanguage() {
    const language: string =
      this.selectedTarget && this.selectedTarget.getAttribute('language') ? this.selectedTarget.getAttribute('language') : 'javascript';
    this.currentLanguage = await getCodeLanguage(language);
  }

  private emitCodeDidChange() {
    this.codeDidChange?.emit(this.selectedTarget);
  }

  private toggleLineNumbers($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedTarget) {
        resolve();
        return;
      }

      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.selectedTarget.setAttribute('line-numbers', $event.detail.checked);

      this.emitCodeDidChange();

      resolve();
    });
  }

  private async openCodeLanguage() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-code-languages',
      componentProps: {
        selectedTarget: this.selectedTarget,
        codeDidChange: this.codeDidChange,
        currentLanguage: this.currentLanguage
      }
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        this.currentLanguage = detail.data;
      }
    });

    await modal.present();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.highlightLines = ($event.target as InputTargetEvent).value;
  }

  private async highlightSelectedLines() {
    if (!this.selectedTarget) {
      return;
    }

    if (this.highlightLines && this.highlightLines.length > 0) {
      this.selectedTarget.setAttribute('highlight-lines', this.highlightLines);
    } else {
      this.selectedTarget.removeAttribute('highlight-lines');
    }

    // Reload component with new lines to highlight
    await (this.selectedTarget as HTMLDeckgoHighlightCodeElement).load();

    this.emitCodeDidChange();
  }

  // prettier-ignore
  private async presentHighlightInfo($event: UIEvent) {
    $event.stopPropagation();

    const alert: HTMLIonAlertElement = await alertController.create({
      message: 'If you wish to highlight some specific lines of your code, list line numbers separately using comma. Group separated with dash.<br/><br/>For example: 1 4-5 13-15<br/><br/>Which highlights line 1, lines 4 to 5 and 13 to 15.',
      buttons: ['Ok']
    });

    return await alert.present();
  }

  render() {
    return (
      <Fragment>
        <ion-toolbar>
          <h2>{i18n.state.editor.code_options}</h2>
          <app-close-menu slot="end" onClose={() => this.closePopover()}></app-close-menu>
        </ion-toolbar>

        <ion-list class="article">
          <ion-item-divider>
            <ion-label class="language">{i18n.state.editor.language}</ion-label>
          </ion-item-divider>

          <ion-item onClick={() => this.openCodeLanguage()} class="select-language">
            <div>
              <ion-label>{this.currentLanguage ? this.currentLanguage.title : ''}</ion-label>

              <div class="select-icon" role="presentation">
                <div class="select-icon-inner"></div>
              </div>
            </div>
          </ion-item>

          <ion-item-divider class="ion-margin-top">
            <ion-label>{i18n.state.editor.highlight_lines}</ion-label>

            <button slot="end" class="info" onClick={($event: UIEvent) => this.presentHighlightInfo($event)}>
              <AppIcon name="help" ariaLabel="" ariaHidden={true}></AppIcon>
            </button>
          </ion-item-divider>

          <ion-item class="with-padding">
            <ion-input
              value={this.highlightLines}
              placeholder={i18n.state.editor.highlight_lines}
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={() => this.highlightSelectedLines()}></ion-input>
          </ion-item>

          <ion-item class="ion-margin-top ion-margin-bottom">
            <ion-label>{i18n.state.editor.display_line_number}</ion-label>
            <ion-checkbox
              slot="end"
              checked={this.lineNumbers}
              onIonChange={($event: CustomEvent) => this.toggleLineNumbers($event)}></ion-checkbox>
          </ion-item>
        </ion-list>
      </Fragment>
    );
  }
}
