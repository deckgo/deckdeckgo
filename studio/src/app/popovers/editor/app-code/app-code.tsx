import {Component, Element, EventEmitter, Prop, State, h} from '@stencil/core';

import {modalController, OverlayEventDetail} from '@ionic/core';

import {DeckdeckgoHighlightCodeTerminal, DeckdeckgoHighlightCodeCarbonTheme} from '@deckdeckgo/highlight-code';

import {PrismLanguage, PrismService} from '../../../services/editor/prism/prism.service';

enum CodeFontSize {
  VERY_SMALL,
  SMALL,
  NORMAL,
  BIG,
  VERY_BIG,
}

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
  private currentFontSize: CodeFontSize = undefined;

  @State()
  private lineNumbers: boolean = false;

  @State()
  private terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;

  @State()
  private theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

  private prismService: PrismService;

  constructor() {
    this.prismService = PrismService.getInstance();
  }

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private initCurrent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.initCurrentLanguage();

      this.currentFontSize = await this.initFontSize();
      this.lineNumbers = this.selectedElement && this.selectedElement.hasAttribute('line-numbers');
      this.terminal =
        this.selectedElement && this.selectedElement.hasAttribute('terminal')
          ? (this.selectedElement.getAttribute('terminal') as DeckdeckgoHighlightCodeTerminal)
          : DeckdeckgoHighlightCodeTerminal.CARBON;

      this.theme =
        this.selectedElement && this.selectedElement.hasAttribute('theme')
          ? (this.selectedElement.getAttribute('theme') as DeckdeckgoHighlightCodeCarbonTheme)
          : DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

      resolve();
    });
  }

  private async initCurrentLanguage() {
    const language: string =
      this.selectedElement && this.selectedElement.getAttribute('language') ? this.selectedElement.getAttribute('language') : 'javascript';
    this.currentLanguage = await this.prismService.getLanguage(language);
  }

  private emitCodeDidChange() {
    this.codeDidChange.emit(this.selectedElement);
  }

  private initFontSize(): Promise<CodeFontSize> {
    return new Promise<CodeFontSize>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        resolve(null);
        return;
      }

      const property: string = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-font-size');

      if (property === '50%') {
        resolve(CodeFontSize.VERY_SMALL);
      } else if (property === '75%') {
        resolve(CodeFontSize.SMALL);
      } else if (property === '150%') {
        resolve(CodeFontSize.BIG);
      } else if (property === '200%') {
        resolve(CodeFontSize.VERY_BIG);
      } else {
        resolve(CodeFontSize.NORMAL);
      }
    });
  }

  private toggleFontSize($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.currentFontSize = $event.detail.value;

      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.style.removeProperty('--deckgo-highlight-code-font-size');

      if (this.currentFontSize === CodeFontSize.VERY_SMALL) {
        this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '50%');
      } else if (this.currentFontSize === CodeFontSize.SMALL) {
        this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '75%');
      } else if (this.currentFontSize === CodeFontSize.BIG) {
        this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '150%');
      } else if (this.currentFontSize === CodeFontSize.VERY_BIG) {
        this.selectedElement.style.setProperty('--deckgo-highlight-code-font-size', '200%');
      }

      this.emitCodeDidChange();

      resolve();
    });
  }

  private toggle($event: CustomEvent, attribute: 'terminal' | 'theme'): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (attribute === 'terminal') {
        this.terminal = $event.detail.value;
      } else if (attribute === 'theme') {
        this.theme = $event.detail.value;
      }

      this.selectedElement.setAttribute(attribute, $event.detail.value);

      this.emitCodeDidChange();

      resolve();
    });
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
        <h2>Code attributes</h2>
        <ion-router-link slot="end" onClick={() => this.closePopover()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <ion-list>
        <ion-item-divider>
          <ion-label>Language</ion-label>
        </ion-item-divider>

        <ion-item onClick={() => this.openCodeLanguage()} class="select-language">
          <div>
            <ion-label>{this.currentLanguage ? this.currentLanguage.title : ''}</ion-label>

            <div class="select-icon" role="presentation">
              <div class="select-icon-inner"></div>
            </div>
          </div>
        </ion-item>

        <ion-item-divider class="ion-padding-top">
          <ion-label>Font size</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-label>Size</ion-label>

          <ion-select
            value={this.currentFontSize}
            placeholder="Select a font size"
            onIonChange={($event: CustomEvent) => this.toggleFontSize($event)}
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={CodeFontSize.VERY_SMALL}>Very small</ion-select-option>
            <ion-select-option value={CodeFontSize.SMALL}>Small</ion-select-option>
            <ion-select-option value={CodeFontSize.NORMAL}>Normal</ion-select-option>
            <ion-select-option value={CodeFontSize.BIG}>Big</ion-select-option>
            <ion-select-option value={CodeFontSize.VERY_BIG}>Very big</ion-select-option>
          </ion-select>
        </ion-item>

        <ion-item-divider class="ion-padding-top">
          <ion-label>Terminal</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-label>Terminal</ion-label>

          <ion-select
            value={this.terminal}
            placeholder="Select a terminal"
            onIonChange={($event: CustomEvent) => this.toggle($event, 'terminal')}
            class="ion-padding-start ion-padding-end ion-text-capitalize">
            {Object.keys(DeckdeckgoHighlightCodeTerminal).map((key: string) => {
              return (
                <ion-select-option value={DeckdeckgoHighlightCodeTerminal[key]}>
                  {DeckdeckgoHighlightCodeTerminal[key].replace(/^\w/, (c) => c.toUpperCase())}
                </ion-select-option>
              );
            })}
          </ion-select>
        </ion-item>

        <ion-item-divider class="ion-padding-top">
          <ion-label>Theme</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-label>Theme</ion-label>

          <ion-select
            value={this.theme}
            placeholder="Select a theme"
            disabled={this.terminal !== DeckdeckgoHighlightCodeTerminal.CARBON}
            onIonChange={($event: CustomEvent) => this.toggle($event, 'theme')}
            class="ion-padding-start ion-padding-end ion-text-capitalize">
            {Object.keys(DeckdeckgoHighlightCodeCarbonTheme).map((key: string) => {
              return (
                <ion-select-option value={DeckdeckgoHighlightCodeCarbonTheme[key]}>
                  {DeckdeckgoHighlightCodeCarbonTheme[key].replace(/^\w/, (c) => c.toUpperCase())}
                </ion-select-option>
              );
            })}
          </ion-select>
        </ion-item>

        <ion-item>
          <ion-label>Display line numbers</ion-label>
          <ion-checkbox slot="end" checked={this.lineNumbers} onIonChange={($event: CustomEvent) => this.toggleLineNumbers($event)}></ion-checkbox>
        </ion-item>
      </ion-list>,
    ];
  }
}
