import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import i18n from '../../../../../stores/i18n.store';

import {FontsService} from '../../../../../services/editor/fonts/fonts.service';

@Component({
  tag: 'app-deck-fonts',
  styleUrl: 'app-deck-fonts.scss',
})
export class AppDeckFonts {
  @Prop()
  deckElement: HTMLElement;

  @Event() fontsChange: EventEmitter<void>;

  @State()
  private selectedFont: string | undefined;

  @State()
  private fonts: GoogleFont[];

  private fontsService: FontsService;

  constructor() {
    this.fontsService = FontsService.getInstance();
  }

  async componentWillLoad() {
    await this.initSelectedFont();

    this.fonts = await this.fontsService.loadAllGoogleFonts();
  }

  private initSelectedFont(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.deckElement || !this.deckElement.style || !this.deckElement.style.getPropertyValue('font-family')) {
        this.selectedFont = undefined;
        resolve();
        return;
      }

      this.selectedFont = this.deckElement.style.getPropertyValue('font-family').replace(/\'/g, '').replace(/"/g, '');

      resolve();
    });
  }

  private async selectFont(font: GoogleFont | null) {
    if (!this.deckElement) {
      return;
    }

    if (!font) {
      this.deckElement.style.removeProperty('font-family');
    } else {
      this.deckElement.style.setProperty('font-family', font.family);
    }

    this.fontsChange.emit();

    await this.initSelectedFont();
  }

  render() {
    return [
      <app-color-text-background selectedElement={this.deckElement} deck={true} onColorChange={() => this.fontsChange.emit()}></app-color-text-background>,
      this.renderFonts(),
    ];
  }

  private renderFonts() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">{i18n.state.editor.typography}</ion-label>
        <div class="container ion-margin-bottom">
          {this.renderDefaultFont(this.selectedFont === undefined)}
          {this.renderGoogleFonts()}
        </div>
      </app-expansion-panel>
    );
  }

  private renderGoogleFonts() {
    if (this.fonts === undefined) {
      return undefined;
    }

    return this.fonts.map((font: GoogleFont) => {
      return this.renderFont(font, this.selectedFont === font.family.replace(/\'/g, ''));
    });
  }

  private renderDefaultFont(selected: boolean) {
    return (
      <div class={`item ${selected ? 'selected' : ''}`} custom-tappable onClick={() => this.selectFont(null)}>
        <deckgo-slide-title class="showcase">
          <p slot="title" class="default">
            {i18n.state.editor.default}
          </p>
        </deckgo-slide-title>
      </div>
    );
  }

  private renderFont(font: GoogleFont, selected: boolean) {
    return (
      <div class={`item ${selected ? 'selected' : ''}`} custom-tappable onClick={() => this.selectFont(font)}>
        <deckgo-slide-title class="showcase">
          <p slot="title" style={{'font-family': font.family}}>
            {font.name}
          </p>
        </deckgo-slide-title>
      </div>
    );
  }
}
