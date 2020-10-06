import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {alertController, RangeChangeEventDetail} from '@ionic/core';

import {DeckdeckgoHighlightCodeCarbonTheme, DeckdeckgoHighlightCodeTerminal} from '@deckdeckgo/highlight-code';

import paletteStore from '../../../../../stores/palette.store';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';
import {PaletteUtils} from '../../../../../utils/editor/palette.utils';

enum CodeColorType {
  COMMENTS,
  PUNCTUATION,
  PROPERTY,
  SELECTOR,
  OPERATOR,
  KEYWORD,
  FUNCTION,
  REGEX,
  LINE_NUMBERS,
}

@Component({
  tag: 'app-color-code',
  styleUrl: 'app-color-code.scss',
})
export class AppColorCode {
  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  moreColors: boolean = true;

  @State()
  private codeColorType: CodeColorType = undefined;

  @State()
  private codeColor: string;

  @State()
  private codeColorOpacity: number = 100;

  @State()
  private highlightLines: string;

  @State()
  private highlightColor: string;

  @State()
  private highlightColorOpacity: number = 100;

  @State()
  private terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;

  @State()
  private theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

  @Event() codeDidChange: EventEmitter<void>;

  async componentWillLoad() {
    const promises: Promise<void>[] = [this.initColor(), this.initCurrentHiglight(), this.initTerminal()];

    await Promise.all(promises);
  }

  // prettier-ignore
  private initCurrentHiglight(): Promise<void> {
    return new Promise<void>(async (resolve) => {
            this.highlightLines = this.selectedElement?.getAttribute('highlight-lines') ?? null;

            const color: string = this.selectedElement?.style?.getPropertyValue('--deckgo-highlight-code-line-background') ?? '62,69,100';

      let styleColor: InitStyleColor = await ColorUtils.splitColor(color);

      this.highlightColor = styleColor.rgb ?? color;
      this.highlightColorOpacity = styleColor.opacity;

      resolve();
    });
  }

  private async initTerminal() {
    this.terminal =
      this.selectedElement && this.selectedElement.hasAttribute('terminal')
        ? (this.selectedElement.getAttribute('terminal') as DeckdeckgoHighlightCodeTerminal)
        : DeckdeckgoHighlightCodeTerminal.CARBON;

    this.theme =
      this.selectedElement && this.selectedElement.hasAttribute('theme')
        ? (this.selectedElement.getAttribute('theme') as DeckdeckgoHighlightCodeCarbonTheme)
        : DeckdeckgoHighlightCodeCarbonTheme.DRACULA;
  }

  private selectColor($event: CustomEvent, colorFunction: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.parentElement) {
        resolve();
        return;
      }

      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      await PaletteUtils.updatePalette($event.detail);

      colorFunction($event);

      this.emitColorChange();

      resolve();
    });
  }

  private setCodeColor = async ($event: CustomEvent) => {
    this.codeColor = $event.detail.rgb ?? $event.detail.hex;
    await this.applyCodeColor();
  };

  private setHighlightColor = async ($event: CustomEvent) => {
    this.highlightColor = $event.detail.rgb ?? $event.detail.hex;
    await this.applyHighlightColor();
  };

  private applyCodeColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.codeColor) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.codeColor},${ColorUtils.transformOpacity(this.codeColorOpacity)})`;

      this.selectedElement.style.setProperty(this.getStyle(), selectedColor);

      resolve();
    });
  }

  private applyHighlightColor(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.selectedElement || !this.highlightColor) {
        resolve();
        return;
      }

      const selectedColor: string = `rgba(${this.highlightColor},${ColorUtils.transformOpacity(this.highlightColorOpacity)})`;

      this.selectedElement.style.setProperty('--deckgo-highlight-code-line-background', selectedColor);

      resolve();
    });
  }

  private toggleColorType($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.codeColorType = $event.detail.value;
      await this.initColor();

      resolve();
    });
  }

  private getStyle(): string {
    if (this.codeColorType === CodeColorType.PUNCTUATION) {
      return '--deckgo-highlight-code-token-punctuation';
    } else if (this.codeColorType === CodeColorType.PROPERTY) {
      return '--deckgo-highlight-code-token-property';
    } else if (this.codeColorType === CodeColorType.SELECTOR) {
      return '--deckgo-highlight-code-token-selector';
    } else if (this.codeColorType === CodeColorType.OPERATOR) {
      return '--deckgo-highlight-code-token-operator';
    } else if (this.codeColorType === CodeColorType.KEYWORD) {
      return '--deckgo-highlight-code-token-atrule';
    } else if (this.codeColorType === CodeColorType.FUNCTION) {
      return '--deckgo-highlight-code-token-function';
    } else if (this.codeColorType === CodeColorType.REGEX) {
      return '--deckgo-highlight-code-token-regex';
    } else if (this.codeColorType === CodeColorType.LINE_NUMBERS) {
      return '--deckgo-highlight-code-line-numbers';
    } else {
      return '--deckgo-highlight-code-token-comment';
    }
  }

  // prettier-ignore
  private initColor(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        this.codeColor = undefined;
        this.codeColorOpacity = 100;

        resolve();
        return;
      }

      let color: string;

      if (this.codeColorType === CodeColorType.PUNCTUATION) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-punctuation') ?? '98,114,164';
      } else if (this.codeColorType === CodeColorType.PROPERTY) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-property') ?? '189,147,249';
      } else if (this.codeColorType === CodeColorType.SELECTOR) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-selector') ?? '80,250,123';
      } else if (this.codeColorType === CodeColorType.OPERATOR) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-operator') ?? '255,121,198';
      } else if (this.codeColorType === CodeColorType.KEYWORD) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-atrule') ?? '255,121,198';
      } else if (this.codeColorType === CodeColorType.FUNCTION) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-function') ?? '255,184,108';
      } else if (this.codeColorType === CodeColorType.REGEX) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-regex') ?? '241,250,140';
      } else if (this.codeColorType === CodeColorType.LINE_NUMBERS) {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-line-numbers') ?? '153,153,153';
      } else {
        color = this.selectedElement.style.getPropertyValue('--deckgo-highlight-code-token-comment') ?? '153,153,153';
      }

      let styleColor: InitStyleColor = await ColorUtils.splitColor(color);

      this.codeColor = styleColor.rgb ?? color;
      this.codeColorOpacity = styleColor.opacity;

      resolve();
    });
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.highlightLines = ($event.target as InputTargetEvent).value;
  }

  private highlightSelectedLines(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      const currentHighlight: string = this.selectedElement.getAttribute('highlight-lines');

      if (currentHighlight === this.highlightLines) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('highlight-lines', this.highlightLines);

      // Reload component with new lines to highlight
      await (this.selectedElement as any).load();

      this.emitColorChange();

      resolve();
    });
  }

  // prettier-ignore
  private async presentHighlightInfo($event: UIEvent) {
    $event.stopPropagation();

    const alert: HTMLIonAlertElement = await alertController.create({
            message: 'If you wish to highlight some specific lines of your code, list their line numbers separately using comma.<br/><br/>For example: 1,2 7,7 13,15<br/><br/>Which would highlight lines 1 to 2, line 7 and lines 13 to 15.',
      buttons: ['Ok']
    });

    return await alert.present();
  }

  private emitColorChange() {
    this.codeDidChange.emit();
  }

  private updateOpacity($event: CustomEvent<RangeChangeEventDetail>, opacityFunction: Function): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail.value < 0 || $event.detail.value > 100) {
        resolve();
        return;
      }

      $event.stopPropagation();

      const opacity: number = $event.detail.value as number;

      opacityFunction(opacity);

      this.emitColorChange();

      resolve();
    });
  }

  private setCodeOpacity = async (opacity: number) => {
    this.codeColorOpacity = opacity;
    await this.applyCodeColor();
  };

  private setHighlightOpacity = async (opacity: number) => {
    this.highlightColorOpacity = opacity;
    await this.applyHighlightColor();
  };

  private resetCodeColor(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.style.removeProperty(this.getStyle());

      await this.initColor();

      this.emitColorChange();

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

      this.codeDidChange.emit();

      resolve();
    });
  }

  render() {
    return [this.renderTerminal(), this.renderTheme(), this.renderCategoryColor(), this.renderHighlightLinesColor()];
  }

  private renderCategoryColor() {
    return (
      <app-expansion-panel expanded={'close'}>
        <ion-label slot="title">More colors</ion-label>
        <ion-list>
          <ion-item class="select">
            <ion-label>Apply a color to</ion-label>

            <ion-select
              value={this.codeColorType}
              placeholder="Select a category"
              onIonChange={(e: CustomEvent) => this.toggleColorType(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={CodeColorType.COMMENTS}>Comments</ion-select-option>
              <ion-select-option value={CodeColorType.FUNCTION}>Functions</ion-select-option>
              <ion-select-option value={CodeColorType.KEYWORD}>Keywords</ion-select-option>
              <ion-select-option value={CodeColorType.OPERATOR}>Operators</ion-select-option>
              <ion-select-option value={CodeColorType.PUNCTUATION}>Punctuation</ion-select-option>
              <ion-select-option value={CodeColorType.PROPERTY}>Properties</ion-select-option>
              <ion-select-option value={CodeColorType.REGEX}>Regex</ion-select-option>
              <ion-select-option value={CodeColorType.SELECTOR}>Selector</ion-select-option>
              <ion-select-option value={CodeColorType.LINE_NUMBERS}>Line numbers</ion-select-option>
            </ion-select>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Opacity <small>{this.codeColorOpacity}%</small>
            </ion-label>
          </ion-item-divider>

          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={0}
              max={100}
              disabled={!this.codeColor || this.codeColor === undefined || this.codeColorType === undefined}
              value={this.codeColorOpacity}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity($event, this.setCodeOpacity)}></ion-range>
          </ion-item>

          <div class={this.codeColorType === undefined ? 'ion-padding-start disabled' : 'ion-padding-start'}>
            <deckgo-color
              palette={paletteStore.state.palette}
              class="ion-padding-bottom"
              onColorChange={($event: CustomEvent) => this.selectColor($event, this.setCodeColor)}
              color-rgb={this.codeColor}
              more={this.moreColors}>
              <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
          </div>

          <ion-item class="action-button ion-margin-bottom">
            <ion-button shape="round" onClick={() => this.resetCodeColor()} fill="outline" class="delete">
              <ion-label>Reset color</ion-label>
            </ion-button>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderTerminal() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Terminal</ion-label>

        <ion-list class="terminal">
          <ion-item class="select">
            <ion-label>Terminal</ion-label>

            <ion-select
              value={this.terminal}
              placeholder="Select a terminal"
              onIonChange={($event: CustomEvent) => this.toggle($event, 'terminal')}
              interface="popover"
              mode="md"
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
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderTheme() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Theme</ion-label>

        <ion-list class="theme">
          <ion-item class="select">
            <ion-label>Theme</ion-label>

            <ion-select
              value={this.theme}
              placeholder="Select a theme"
              disabled={this.terminal !== DeckdeckgoHighlightCodeTerminal.CARBON}
              onIonChange={($event: CustomEvent) => this.toggle($event, 'theme')}
              interface="popover"
              mode="md"
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
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderHighlightLinesColor() {
    return (
      <app-expansion-panel expanded={'close'}>
        <ion-label slot="title">Highlight lines</ion-label>
        <button slot="info" class="info" onClick={($event: UIEvent) => this.presentHighlightInfo($event)}>
          <ion-icon name="help"></ion-icon>
        </button>
        <ion-list>
          <ion-item class="with-padding">
            <ion-input
              value={this.highlightLines}
              placeholder="List your lines here"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={() => this.highlightSelectedLines()}></ion-input>
          </ion-item>

          <ion-item-divider class="ion-padding-top">
            <ion-label>
              Opacity <small>{this.highlightColorOpacity}%</small>
            </ion-label>
          </ion-item-divider>

          <ion-item class="item-opacity">
            <ion-range
              color="primary"
              min={0}
              max={100}
              disabled={!this.highlightColor || this.highlightColor === undefined || !this.highlightLines || this.highlightLines === undefined}
              value={this.highlightColorOpacity}
              mode="md"
              onIonChange={($event: CustomEvent<RangeChangeEventDetail>) => this.updateOpacity($event, this.setHighlightOpacity)}></ion-range>
          </ion-item>

          <div class={!this.highlightLines || this.highlightLines === undefined ? 'ion-padding-start disabled' : 'ion-padding-start'}>
            <deckgo-color
              palette={paletteStore.state.palette}
              class="ion-padding-bottom"
              onColorChange={($event: CustomEvent) => this.selectColor($event, this.setHighlightColor)}
              color-rgb={this.highlightColor}
              more={this.moreColors}>
              <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg" slot="more" aria-label="More" class="more"></ion-icon>
            </deckgo-color>
          </div>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
