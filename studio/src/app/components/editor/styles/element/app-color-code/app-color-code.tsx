import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {alertController} from '@ionic/core';

import {DeckdeckgoHighlightCodeCarbonTheme, DeckdeckgoHighlightCodeTerminal} from '@deckdeckgo/highlight-code';

import {ColorUtils, InitStyleColor} from '../../../../../utils/editor/color.utils';

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

  @State()
  private codeColorType: CodeColorType = undefined;

  @State()
  private highlightLines: string;

  @State()
  private terminal: DeckdeckgoHighlightCodeTerminal = DeckdeckgoHighlightCodeTerminal.CARBON;

  @State()
  private theme: DeckdeckgoHighlightCodeCarbonTheme = DeckdeckgoHighlightCodeCarbonTheme.DRACULA;

  @State()
  private toolbar: boolean = true;

  @Event() codeDidChange: EventEmitter<void>;

  private colorCodeRef!: HTMLAppColorElement;

  async componentWillLoad() {
    await this.initTerminal();
  }

  private initCodeColor = async (): Promise<InitStyleColor> => {
    if (!this.selectedElement) {
      return {
        rgb: null,
        opacity: null,
      };
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

    return ColorUtils.splitColor(color);
  };

  private initHighlightColor = async (): Promise<InitStyleColor> => {
    this.highlightLines = this.selectedElement?.getAttribute('highlight-lines') ?? null;
    const color: string = this.selectedElement?.style?.getPropertyValue('--deckgo-highlight-code-line-background') ?? '62,69,100';

    return ColorUtils.splitColor(color);
  };

  private async resetHighlightColor() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.removeProperty('--deckgo-highlight-code-line-background');

    this.emitCodeChange();
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

    this.toolbar = this.selectedElement?.style.getPropertyValue('--deckgo-highlight-code-carbon-toolbar-display') !== 'none';
  }

  private async applyCodeColor($event: CustomEvent<string>) {
    if (!this.selectedElement || !$event) {
      return;
    }

    this.selectedElement.style.setProperty(this.getStyle(), $event.detail);

    this.emitCodeChange();
  }

  private async applyHighlightColor($event: CustomEvent<string>) {
    if (!this.selectedElement || !$event) {
      return;
    }

    this.selectedElement.style.setProperty('--deckgo-highlight-code-line-background', $event.detail);

    this.emitCodeChange();
  }

  private async toggleColorType($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.codeColorType = $event.detail.value;
    await this.colorCodeRef?.loadColor();
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

      this.emitCodeChange();

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

  private emitCodeChange() {
    this.codeDidChange.emit();
  }

  private async resetCodeColor() {
    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.removeProperty(this.getStyle());

    this.emitCodeChange();
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

  private async toggleToolbar() {
    if (!this.selectedElement) {
      return;
    }

    this.toolbar = !this.toolbar;

    if (this.toolbar) {
      this.selectedElement.style.removeProperty('--deckgo-highlight-code-carbon-toolbar-display');
    } else {
      this.selectedElement.style.setProperty('--deckgo-highlight-code-carbon-toolbar-display', 'none');
    }

    this.emitCodeChange();
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
        </ion-list>

        <app-color
          class="ion-margin-top"
          ref={(el) => (this.colorCodeRef = el as HTMLAppColorElement)}
          initColor={this.initCodeColor}
          onResetColor={() => this.resetCodeColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyCodeColor($event)}></app-color>
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

          <ion-item>
            <ion-label>Display toolbar</ion-label>
            <ion-checkbox
              disabled={this.terminal !== DeckdeckgoHighlightCodeTerminal.CARBON}
              slot="end"
              color="dark"
              checked={this.toolbar}
              onIonChange={() => this.toggleToolbar()}></ion-checkbox>
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
        </ion-list>

        <app-color
          class="ion-margin-top"
          initColor={this.initHighlightColor}
          onResetColor={() => this.resetHighlightColor()}
          onColorDidChange={($event: CustomEvent<string>) => this.applyHighlightColor($event)}></app-color>
      </app-expansion-panel>
    );
  }
}
