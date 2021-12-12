import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import i18n from '../../../../../../stores/i18n.store';

import {DeckdeckgoHighlightCodeCarbonTheme, DeckdeckgoHighlightCodeTerminal} from '@deckdeckgo/highlight-code';

import {ColorUtils, InitStyleColor} from '../../../../../../utils/editor/color.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.deck.utils';

enum CodeColorType {
  COMMENTS,
  PUNCTUATION,
  PROPERTY,
  SELECTOR,
  OPERATOR,
  KEYWORD,
  FUNCTION,
  REGEX,
  LINE_NUMBERS
}

@Component({
  tag: 'app-color-code',
  styleUrl: 'app-color-code.scss'
})
export class AppColorCode {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private codeColorType: CodeColorType = undefined;

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
        opacity: null
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

    this.updateStyle($event.detail);
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

  private emitCodeChange() {
    this.codeDidChange.emit();
  }

  private async resetCodeColor() {
    if (!this.selectedElement) {
      return;
    }

    this.updateStyle(null);
  }

  private updateStyle(value: string | null) {
    const redoType: CodeColorType = this.codeColorType;

    setStyle(this.selectedElement, {
      properties: [{property: this.getStyle(), value}],
      type: 'element',
      updateUI: async (_value: string) => {
        await this.colorCodeRef.loadColor();

        this.codeColorType = redoType;
      }
    });

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
    return [this.renderTerminal(), this.renderTheme(), this.renderCategoryColor()];
  }

  private renderCategoryColor() {
    return (
      <app-expansion-panel expanded={'close'}>
        <ion-label slot="title">{i18n.state.editor.more_colors}</ion-label>
        <ion-list>
          <ion-item class="select">
            <ion-label>{i18n.state.editor.apply_a_color_to}</ion-label>

            <ion-select
              value={this.codeColorType}
              placeholder={i18n.state.editor.select_category}
              onIonChange={(e: CustomEvent) => this.toggleColorType(e)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={CodeColorType.COMMENTS}>{i18n.state.editor.comments}</ion-select-option>
              <ion-select-option value={CodeColorType.FUNCTION}>{i18n.state.editor.functions}</ion-select-option>
              <ion-select-option value={CodeColorType.KEYWORD}>{i18n.state.editor.keywords}</ion-select-option>
              <ion-select-option value={CodeColorType.OPERATOR}>{i18n.state.editor.operators}</ion-select-option>
              <ion-select-option value={CodeColorType.PUNCTUATION}>{i18n.state.editor.punctuation}</ion-select-option>
              <ion-select-option value={CodeColorType.PROPERTY}>{i18n.state.editor.properties}</ion-select-option>
              <ion-select-option value={CodeColorType.REGEX}>{i18n.state.editor.regex}</ion-select-option>
              <ion-select-option value={CodeColorType.SELECTOR}>{i18n.state.editor.selector}</ion-select-option>
              <ion-select-option value={CodeColorType.LINE_NUMBERS}>{i18n.state.editor.line_numbers}</ion-select-option>
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
        <ion-label slot="title">{i18n.state.editor.terminal}</ion-label>

        <ion-list class="terminal">
          <ion-item class="select">
            <ion-label>{i18n.state.editor.terminal}</ion-label>

            <ion-select
              value={this.terminal}
              placeholder={i18n.state.editor.select_terminal}
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
        <ion-label slot="title">{i18n.state.editor.theme}</ion-label>

        <ion-list class="theme">
          <ion-item class="select">
            <ion-label>{i18n.state.editor.theme}</ion-label>

            <ion-select
              value={this.theme}
              placeholder={i18n.state.editor.select_theme}
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
            <ion-label>{i18n.state.editor.display_toolbar}</ion-label>
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
}
