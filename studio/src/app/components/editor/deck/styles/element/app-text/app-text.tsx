import {Component, Event, EventEmitter, Fragment, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../../../stores/settings.store';
import i18n from '../../../../../../stores/i18n.store';

import {SettingsUtils} from '../../../../../../utils/core/settings.utils';

import {EditMode, Expanded} from '../../../../../../types/core/settings';
import {FontSize} from '../../../../../../types/editor/font-size';
import {SelectedElement} from '../../../../../../types/editor/selected-element';

import {AlignUtils, TextAlign} from '../../../../../../utils/editor/align.utils';
import {initFontSize, toggleFontSize} from '../../../../../../utils/editor/font-size.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.utils';

enum LetterSpacing {
  TIGHTER,
  TIGHT,
  NORMAL,
  WIDE,
  WIDER,
  SUPERWIDE,
  WIDEST
}

@Component({
  tag: 'app-text'
})
export class AppText {
  @Prop()
  selectedElement: SelectedElement;

  @State()
  private align: TextAlign | undefined;

  @State()
  private alignCSS: string;

  @State()
  private letterSpacing: LetterSpacing = LetterSpacing.NORMAL;

  @State()
  private letterSpacingCSS: string;

  @State()
  private fontSize: FontSize | undefined = undefined;

  @State()
  private fontSizeCSS: string;

  @Event() textDidChange: EventEmitter<void>;

  private destroyListener;

  // When we update states on undo / redo it triggers a rerender which triggers the onChange events of Ionic components
  private ignoreUpdateStyle: boolean = false;

  async componentWillLoad() {
    await this.init();

    await this.initCSS();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initCSS();
        return;
      }

      await this.init();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async init() {
    this.letterSpacing = await this.initLetterSpacing();
    this.align = await AlignUtils.getAlignment(this.selectedElement?.element);
    this.fontSize = await initFontSize(this.selectedElement?.element);
  }

  private async initLetterSpacing(): Promise<LetterSpacing> {
    if (!this.selectedElement || !this.selectedElement.element) {
      return LetterSpacing.NORMAL;
    }

    const spacing: string = this.selectedElement.element.style.letterSpacing;

    if (!spacing || spacing === '') {
      return LetterSpacing.NORMAL;
    }

    if (spacing === '-0.1em') {
      return LetterSpacing.TIGHTER;
    } else if (spacing === '-0.05em') {
      return LetterSpacing.TIGHT;
    } else if (spacing === '0.1em') {
      return LetterSpacing.WIDE;
    } else if (spacing === '0.2em') {
      return LetterSpacing.WIDER;
    } else if (spacing === '0.3em') {
      return LetterSpacing.SUPERWIDE;
    } else if (spacing === '0.4em') {
      return LetterSpacing.WIDEST;
    }

    return LetterSpacing.NORMAL;
  }

  private async updateLetterSpacing($event: CustomEvent): Promise<void> {
    if (!this.selectedElement || !this.selectedElement.element || !$event || !$event.detail) {
      return;
    }

    let letterSpacingConverted = '';
    switch ($event.detail.value) {
      case LetterSpacing.TIGHTER:
        letterSpacingConverted = '-0.1em';
        break;
      case LetterSpacing.TIGHT:
        letterSpacingConverted = '-0.05em';
        break;
      case LetterSpacing.WIDE:
        letterSpacingConverted = '0.1em';
        break;
      case LetterSpacing.WIDER:
        letterSpacingConverted = '0.2em';
        break;
      case LetterSpacing.SUPERWIDE:
        letterSpacingConverted = '0.3em';
        break;
      case LetterSpacing.WIDEST:
        letterSpacingConverted = '0.4em';
        break;
      default:
        letterSpacingConverted = 'normal';
    }

    this.letterSpacing = $event.detail.value;

    this.updateLetterSpacingCSS(letterSpacingConverted);
  }

  private async initCSS() {
    this.letterSpacingCSS = this.selectedElement?.element?.style.letterSpacing;
    this.alignCSS = this.selectedElement?.element?.style.textAlign;
    this.fontSizeCSS = this.selectedElement?.element?.style.fontSize;
  }

  private handleLetterSpacingInput($event: CustomEvent<KeyboardEvent>) {
    this.letterSpacingCSS = ($event.target as InputTargetEvent).value;
  }

  private updateLetterSpacingCSS(letterSpacingCSS: string) {
    this.updateStyle({property: 'letter-spacing', value: letterSpacingCSS});

    this.textDidChange.emit();
  }

  private updateStyle({property, value}: {property: 'letter-spacing' | 'text-align' | 'font-size'; value: string}) {
    if (this.ignoreUpdateStyle) {
      this.ignoreUpdateStyle = false;
      return;
    }

    setStyle(this.selectedElement.element, {
      properties: [{property, value}],
      type: this.selectedElement.type,
      updateUI: async () => {
        // ion-change triggers the event each time its value changes, because we re-render, it triggers it again
        this.ignoreUpdateStyle = true;

        if (settingsStore.state.editMode === 'css') {
          switch (property) {
            case 'letter-spacing':
              this.letterSpacingCSS = this.selectedElement?.element?.style.letterSpacing;
              break;
            case 'font-size':
              this.fontSizeCSS = this.selectedElement?.element?.style.fontSize;
              break;
            case 'text-align':
              this.alignCSS = this.selectedElement?.element?.style.textAlign;
          }

          return;
        }

        switch (property) {
          case 'letter-spacing':
            this.letterSpacing = await this.initLetterSpacing();
            break;
          case 'font-size':
            this.fontSize = await initFontSize(this.selectedElement?.element);
            break;
          case 'text-align':
            this.align = await AlignUtils.getAlignment(this.selectedElement?.element);
        }
      }
    });
  }

  private async updateAlign($event: CustomEvent): Promise<void> {
    if (!this.selectedElement || !this.selectedElement.element || !$event || !$event.detail) {
      return;
    }

    this.align = $event.detail.value;

    this.updateAlignCSS($event.detail.value);
  }

  private handleAlignInput($event: CustomEvent<KeyboardEvent>) {
    this.alignCSS = ($event.target as InputTargetEvent).value as TextAlign;
  }

  private updateAlignCSS(alignCSS: string) {
    if (!this.selectedElement || !this.selectedElement.element) {
      return;
    }

    this.updateStyle({property: 'text-align', value: alignCSS});

    this.textDidChange.emit();
  }

  private async toggleFontSize($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.fontSize = $event.detail.value;

    if (!this.selectedElement || !this.selectedElement.element) {
      return;
    }

    const size: string | undefined = toggleFontSize(this.selectedElement.element, this.fontSize);

    if (!size) {
      return;
    }

    this.updateFontSizeCSS(size);
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.fontSizeCSS = ($event.target as InputTargetEvent).value;
  }

  private updateFontSizeCSS(size: string) {
    this.updateStyle({property: 'font-size', value: size});

    this.textDidChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.text}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({text: $event.detail})}>
        <ion-label slot="title">{i18n.state.editor.text}</ion-label>
        <ion-list>
          {this.renderFontSize()}
          {this.renderLetterSpacing()}
          {this.renderAlign()}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderFontSize() {
    return (
      <Fragment>
        <ion-item-divider>
          <ion-label>{i18n.state.editor.scale}</ion-label>
        </ion-item-divider>

        <ion-item class="select properties">
          <ion-label>{i18n.state.editor.size}</ion-label>

          <ion-select
            value={this.fontSize}
            placeholder={i18n.state.editor.select_font_size}
            onIonChange={($event: CustomEvent) => this.toggleFontSize($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={FontSize.VERY_SMALL}>{i18n.state.editor.very_small}</ion-select-option>
            <ion-select-option value={FontSize.SMALL}>{i18n.state.editor.small}</ion-select-option>
            <ion-select-option value={FontSize.NORMAL}>{i18n.state.editor.normal}</ion-select-option>
            <ion-select-option value={FontSize.BIG}>{i18n.state.editor.big}</ion-select-option>
            <ion-select-option value={FontSize.VERY_BIG}>{i18n.state.editor.very_big}</ion-select-option>
            {this.fontSize === FontSize.CUSTOM ? (
              <ion-select-option value={FontSize.CUSTOM}>{i18n.state.editor.custom}</ion-select-option>
            ) : undefined}
          </ion-select>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.fontSizeCSS}
            placeholder="font-size"
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
            onIonChange={() => this.updateFontSizeCSS(this.fontSizeCSS)}></ion-input>
        </ion-item>
      </Fragment>
    );
  }

  private renderLetterSpacing() {
    return (
      <Fragment>
        <ion-item-divider>
          <ion-label>{i18n.state.editor.letter_spacing}</ion-label>
        </ion-item-divider>

        <ion-item class="select properties">
          <ion-label>{i18n.state.editor.letter_spacing}</ion-label>
          <ion-select
            value={this.letterSpacing}
            placeholder={i18n.state.editor.letter_spacing}
            onIonChange={($event: CustomEvent) => this.updateLetterSpacing($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={LetterSpacing.TIGHTER}>{i18n.state.editor.tighter}</ion-select-option>
            <ion-select-option value={LetterSpacing.TIGHT}>{i18n.state.editor.tight}</ion-select-option>
            <ion-select-option value={LetterSpacing.NORMAL}>{i18n.state.editor.normal}</ion-select-option>
            <ion-select-option value={LetterSpacing.WIDE}>{i18n.state.editor.wide}</ion-select-option>
            <ion-select-option value={LetterSpacing.WIDER}>{i18n.state.editor.wider}</ion-select-option>
            <ion-select-option value={LetterSpacing.SUPERWIDE}>{i18n.state.editor.superwide}</ion-select-option>
            <ion-select-option value={LetterSpacing.WIDEST}>{i18n.state.editor.widest}</ion-select-option>
          </ion-select>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.letterSpacingCSS}
            placeholder={i18n.state.editor.letter_spacing}
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleLetterSpacingInput(e)}
            onIonChange={() => this.updateLetterSpacingCSS(this.letterSpacingCSS)}></ion-input>
        </ion-item>
      </Fragment>
    );
  }

  private renderAlign() {
    if (this.align === undefined) {
      return undefined;
    }

    return (
      <Fragment>
        <ion-item-divider>
          <ion-label>{i18n.state.editor.alignment}</ion-label>
        </ion-item-divider>

        <ion-item class="select properties">
          <ion-label>{i18n.state.editor.alignment}</ion-label>

          <ion-select
            value={this.align}
            placeholder={i18n.state.editor.alignment}
            onIonChange={($event: CustomEvent) => this.updateAlign($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={TextAlign.LEFT}>{i18n.state.editor.left}</ion-select-option>
            <ion-select-option value={TextAlign.CENTER}>{i18n.state.editor.center}</ion-select-option>
            <ion-select-option value={TextAlign.RIGHT}>{i18n.state.editor.right}</ion-select-option>
          </ion-select>
        </ion-item>

        <ion-item class="with-padding css">
          <ion-input
            value={this.alignCSS}
            placeholder={i18n.state.editor.text_align}
            debounce={500}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleAlignInput(e)}
            onIonChange={() => this.updateAlignCSS(this.alignCSS)}></ion-input>
        </ion-item>
      </Fragment>
    );
  }
}
