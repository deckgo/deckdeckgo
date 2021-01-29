import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import settingsStore from '../../../../../stores/settings.store';

import {SettingsUtils} from '../../../../../utils/core/settings.utils';

import {EditMode, Expanded} from '../../../../../types/core/settings';

enum FontSize {
  VERY_SMALL,
  SMALL,
  NORMAL,
  BIG,
  VERY_BIG,
}

@Component({
  tag: 'app-font-size',
  styleUrl: 'app-font-size.scss',
})
export class AppFontSize {
  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  selector: '--deckgo-highlight-code-font-size' | '--deckgo-math-font-size' = '--deckgo-highlight-code-font-size';

  @State()
  private currentFontSize: FontSize = undefined;

  @State()
  private fontSizeCSS: string;

  @Event() codeDidChange: EventEmitter<void>;

  private destroyListener;

  async componentWillLoad() {
    this.currentFontSize = await this.initFontSize();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initFontSizeCSS();
        return;
      }

      this.currentFontSize = await this.initFontSize();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initFontSizeCSS() {
    this.fontSizeCSS = this.selectedElement.style.getPropertyValue(this.selector);
  }

  private initFontSize(): Promise<FontSize> {
    return new Promise<FontSize>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        resolve(null);
        return;
      }

      const property: string = this.selectedElement.style.getPropertyValue(this.selector);

      if (property === '50%') {
        resolve(FontSize.VERY_SMALL);
      } else if (property === '75%') {
        resolve(FontSize.SMALL);
      } else if (property === '150%') {
        resolve(FontSize.BIG);
      } else if (property === '200%') {
        resolve(FontSize.VERY_BIG);
      } else {
        resolve(FontSize.NORMAL);
      }
    });
  }

  private async toggleFontSize($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.currentFontSize = $event.detail.value;

    if (!this.selectedElement) {
      return;
    }

    this.selectedElement.style.removeProperty(this.selector);

    if (this.currentFontSize === FontSize.VERY_SMALL) {
      this.selectedElement.style.setProperty(this.selector, '50%');
    } else if (this.currentFontSize === FontSize.SMALL) {
      this.selectedElement.style.setProperty(this.selector, '75%');
    } else if (this.currentFontSize === FontSize.BIG) {
      this.selectedElement.style.setProperty(this.selector, '150%');
    } else if (this.currentFontSize === FontSize.VERY_BIG) {
      this.selectedElement.style.setProperty(this.selector, '200%');
    }

    this.codeDidChange.emit();
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.fontSizeCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateFontSizeCSS() {
    this.selectedElement.style.setProperty(this.selector, this.fontSizeCSS);

    this.codeDidChange.emit();
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.fontSize}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({fontSize: $event.detail})}>
        <ion-label slot="title">Size</ion-label>

        <ion-list>
          <ion-item class="select properties">
            <ion-label>Size</ion-label>

            <ion-select
              value={this.currentFontSize}
              placeholder="Select a font size"
              onIonChange={($event: CustomEvent) => this.toggleFontSize($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={FontSize.VERY_SMALL}>Very small</ion-select-option>
              <ion-select-option value={FontSize.SMALL}>Small</ion-select-option>
              <ion-select-option value={FontSize.NORMAL}>Normal</ion-select-option>
              <ion-select-option value={FontSize.BIG}>Big</ion-select-option>
              <ion-select-option value={FontSize.VERY_BIG}>Very big</ion-select-option>
            </ion-select>
          </ion-item>

          <ion-item class="with-padding css">
            <ion-input
              value={this.fontSizeCSS}
              placeholder="font-size"
              debounce={500}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={async () => await this.updateFontSizeCSS()}></ion-input>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
