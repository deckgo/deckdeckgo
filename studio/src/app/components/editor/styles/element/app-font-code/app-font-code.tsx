import {Component, Event, EventEmitter, h, Prop, State} from '@stencil/core';

enum CodeFontSize {
  VERY_SMALL,
  SMALL,
  NORMAL,
  BIG,
  VERY_BIG,
}

@Component({
  tag: 'app-font-code',
})
export class AppColorCode {
  @Prop()
  selectedElement: HTMLElement;

  @State()
  private currentFontSize: CodeFontSize = undefined;

  @Event() codeDidChange: EventEmitter<void>;

  async componentWillLoad() {
    this.currentFontSize = await this.initFontSize();
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

      this.codeDidChange.emit();

      resolve();
    });
  }

  render() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Size</ion-label>

        <ion-list>
          <ion-item class="select">
            <ion-label>Size</ion-label>

            <ion-select
              value={this.currentFontSize}
              placeholder="Select a font size"
              onIonChange={($event: CustomEvent) => this.toggleFontSize($event)}
              interface="popover"
              mode="md"
              class="ion-padding-start ion-padding-end">
              <ion-select-option value={CodeFontSize.VERY_SMALL}>Very small</ion-select-option>
              <ion-select-option value={CodeFontSize.SMALL}>Small</ion-select-option>
              <ion-select-option value={CodeFontSize.NORMAL}>Normal</ion-select-option>
              <ion-select-option value={CodeFontSize.BIG}>Big</ion-select-option>
              <ion-select-option value={CodeFontSize.VERY_BIG}>Very big</ion-select-option>
            </ion-select>
          </ion-item>
        </ion-list>
      </app-expansion-panel>
    );
  }
}
