import {Component, Element, EventEmitter, Prop, State, h} from '@stencil/core';

enum MathFontSize {
  VERY_SMALL,
  SMALL,
  NORMAL,
  BIG,
  VERY_BIG,
}

@Component({
  tag: 'app-math',
  styleUrl: 'app-math.scss',
})
export class AppMath {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  mathDidChange: EventEmitter<HTMLElement>;

  @State()
  private currentFontSize: MathFontSize = undefined;

  @State()
  private macros: string | undefined;

  constructor() {}

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async initCurrent(): Promise<void> {
    this.currentFontSize = await this.initFontSize();
    this.macros = this.selectedElement ? this.selectedElement.getAttribute('macros') : undefined;
  }

  private emitMathDidChange() {
    this.mathDidChange.emit(this.selectedElement);
  }

  private initFontSize(): Promise<MathFontSize> {
    return new Promise<MathFontSize>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        resolve(null);
        return;
      }

      const property: string = this.selectedElement.style.getPropertyValue('--deckgo-math-font-size');

      if (property === '50%') {
        resolve(MathFontSize.VERY_SMALL);
      } else if (property === '75%') {
        resolve(MathFontSize.SMALL);
      } else if (property === '150%') {
        resolve(MathFontSize.BIG);
      } else if (property === '200%') {
        resolve(MathFontSize.VERY_BIG);
      } else {
        resolve(MathFontSize.NORMAL);
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

      this.selectedElement.style.removeProperty('--deckgo-math-font-size');

      if (this.currentFontSize === MathFontSize.VERY_SMALL) {
        this.selectedElement.style.setProperty('--deckgo-math-font-size', '50%');
      } else if (this.currentFontSize === MathFontSize.SMALL) {
        this.selectedElement.style.setProperty('--deckgo-math-font-size', '75%');
      } else if (this.currentFontSize === MathFontSize.BIG) {
        this.selectedElement.style.setProperty('--deckgo-math-font-size', '150%');
      } else if (this.currentFontSize === MathFontSize.VERY_BIG) {
        this.selectedElement.style.setProperty('--deckgo-math-font-size', '200%');
      }

      this.emitMathDidChange();

      resolve();
    });
  }

  private handleMacrosInput($event: CustomEvent<KeyboardEvent>) {
    this.macros = ($event.target as InputTargetEvent).value;
  }

  private async applyMacrosInput(): Promise<void> {
    if (!this.selectedElement) {
      return;
    }

    if (this.macros && this.macros !== '') {
      this.selectedElement.setAttribute('macros', this.macros);
    } else {
      this.selectedElement.removeAttribute('macros');
    }

    this.emitMathDidChange();
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Math attributes</h2>
        <ion-router-link slot="end" onClick={() => this.closePopover()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <ion-list>
        <ion-item-divider class="ion-padding-top">
          <ion-label>Font size</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-label>Size</ion-label>

          <ion-select
            value={this.currentFontSize}
            placeholder="Select a font size"
            onIonChange={($event: CustomEvent) => this.toggleFontSize($event)}
            interface="popover"
            mode="md"
            class="ion-padding-start ion-padding-end">
            <ion-select-option value={MathFontSize.VERY_SMALL}>Very small</ion-select-option>
            <ion-select-option value={MathFontSize.SMALL}>Small</ion-select-option>
            <ion-select-option value={MathFontSize.NORMAL}>Normal</ion-select-option>
            <ion-select-option value={MathFontSize.BIG}>Big</ion-select-option>
            <ion-select-option value={MathFontSize.VERY_BIG}>Very big</ion-select-option>
          </ion-select>
        </ion-item>

        <ion-item-divider>
          <ion-label>Macros</ion-label>
        </ion-item-divider>

        <ion-item class="select">
          <ion-textarea
            rows={5}
            value={this.macros}
            debounce={500}
            maxlength={254}
            placeholder="A collection of custom macros. Property with a name like \\name which maps to a string that describes the expansion."
            onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleMacrosInput($event)}
            onIonChange={() => this.applyMacrosInput()}></ion-textarea>
        </ion-item>
      </ion-list>,
    ];
  }
}
