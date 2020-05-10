import {Component, Element, EventEmitter, Prop, State, h} from '@stencil/core';

enum MathFontSize {
  VERY_SMALL,
  SMALL,
  NORMAL,
  BIG,
  VERY_BIG
}

@Component({
  tag: 'app-math',
  styleUrl: 'app-math.scss'
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
  private leqno: boolean = false;

  @State()
  private fleqn: boolean = false;

  constructor() {}

  async componentWillLoad() {
    await this.initCurrent();
  }

  private async closePopover() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private initCurrent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.currentFontSize = await this.initFontSize();
      this.leqno = this.selectedElement && this.selectedElement.hasAttribute('leqno');
      this.fleqn = this.selectedElement && this.selectedElement.hasAttribute('fleqn');
      resolve();
    });
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

  private toggleOptions($event: CustomEvent, propName: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute(propName, $event.detail.checked);

      this.emitMathDidChange();

      resolve();
    });
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Math Options</h2>
        <ion-router-link slot="end" onClick={() => this.closePopover()}>
          <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <ion-list>
        <ion-item>
          <ion-label>Leqno</ion-label>
          <ion-checkbox slot="end" checked={this.leqno} onIonChange={($event: CustomEvent) => this.toggleOptions($event, 'leqno')}></ion-checkbox>
        </ion-item>

        <ion-item>
          <ion-label>Fleqn</ion-label>
          <ion-checkbox slot="end" checked={this.fleqn} onIonChange={($event: CustomEvent) => this.toggleOptions($event, 'fleqn')}></ion-checkbox>
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
            <ion-select-option value={MathFontSize.VERY_SMALL}>Very small</ion-select-option>
            <ion-select-option value={MathFontSize.SMALL}>Small</ion-select-option>
            <ion-select-option value={MathFontSize.NORMAL}>Normal</ion-select-option>
            <ion-select-option value={MathFontSize.BIG}>Big</ion-select-option>
            <ion-select-option value={MathFontSize.VERY_BIG}>Very big</ion-select-option>
          </ion-select>
        </ion-item>
      </ion-list>
    ];
  }
}
