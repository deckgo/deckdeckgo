import {Component, Element, Prop, State, h, EventEmitter, Event} from '@stencil/core';

import {isSlide} from '@deckdeckgo/deck-utils';

import settingsStore from '../../../../../stores/settings.store';

import {EditMode, Expanded} from '../../../../../types/core/settings';

import {SettingsUtils} from '../../../../../utils/core/settings.utils';

enum ImageSize {
  SMALL = '25%',
  MEDIUM = '50%',
  LARGE = '75%',
  ORIGINAL = '100%',
}

enum ImageAlignment {
  START = 'flex-start',
  CENTER = 'center',
  END = 'flex-end',
}

@Component({
  tag: 'app-image-style',
})
export class AppImageStyle {
  @Element() el: HTMLElement;

  @Event() private imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  selectedElement: HTMLElement;

  @State()
  private currentImageSize: ImageSize;

  @State()
  private currentImageAlignment: ImageAlignment;

  @State()
  private imageHeightCSS: string;

  @State()
  private imageJustifyContentCSS: string;

  private destroyListener;

  async componentWillLoad() {
    this.currentImageSize = await this.initImageSize();
    this.currentImageAlignment = await this.initImageAlignment();

    this.destroyListener = settingsStore.onChange('editMode', async (edit: EditMode) => {
      if (edit === 'css') {
        await this.initCSS();
        return;
      }

      this.currentImageSize = await this.initImageSize();
      this.currentImageAlignment = await this.initImageAlignment();
    });
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }
  }

  private async initCSS() {
    this.imageHeightCSS = this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height');
    this.imageJustifyContentCSS = this.selectedElement.style.getPropertyValue('justify-content');
  }

  private initImageSize(): Promise<ImageSize> {
    return new Promise<ImageSize>((resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        resolve(null);
        return;
      }

      if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '25%') {
        resolve(ImageSize.SMALL);
      } else if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '50%') {
        resolve(ImageSize.MEDIUM);
      } else if (this.selectedElement.style.getPropertyValue('--deckgo-lazy-img-height') === '75%') {
        resolve(ImageSize.LARGE);
      } else {
        resolve(ImageSize.ORIGINAL);
      }
    });
  }

  private initImageAlignment(): Promise<ImageAlignment> {
    return new Promise<ImageAlignment>(async (resolve) => {
      if (!this.selectedElement || !this.selectedElement.style) {
        resolve(null);
        return;
      }

      if (this.selectedElement.style.getPropertyValue('justify-content') === 'center') {
        resolve(ImageAlignment.CENTER);
      } else if (this.selectedElement.style.getPropertyValue('justify-content') === 'flex-end') {
        resolve(ImageAlignment.END);
      } else if (this.selectedElement.style.getPropertyValue('justify-content') === 'flex-start') {
        resolve(ImageAlignment.START);
      } else {
        const result: ImageAlignment = await this.findSlideDefaultAlignment();
        resolve(result);
      }
    });
  }

  private findSlideDefaultAlignment(): Promise<ImageAlignment> {
    return new Promise<ImageAlignment>((resolve) => {
      const parent: HTMLElement = this.selectedElement.parentElement;

      if (isSlide(parent)) {
        const container: HTMLElement = parent.shadowRoot.querySelector('.deckgo-slide');
        if (container) {
          const style: CSSStyleDeclaration = window.getComputedStyle(container);

          if (style && style.alignItems === 'center') {
            resolve(ImageAlignment.CENTER);
            return;
          }
        }
      }

      resolve(ImageAlignment.START);
    });
  }

  private toggleImageSize($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.currentImageSize = $event.detail.value;

      if (!this.selectedElement) {
        resolve();
        return;
      }

      if (this.currentImageSize === ImageSize.ORIGINAL) {
        this.selectedElement.style.removeProperty('--deckgo-lazy-img-height');
      } else {
        this.selectedElement.style.setProperty('--deckgo-lazy-img-height', this.currentImageSize);
      }

      this.imgDidChange.emit(this.selectedElement);

      resolve();
    });
  }

  private toggleImageAlignment($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.currentImageAlignment = $event.detail.value;

      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.style.setProperty('display', 'inline-flex');
      this.selectedElement.style.setProperty('justify-content', this.currentImageAlignment);

      this.imgDidChange.emit(this.selectedElement);

      resolve();
    });
  }

  private handleImageHeightInput($event: CustomEvent<KeyboardEvent>) {
    this.imageHeightCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateImageHeightCSS() {
    if (!this.imageHeightCSS || this.imageHeightCSS === '') {
      this.selectedElement.style.removeProperty('--deckgo-lazy-img-height');
    } else {
      this.selectedElement.style.setProperty('--deckgo-lazy-img-height', this.imageHeightCSS);
    }

    this.imgDidChange.emit(this.selectedElement);
  }

  private handleImageJustifyContentInput($event: CustomEvent<KeyboardEvent>) {
    this.imageJustifyContentCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateImageJustifyContentCSS() {
    this.selectedElement.style.setProperty('display', 'inline-flex');
    this.selectedElement.style.setProperty('justify-content', this.imageJustifyContentCSS);

    this.imgDidChange.emit(this.selectedElement);
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.imageStyle}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({imageStyle: $event.detail})}>
        <ion-label slot="title">Image</ion-label>
        <ion-list>
          {this.renderImageSize()}
          {this.renderImageAlignment()}
        </ion-list>
      </app-expansion-panel>
    );
  }

  private renderImageSize() {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>Size</ion-label>
      </ion-item-divider>,

      <ion-item class="select properties">
        <ion-label>Size</ion-label>

        <ion-select
          value={this.currentImageSize}
          placeholder="Select an image size"
          onIonChange={(e: CustomEvent) => this.toggleImageSize(e)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end">
          <ion-select-option value={ImageSize.SMALL}>Small</ion-select-option>
          <ion-select-option value={ImageSize.MEDIUM}>Medium</ion-select-option>
          <ion-select-option value={ImageSize.LARGE}>Large</ion-select-option>
          <ion-select-option value={ImageSize.ORIGINAL}>Original</ion-select-option>
        </ion-select>
      </ion-item>,

      <ion-item class="with-padding css">
        <ion-input
          value={this.imageHeightCSS}
          placeholder="height"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleImageHeightInput(e)}
          onIonChange={async () => await this.updateImageHeightCSS()}></ion-input>
      </ion-item>,
    ];
  }

  private renderImageAlignment() {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>Alignment</ion-label>
      </ion-item-divider>,

      <ion-item class="select properties">
        <ion-label>Alignment</ion-label>

        <ion-select
          value={this.currentImageAlignment}
          placeholder="Align the image"
          onIonChange={(e: CustomEvent) => this.toggleImageAlignment(e)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end">
          <ion-select-option value={ImageAlignment.START}>Start</ion-select-option>
          <ion-select-option value={ImageAlignment.CENTER}>Center</ion-select-option>
          <ion-select-option value={ImageAlignment.END}>End</ion-select-option>
        </ion-select>
      </ion-item>,

      <ion-item class="with-padding css">
        <ion-input
          value={this.imageJustifyContentCSS}
          placeholder="justify-content"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleImageJustifyContentInput(e)}
          onIonChange={async () => await this.updateImageJustifyContentCSS()}></ion-input>
      </ion-item>,
    ];
  }
}
