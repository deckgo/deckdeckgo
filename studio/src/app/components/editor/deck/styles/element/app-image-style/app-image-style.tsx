import {Component, Element, Prop, State, h, EventEmitter, Event} from '@stencil/core';

import {isSlide} from '@deckdeckgo/deck-utils';

import settingsStore from '../../../../../../stores/settings.store';
import i18n from '../../../../../../stores/i18n.store';

import {EditMode, Expanded} from '../../../../../../types/core/settings';

import {SettingsUtils} from '../../../../../../utils/core/settings.utils';
import {setStyle} from '../../../../../../utils/editor/undo-redo.deck.utils';

enum ImageSize {
  SMALL = '25%',
  MEDIUM = '50%',
  LARGE = '75%',
  ORIGINAL = '100%'
}

enum ImageAlignment {
  START = 'flex-start',
  CENTER = 'center',
  END = 'flex-end'
}

@Component({
  tag: 'app-image-style'
})
export class AppImageStyle {
  @Element() el: HTMLElement;

  @Event() private imgDidChange: EventEmitter<HTMLElement>;

  @Prop()
  selectedTarget: HTMLElement;

  @State()
  private currentImageSize: ImageSize;

  @State()
  private currentImageAlignment: ImageAlignment;

  @State()
  private imageHeightCSS: string;

  @State()
  private imageJustifyContentCSS: string;

  private destroyListener;

  private ignoreUpdateStyle: boolean = false;

  async componentWillLoad() {
    await this.init();

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
    this.currentImageSize = await this.initImageSize();
    this.currentImageAlignment = await this.initImageAlignment();
  }

  private async initCSS() {
    this.imageHeightCSS = this.selectedTarget.style.getPropertyValue('--deckgo-lazy-img-height');
    this.imageJustifyContentCSS = this.selectedTarget.style.getPropertyValue('justify-content');
  }

  private initImageSize(): Promise<ImageSize> {
    return new Promise<ImageSize>((resolve) => {
      if (!this.selectedTarget || !this.selectedTarget.style) {
        resolve(null);
        return;
      }

      if (this.selectedTarget.style.getPropertyValue('--deckgo-lazy-img-height') === '25%') {
        resolve(ImageSize.SMALL);
      } else if (this.selectedTarget.style.getPropertyValue('--deckgo-lazy-img-height') === '50%') {
        resolve(ImageSize.MEDIUM);
      } else if (this.selectedTarget.style.getPropertyValue('--deckgo-lazy-img-height') === '75%') {
        resolve(ImageSize.LARGE);
      } else {
        resolve(ImageSize.ORIGINAL);
      }
    });
  }

  private initImageAlignment(): Promise<ImageAlignment> {
    return new Promise<ImageAlignment>(async (resolve) => {
      if (!this.selectedTarget || !this.selectedTarget.style) {
        resolve(null);
        return;
      }

      if (this.selectedTarget.style.getPropertyValue('justify-content') === 'center') {
        resolve(ImageAlignment.CENTER);
      } else if (this.selectedTarget.style.getPropertyValue('justify-content') === 'flex-end') {
        resolve(ImageAlignment.END);
      } else if (this.selectedTarget.style.getPropertyValue('justify-content') === 'flex-start') {
        resolve(ImageAlignment.START);
      } else {
        const result: ImageAlignment = await this.findSlideDefaultAlignment();
        resolve(result);
      }
    });
  }

  private findSlideDefaultAlignment(): Promise<ImageAlignment> {
    return new Promise<ImageAlignment>((resolve) => {
      const parent: HTMLElement = this.selectedTarget.parentElement;

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

  private toggleImageSize($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.currentImageSize = $event.detail.value;

    if (!this.selectedTarget) {
      return;
    }

    this.updateStyle([
      {
        property: '--deckgo-lazy-img-height',
        value: this.currentImageSize === ImageSize.ORIGINAL ? null : this.currentImageSize
      }
    ]);
  }

  private toggleImageAlignment($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    this.currentImageAlignment = $event.detail.value;

    if (!this.selectedTarget) {
      return;
    }

    this.updateStyle([
      {
        property: 'display',
        value: 'inline-flex'
      },
      {
        property: 'justify-content',
        value: this.currentImageAlignment.toString()
      }
    ]);
  }

  private updateStyle(properties: {property: string; value: string | null}[]) {
    if (this.ignoreUpdateStyle) {
      this.ignoreUpdateStyle = false;
      return;
    }

    setStyle(this.selectedTarget, {
      properties,
      type: 'element',
      updateUI: async (_value: string) => {
        // ion-change triggers the event each time its value changes, because we re-render, it triggers it again
        this.ignoreUpdateStyle = true;

        if (settingsStore.state.editMode === 'css') {
          await this.initCSS();
          return;
        }

        await this.init();
      }
    });

    this.imgDidChange.emit(this.selectedTarget);
  }

  private handleImageHeightInput($event: CustomEvent<KeyboardEvent>) {
    this.imageHeightCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateImageHeightCSS() {
    if (!this.imageHeightCSS || this.imageHeightCSS === '') {
      this.selectedTarget.style.removeProperty('--deckgo-lazy-img-height');
    } else {
      this.selectedTarget.style.setProperty('--deckgo-lazy-img-height', this.imageHeightCSS);
    }

    this.imgDidChange.emit(this.selectedTarget);
  }

  private handleImageJustifyContentInput($event: CustomEvent<KeyboardEvent>) {
    this.imageJustifyContentCSS = ($event.target as InputTargetEvent).value;
  }

  private async updateImageJustifyContentCSS() {
    this.selectedTarget.style.setProperty('display', 'inline-flex');
    this.selectedTarget.style.setProperty('justify-content', this.imageJustifyContentCSS);

    this.imgDidChange.emit(this.selectedTarget);
  }

  render() {
    return (
      <app-expansion-panel
        expanded={settingsStore.state.panels.imageStyle}
        onExpansion={($event: CustomEvent<Expanded>) => SettingsUtils.update({imageStyle: $event.detail})}
      >
        <ion-label slot="title">{i18n.state.editor.image}</ion-label>
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
        <ion-label>{i18n.state.editor.size}</ion-label>
      </ion-item-divider>,

      <ion-item class="select properties">
        <ion-label>{i18n.state.editor.size}</ion-label>

        <ion-select
          value={this.currentImageSize}
          placeholder="Select an image size"
          onIonChange={($event: CustomEvent) => this.toggleImageSize($event)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end"
        >
          <ion-select-option value={ImageSize.SMALL}>{i18n.state.editor.small}</ion-select-option>
          <ion-select-option value={ImageSize.MEDIUM}>{i18n.state.editor.medium}</ion-select-option>
          <ion-select-option value={ImageSize.LARGE}>{i18n.state.editor.large}</ion-select-option>
          <ion-select-option value={ImageSize.ORIGINAL}>{i18n.state.editor.original}</ion-select-option>
        </ion-select>
      </ion-item>,

      <ion-item class="with-padding css">
        <ion-input
          value={this.imageHeightCSS}
          placeholder="height"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleImageHeightInput(e)}
          onIonChange={async () => await this.updateImageHeightCSS()}
        ></ion-input>
      </ion-item>
    ];
  }

  private renderImageAlignment() {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>{i18n.state.editor.alignment}</ion-label>
      </ion-item-divider>,

      <ion-item class="select properties">
        <ion-label>{i18n.state.editor.alignment}</ion-label>

        <ion-select
          value={this.currentImageAlignment}
          placeholder="Align the image"
          onIonChange={($event: CustomEvent) => this.toggleImageAlignment($event)}
          interface="popover"
          mode="md"
          class="ion-padding-start ion-padding-end"
        >
          <ion-select-option value={ImageAlignment.START}>{i18n.state.editor.start}</ion-select-option>
          <ion-select-option value={ImageAlignment.CENTER}>{i18n.state.editor.center}</ion-select-option>
          <ion-select-option value={ImageAlignment.END}>{i18n.state.editor.end}</ion-select-option>
        </ion-select>
      </ion-item>,

      <ion-item class="with-padding css">
        <ion-input
          value={this.imageJustifyContentCSS}
          placeholder="justify-content"
          debounce={500}
          onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleImageJustifyContentInput(e)}
          onIonChange={async () => await this.updateImageJustifyContentCSS()}
        ></ion-input>
      </ion-item>
    ];
  }
}
