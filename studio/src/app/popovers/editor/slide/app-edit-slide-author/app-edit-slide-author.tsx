import {Component, Element, Prop, h, EventEmitter, State} from '@stencil/core';

enum ImageMode {
  COVER = 'cover',
  CIRCLE = 'circle',
  NONE = 'none'
}

@Component({
  tag: 'app-edit-slide-author'
})
export class AppEditSlideAuthor {
  @Element() el: HTMLElement;

  @Prop()
  selectedElement: HTMLElement;

  @Prop()
  slideDidChange: EventEmitter<HTMLElement>;

  @State()
  private currentImageMode: ImageMode;

  async componentWillLoad() {
    this.currentImageMode = await this.initImageMode();
  }

  private initImageMode(): Promise<ImageMode> {
    return new Promise<ImageMode>(async (resolve) => {
      if (!this.selectedElement) {
        resolve(null);
        return;
      }

      if (this.selectedElement.getAttribute('img-mode') === 'none') {
        resolve(ImageMode.NONE);
      } else if (this.selectedElement.getAttribute('img-mode') === 'circle') {
        resolve(ImageMode.CIRCLE);
      } else {
        resolve(ImageMode.COVER);
      }
    });
  }

  private toggleImageMode($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      this.currentImageMode = $event.detail.value;

      if (!this.selectedElement) {
        resolve();
        return;
      }

      this.selectedElement.setAttribute('img-mode', this.currentImageMode);

      this.slideDidChange.emit(this.selectedElement);

      resolve();
    });
  }

  render() {
    return [
      <ion-item-divider class="ion-padding-top">
        <ion-label>Image</ion-label>
      </ion-item-divider>,

      <ion-item class="select">
        <ion-label>Image</ion-label>

        <ion-select
          value={this.currentImageMode}
          placeholder="Select a mode to display the image"
          onIonChange={(e: CustomEvent) => this.toggleImageMode(e)}
          class="ion-padding-start ion-padding-end">
          <ion-select-option value={ImageMode.COVER}>Cover</ion-select-option>
          <ion-select-option value={ImageMode.CIRCLE}>Circle</ion-select-option>
          <ion-select-option value={ImageMode.NONE}>None</ion-select-option>
        </ion-select>
      </ion-item>
    ];
  }
}
