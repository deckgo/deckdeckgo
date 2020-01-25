import {Component, Listen, Element, h, State} from '@stencil/core';

import {findSlidesTitle} from '@deckdeckgo/deck-utils';

@Component({
  tag: 'app-remote-slide-picker',
  styleUrl: 'app-remote-slide-picker.scss'
})
export class AppRemoteSettings {
  @Element() el: HTMLElement;

  @State()
  private slides: string[];

  async componentDidLoad() {
    history.pushState({modal: true}, null);

    this.slides = await findSlidesTitle();
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackbutton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async jumpToSlide(index: number) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(index);
  }

  render() {
    return [
      <app-header>
        <ion-buttons slot="start">
          <ion-button onClick={() => this.closeModal()}>
            <ion-icon name="close"></ion-icon>
          </ion-button>
        </ion-buttons>
      </app-header>,
      <ion-content class="ion-padding">
        <ion-list>
          <ion-list-header class="ion-padding-bottom ion-padding-top">
            <ion-label>Jump to slide</ion-label>
          </ion-list-header>

          {this.renderSlides()}
        </ion-list>
      </ion-content>
    ];
  }

  private renderSlides() {
    if (this.slides && this.slides.length > 0) {
      return this.slides.map((slideTitle: string, i: number) => {
        const text = 'Slide ' + (i + 1) + (slideTitle ? ': ' + slideTitle : '');

        return (
          <ion-item ion-item button onClick={() => this.jumpToSlide(i)} detail={false}>
            <ion-label>{text}</ion-label>
            <ion-reorder slot="end"></ion-reorder>
          </ion-item>
        );
      });
    } else {
      return undefined;
    }
  }
}
