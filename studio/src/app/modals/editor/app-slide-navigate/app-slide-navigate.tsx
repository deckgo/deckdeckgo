import {Component, Listen, Element, State, h, EventEmitter, Event} from '@stencil/core';
import {ItemReorderEventDetail} from '@ionic/core';

import {findSlidesTitle} from '@deckdeckgo/deck-utils';

@Component({
  tag: 'app-slide-navigate',
  styleUrl: 'app-slide-navigate.scss'
})
export class AppSlideNavigate {
  @Element() el: HTMLElement;

  @State()
  private slides: string[];

  @Event() private reorder: EventEmitter<ItemReorderEventDetail>;

  async componentDidLoad() {
    history.pushState({modal: true}, null);

    this.slides = await findSlidesTitle();
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  async jumpToSlide(index: number) {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(index);
  }

  private onReorder($event: CustomEvent<ItemReorderEventDetail>): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!$event) {
        resolve();
        return;
      }

      this.reorder.emit($event.detail);

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="primary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon name="close"></ion-icon>
            </ion-button>
          </ion-buttons>
          <ion-title class="ion-text-uppercase">Slides</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding" color="light">
        <main class="ion-padding">
          <p class="ion-padding-start ion-padding-end">Jump to a specific slide or change the order of your slides.</p>

          <ion-reorder-group
            onIonItemReorder={($event: CustomEvent<ItemReorderEventDetail>) => this.onReorder($event)}
            disabled={!this.slides || this.slides.length <= 1}>
            {this.renderSlides()}
          </ion-reorder-group>
        </main>
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
