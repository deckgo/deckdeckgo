import {Component, Element, State, h, EventEmitter, Event, Host} from '@stencil/core';

import {ItemReorderEventDetail} from '@ionic/core';

import {findSlidesTitle} from '@deckdeckgo/deck-utils';

@Component({
  tag: 'app-slide-navigate',
  styleUrl: 'app-slide-navigate.scss',
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

  async jumpToSlide(index: number) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(index);
  }

  private onReorder($event: CustomEvent<ItemReorderEventDetail>) {
    this.reorder.emit($event?.detail);
  }

  render() {
    return (
      <Host>
        <p>Jump to a specific slide or change their order.</p>

        <ion-reorder-group
          onIonItemReorder={($event: CustomEvent<ItemReorderEventDetail>) => this.onReorder($event)}
          disabled={!this.slides || this.slides.length <= 1}>
          {this.renderSlides()}
        </ion-reorder-group>
      </Host>
    );
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
