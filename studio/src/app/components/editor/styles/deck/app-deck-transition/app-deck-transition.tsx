import {Component, Element, Event, EventEmitter, Prop, h, State} from '@stencil/core';

@Component({
  tag: 'app-deck-transition',
  styleUrl: 'app-deck-transition.scss',
})
export class AppDeckTransition {
  @Element() el: HTMLElement;

  @Prop()
  deckElement: HTMLElement;

  @Event() private transitionChange: EventEmitter<void>;

  @State()
  private selectedAnimation: 'slide' | 'fade' | 'none';

  @State()
  private selectedDirection: 'horizontal' | 'vertical' | 'papyrus';

  private timerIntervalDecks: NodeJS.Timeout;
  private timerCounterDecks: number = 0;

  private timerIntervalPapyrus: NodeJS.Timeout;
  private timerCounterPapyrus: number = 0;

  async componentWillLoad() {
    await this.initSelectedAnimation();
    await this.initSelectedDirection();
  }

  async componentDidLoad() {
    await this.animateDecks();
    await this.animatePapyrus();
  }

  disconnectedCallback() {
    if (this.timerIntervalDecks) {
      clearInterval(this.timerIntervalDecks);
    }

    if (this.timerIntervalPapyrus) {
      clearInterval(this.timerIntervalPapyrus);
    }
  }

  private async initSelectedAnimation() {
    if (!this.deckElement || !this.deckElement.hasAttribute('animation')) {
      this.selectedAnimation = 'slide';
      return;
    }

    this.selectedAnimation = this.deckElement.getAttribute('animation') as 'slide' | 'fade' | 'none';
  }

  private async initSelectedDirection() {
    if (!this.deckElement || !this.deckElement.hasAttribute('direction')) {
      this.selectedDirection = 'horizontal';
      return;
    }

    this.selectedDirection = this.deckElement.getAttribute('direction') as 'horizontal' | 'vertical' | 'papyrus';
  }

  private async animateDecks() {
    this.timerIntervalDecks = setInterval(async () => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-deck:not(.papyrus)');

      if (elements) {
        for (const element of Array.from(elements)) {
          if (element.classList.contains('showcase-direction')) {
            await this.animateDecksDirection(element);
          } else {
            await this.animationDecksAnimation(element);
          }
        }
      }

      this.timerCounterDecks++;
    }, 1400);
  }

  private async animatePapyrus() {
    this.timerIntervalPapyrus = setInterval(async () => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('div.showcase-papyrus');

      if (elements) {
        for (const element of Array.from(elements)) {
          this.timerCounterPapyrus = this.timerCounterPapyrus > 19 ? 0 : this.timerCounterPapyrus + 1;

          element.scroll({top: element.offsetHeight * 4 * (this.timerCounterPapyrus / 20), behavior: 'smooth'});
        }
      }

      this.timerCounterPapyrus++;
    }, 500);
  }

  private async animateDecksDirection(element: HTMLElement) {
    const end: boolean = await (element as any).isEnd();

    if (end) {
      await (element as any).slideTo(0);
    } else {
      await (element as any).slideNext();
    }
  }

  private async animationDecksAnimation(element: HTMLElement) {
    if (this.timerCounterDecks % 2 === 0) {
      await (element as any).slideNext();
    } else {
      await (element as any).slidePrev();
    }
  }

  private async applyDirection(direction: 'horizontal' | 'vertical' | 'papyrus') {
    if (!this.deckElement || !direction) {
      return;
    }

    this.deckElement.setAttribute('direction', direction);

    this.selectedDirection = direction;

    this.transitionChange.emit();
  }

  private async applyAnimation(animation: 'slide' | 'fade' | 'none') {
    if (!this.deckElement || !animation) {
      return;
    }

    this.deckElement.setAttribute('animation', animation);

    this.selectedAnimation = animation;

    this.transitionChange.emit();
  }

  render() {
    return [this.renderDirection(), this.renderAnimation()];
  }

  private renderDirection() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Direction</ion-label>

        <div class="device">
          <ion-segment mode="ios" value="default">
            <ion-segment-button value="default" mode="ios">
              <ion-label>Default</ion-label>
            </ion-segment-button>
            <ion-segment-button value="mobile" mode="ios">
              <ion-label>Mobile</ion-label>
            </ion-segment-button>
          </ion-segment>
        </div>

        <div class="container ion-margin-bottom">
          {this.renderDeckItem(
            'direction',
            'horizontal',
            this.selectedAnimation,
            'var(--ion-color-primary)',
            'Horizontal',
            this.selectedDirection === 'horizontal',
            () => this.applyDirection('horizontal')
          )}

          {this.renderDeckItem(
            'direction',
            'vertical',
            this.selectedAnimation,
            'var(--ion-color-secondary)',
            'Vertical',
            this.selectedDirection === 'vertical',
            () => this.applyDirection('vertical')
          )}

          {this.renderShowcasePapyrus('var(--ion-color-tertiary)', 'Papyrus', this.selectedDirection === 'papyrus', () => this.applyDirection('papyrus'))}
        </div>
      </app-expansion-panel>
    );
  }

  private renderAnimation() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Animation</ion-label>
        <div class="container ion-margin-bottom">
          {this.renderDeckItem(
            'animation',
            this.selectedDirection,
            'slide',
            'var(--ion-color-primary)',
            'Slide animation',
            this.selectedAnimation === 'slide',
            () => this.applyAnimation('slide')
          )}

          {this.renderDeckItem(
            'animation',
            this.selectedDirection,
            'fade',
            'var(--ion-color-secondary)',
            'Fade effect',
            this.selectedAnimation === 'fade',
            () => this.applyAnimation('fade')
          )}

          {this.renderDeckItem(
            'animation',
            this.selectedDirection,
            'none',
            'var(--ion-color-tertiary)',
            'Instant transition',
            this.selectedAnimation === 'none',
            () => this.applyAnimation('none')
          )}
        </div>
      </app-expansion-panel>
    );
  }

  private renderDeckItem(
    showcase: 'animation' | 'direction',
    direction: 'horizontal' | 'vertical' | 'papyrus',
    animation: 'slide' | 'fade' | 'none',
    nextSlideBackground: string,
    text: string,
    selected: boolean,
    action: (selection) => Promise<void>
  ) {
    return (
      <div class={`item ${selected ? 'selected' : ''} item-direction-${direction} item-animation-${animation}`} custom-tappable onClick={action}>
        <deckgo-deck embedded={true} keyboard={false} animation={animation} direction={direction} class={`showcase-${showcase}`}>
          <deckgo-slide-title style={{'--background': `${nextSlideBackground}`, '--color': 'white'}}>
            <p slot="title">{text}</p>
          </deckgo-slide-title>

          <deckgo-slide-title>
            <p slot="title">{text}</p>
          </deckgo-slide-title>

          {showcase === 'direction' ? this.renderMoreSlides(nextSlideBackground, text) : undefined}
        </deckgo-deck>
      </div>
    );
  }

  private renderShowcasePapyrus(nextSlideBackground: string, text: string, selected: boolean, action: (selection) => Promise<void>) {
    return (
      <div class={`item ${selected ? 'selected' : ''} item-direction-papyrus`} custom-tappable onClick={action}>
        <div class="showcase-papyrus">
          <deckgo-slide-title style={{'--background': `${nextSlideBackground}`, '--color': 'white'}}>
            <p slot="title">{text}</p>
          </deckgo-slide-title>

          <deckgo-slide-title>
            <p slot="title">{text}</p>
          </deckgo-slide-title>

          {this.renderMoreSlides(nextSlideBackground, text)}
        </div>
      </div>
    );
  }

  private renderMoreSlides(nextSlideBackground: string, text: string) {
    return [
      <deckgo-slide-title style={{'--background': `${nextSlideBackground}`, '--color': 'white'}}>
        <p slot="title">{text}</p>
      </deckgo-slide-title>,

      <deckgo-slide-title>
        <p slot="title">{text}</p>
      </deckgo-slide-title>,
    ];
  }
}
