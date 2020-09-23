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
  private selectedTransition: 'slide' | 'fade' | 'none';

  @State()
  private selectedDirection: 'horizontal' | 'vertical' | 'papyrus';

  private timerInterval: NodeJS.Timeout;
  private timerCounter: number = 0;

  async componentWillLoad() {
    await this.initSelectedTransition();
  }

  async componentDidLoad() {
    await this.animateDecks();
  }

  disconnectedCallback() {
    if (this.timerInterval) {
      clearInterval(this.timerInterval);
    }
  }

  private initSelectedTransition(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.deckElement || !this.deckElement.hasAttribute('transition')) {
        this.selectedTransition = 'slide';
        resolve();
        return;
      }

      this.selectedTransition = this.deckElement.getAttribute('transition') as 'slide' | 'fade' | 'none';

      resolve();
    });
  }

  private async animateDecks() {
    this.timerInterval = setInterval(async () => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-deck');

      if (elements) {
        for (const element of Array.from(elements)) {
          if (this.timerCounter % 2 === 0) {
            await (element as any).slideNext();
          } else {
            await (element as any).slidePrev();
          }
        }
      }

      this.timerCounter++;
    }, 2000);
  }

  private async applyDirection(direction: 'horizontal' | 'vertical' | 'papyrus') {
    if (!this.deckElement || !direction) {
      return;
    }

    this.deckElement.setAttribute('direction', direction);

    this.transitionChange.emit();
  }

  private async applyAnimation(animation: 'slide' | 'fade' | 'none') {
    if (!this.deckElement || !animation) {
      return;
    }

    this.deckElement.setAttribute('transition', animation);

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
          {this.renderDeckItem('horizontal', this.selectedTransition, 'var(--ion-color-primary)', 'Horizontal', this.selectedDirection === 'horizontal', () =>
            this.applyDirection('horizontal')
          )}

          {this.renderDeckItem('vertical', this.selectedTransition, 'var(--ion-color-secondary)', 'Vertical', this.selectedDirection === 'vertical', () =>
            this.applyDirection('vertical')
          )}

          {this.renderDeckItem('papyrus', this.selectedTransition, 'var(--ion-color-tertiary)', 'Papyrus', this.selectedDirection === 'papyrus', () =>
            this.applyDirection('papyrus')
          )}
        </div>
      </app-expansion-panel>
    );
  }

  private renderAnimation() {
    return (
      <app-expansion-panel>
        <ion-label slot="title">Animation</ion-label>
        <div class="container ion-margin-bottom">
          {this.renderDeckItem(this.selectedDirection, 'slide', 'var(--ion-color-primary)', 'Slide animation', this.selectedTransition === 'slide', () =>
            this.applyAnimation('slide')
          )}

          {this.renderDeckItem(this.selectedDirection, 'fade', 'var(--ion-color-secondary)', 'Fade effect', this.selectedTransition === 'fade', () =>
            this.applyAnimation('fade')
          )}

          {this.renderDeckItem(this.selectedDirection, 'none', 'var(--ion-color-tertiary)', 'Instant transition', this.selectedTransition === 'none', () =>
            this.applyAnimation('none')
          )}
        </div>
      </app-expansion-panel>
    );
  }

  private renderDeckItem(
    direction: 'horizontal' | 'vertical' | 'papyrus',
    transition: 'slide' | 'fade' | 'none',
    nextSlideBackground: string,
    text: string,
    selected: boolean,
    action: (selection) => Promise<void>
  ) {
    return (
      <div class={`item ${selected ? 'selected' : ''}`} custom-tappable onClick={action}>
        <deckgo-deck embedded={true} keyboard={false} transition={transition} direction={direction}>
          <deckgo-slide-title>
            <p slot="title">{text}</p>
          </deckgo-slide-title>

          <deckgo-slide-title style={{'--background': `${nextSlideBackground}`, '--color': 'white'}}>
            <p slot="title">{text}</p>
          </deckgo-slide-title>
        </deckgo-deck>
      </div>
    );
  }
}
