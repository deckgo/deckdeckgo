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

  private applyTransition(transition: 'slide' | 'fade' | 'none'): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.deckElement || !transition) {
        resolve();
        return;
      }

      this.deckElement.setAttribute('transition', transition);

      this.transitionChange.emit();

      resolve();
    });
  }

  render() {
    return (
      <div class="container ion-margin-top ion-margin-bottom">
        {this.renderDeckItem('slide', 'var(--ion-color-primary)', 'Slide animation', this.selectedTransition === 'slide')}

        {this.renderDeckItem('fade', 'var(--ion-color-secondary)', 'Fade effect', this.selectedTransition === 'fade')}

        {this.renderDeckItem('none', 'var(--ion-color-tertiary)', 'Instant transition', this.selectedTransition === 'none')}
      </div>
    );
  }

  private renderDeckItem(transition: 'slide' | 'fade' | 'none', nextSlideBackground: string, text: string, selected: boolean) {
    return (
      <div class={`item ${transition} ${selected ? 'selected' : ''}`} custom-tappable onClick={() => this.applyTransition(transition)}>
        <deckgo-deck embedded={true} keyboard={false} transition={transition}>
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
