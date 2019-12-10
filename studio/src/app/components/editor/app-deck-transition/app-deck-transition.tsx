import {Component, Element, Event, EventEmitter, Prop, h} from '@stencil/core';

import {interval, Subscription} from 'rxjs';

@Component({
    tag: 'app-deck-transition',
    styleUrl: 'app-deck-transition.scss'
})
export class AppDeckTransition {

    @Element() el: HTMLElement;

    @Prop()
    deckElement: HTMLElement;

    @Event() private transitionChange: EventEmitter<void>;

    private timerSubscription: Subscription;

    async componentDidLoad() {
        await this.animateDecks();
    }

    componentDidUnload() {
        if (this.timerSubscription) {
            this.timerSubscription.unsubscribe();
        }
    }

    private async animateDecks() {
        this.timerSubscription = interval(2000).subscribe(async (val: number) => {
            const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-deck');

            if (elements) {
                for (const element of Array.from(elements)) {
                    if (val % 2 === 0) {
                        await (element as any).slideNext();
                    } else {
                        await (element as any).slidePrev();
                    }
                }
            }
        });
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
        return <div class="container ion-margin-top ion-margin-bottom">
            {this.renderDeckItem('slide', 'var(--ion-color-primary)', 'Slide animation')}

            {this.renderDeckItem('fade', 'var(--ion-color-secondary)', 'Fade effect')}

            {this.renderDeckItem('none', 'var(--ion-color-tertiary)', 'Instant transition')}
        </div>
    }

    private renderDeckItem(transition: 'slide' | 'fade' | 'none', nextSlideBackground: string, text: string) {
        return <div class="item" custom-tappable onClick={() => this.applyTransition(transition)}>
            <deckgo-deck embedded={true} keyboard={false} transition={transition}>
                <deckgo-slide-title>
                    <p slot="title">{text}</p>
                </deckgo-slide-title>

                <deckgo-slide-title style={{'--background': `${nextSlideBackground}`, '--color': 'white'}}>
                    <p slot="title">{text}</p>
                </deckgo-slide-title>
            </deckgo-deck>
        </div>;
    }

}
