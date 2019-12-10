import {Component, Element, Event, EventEmitter, Prop, h} from '@stencil/core';

@Component({
    tag: 'app-deck-transition'
})
export class AppDeckTransition {

    @Element() el: HTMLElement;

    @Prop()
    deckElement: HTMLElement;

    @Event() private transitionChange: EventEmitter<void>;

    private applyTransition(transition: 'slide' | 'fade' | 'none'): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.deckElement || !transition) {
                resolve();
                return;
            }

            if (transition !== 'slide') {
                this.deckElement.setAttribute('transition', transition);
            } else {
                this.deckElement.removeAttribute('transition');
            }

            this.transitionChange.emit();

            resolve();
        });
    }

    render() {
        return <button onClick={() => this.applyTransition('fade')}>Test set Fade</button>
    }

}
