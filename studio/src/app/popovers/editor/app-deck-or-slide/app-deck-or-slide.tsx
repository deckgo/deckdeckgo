import {Component, Element} from '@stencil/core';

@Component({
    tag: 'app-deck-or-slide',
    styleUrl: 'app-deck-or-slide.scss'
})
export class AppDeckOrSlide {

    @Element() el: HTMLElement;

    async closePopover(deck: boolean) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            deck: deck
        });
    }

    render() {
        return <div padding>
            <a onClick={() => this.closePopover(true)}><p>Apply to the all deck</p></a>
            <a onClick={() => this.closePopover(false)}><p>Just to this slide</p></a>
        </div>
    }
}
