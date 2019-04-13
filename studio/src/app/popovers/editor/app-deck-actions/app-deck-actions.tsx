import {Component, Element} from '@stencil/core';

import {DeckAction} from './deck-action';

@Component({
    tag: 'app-deck-actions',
    styleUrl: 'app-deck-actions.scss'
})
export class AppDeckActions {

    @Element() el: HTMLElement;

    async closePopover(action: DeckAction) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        // TODO: Share when published

        return <div padding>
            <a onClick={() => this.closePopover(DeckAction.JUMP_TO)}><p>Jump to slide</p></a>
            <a onClick={() => this.closePopover(DeckAction.FULLSCREEN)}><p>Fullscreen</p></a>
        </div>
    }
}
