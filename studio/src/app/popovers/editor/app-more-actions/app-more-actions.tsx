import {Component, Element, State, h} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {MoreAction} from './more-action';

@Component({
    tag: 'app-more-actions',
    styleUrl: 'app-more-actions.scss'
})
export class AppMoreActions {

    @Element() el: HTMLElement;

    @State()
    private mobile: boolean = false;

    componentWillLoad() {
        this.mobile = DeckDeckGoUtils.isMobile();
    }

    private async closePopover(action: MoreAction) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        // TODO: Share when published

        return <div class="ion-padding">
            <a onClick={() => this.closePopover(MoreAction.JUMP_TO)}><p>Jump to slide</p></a>
            {this.renderFullscreenOption()}
            <a onClick={() => this.closePopover(MoreAction.REMOTE)}><p>Remote control</p></a>
        </div>
    }

    private renderFullscreenOption() {
        if (!this.mobile) {
            return <a onClick={() => this.closePopover(MoreAction.FULLSCREEN)}><p>Fullscreen</p></a>;
        } else {
            return undefined;
        }
    }
}
