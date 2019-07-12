import {Component, Element, State, h} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {EditorMoreAction} from './editor-more-action';

@Component({
    tag: 'app-editor-more-actions',
    styleUrl: 'app-editor-more-actions.scss'
})
export class AppEditorMoreActions {

    @Element() el: HTMLElement;

    @State()
    private mobile: boolean = false;

    componentWillLoad() {
        this.mobile = DeckDeckGoUtils.isMobile();
    }

    private async closePopover(action: EditorMoreAction) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        // TODO: Share when published

        return <div class="ion-padding">
            <a onClick={() => this.closePopover(EditorMoreAction.JUMP_TO)}><p>Jump to slide</p></a>
            {this.renderFullscreenOption()}
            <a onClick={() => this.closePopover(EditorMoreAction.REMOTE)}><p>Remote control</p></a>
        </div>
    }

    private renderFullscreenOption() {
        if (!this.mobile) {
            return <a onClick={() => this.closePopover(EditorMoreAction.FULLSCREEN)}><p>Fullscreen</p></a>;
        } else {
            return undefined;
        }
    }
}
