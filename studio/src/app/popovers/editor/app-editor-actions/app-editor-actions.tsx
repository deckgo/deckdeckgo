import {Component, Element, State, h} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {EditorAction} from './editor-action';

@Component({
    tag: 'app-editor-actions',
    styleUrl: 'app-editor-actions.scss'
})
export class AppEditorActions {

    @Element() el: HTMLElement;

    @State()
    private mobile: boolean = false;

    componentWillLoad() {
        this.mobile = DeckDeckGoUtils.isMobile();
    }

    async closePopover(action: EditorAction) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        // TODO: Share when published

        return <div class="padding">
            <a onClick={() => this.closePopover(EditorAction.JUMP_TO)}><p>Jump to slide</p></a>
            {this.renderFullscreenOption()}
            <a onClick={() => this.closePopover(EditorAction.REMOTE)}><p>Remote control</p></a>
        </div>
    }

    private renderFullscreenOption() {
        if (!this.mobile) {
            return <a onClick={() => this.closePopover(EditorAction.FULLSCREEN)}><p>Fullscreen</p></a>;
        } else {
            return undefined;
        }
    }
}
