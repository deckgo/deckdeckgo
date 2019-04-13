import {Component, Element} from '@stencil/core';

import {EditorAction} from './editor-action';

@Component({
    tag: 'app-editor-actions',
    styleUrl: 'app-editor-actions.scss'
})
export class AppEditorActions {

    @Element() el: HTMLElement;

    async closePopover(action: EditorAction) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        // TODO: Share when published

        return <div padding>
            <a onClick={() => this.closePopover(EditorAction.JUMP_TO)}><p>Jump to slide</p></a>
            <a onClick={() => this.closePopover(EditorAction.FULLSCREEN)}><p>Fullscreen</p></a>
        </div>
    }
}
