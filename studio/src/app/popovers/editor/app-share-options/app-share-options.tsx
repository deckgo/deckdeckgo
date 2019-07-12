import {Component, Element, h} from '@stencil/core';

import {ShareOption} from './share-option';

@Component({
    tag: 'app-share-options',
    styleUrl: 'app-share-options.scss'
})
export class AppShareOptions {

    @Element() el: HTMLElement;

    private async closePopover(action: ShareOption) {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss({
            action: action
        });
    }

    render() {
        return <div class="ion-padding">
            <a onClick={() => this.closePopover(ShareOption.PUBLISH)}><p>Update published presentation</p></a>
            <a onClick={() => this.closePopover(ShareOption.SHARE)}><p>Share</p></a>
        </div>
    }

}
