import {Component, Element, h} from '@stencil/core';

@Component({
    tag: 'app-info-images',
    styleUrl: 'app-info-images.scss'
})
export class AppInfoImages {

    @Element() el: HTMLElement;

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    render() {
        return <div class="ion-padding">
            <h2>Information about your images</h2>
            <p>Please note that currently all the images you would upload will be public.</p>
            <div class="ion-text-center ion-padding-top"><ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>Got it</ion-button></div>
        </div>
    }

}
