import {Component, Element, h} from '@stencil/core';

@Component({
    tag: 'app-fullscreen-info'
})
export class AppFullscreenInfo {

    @Element() el: HTMLElement;

    private async closePopover() {
        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss();
    }

    render() {
        return <div class="ion-padding">
            <h2>Tips about fullscreen</h2>
            <p>Cool thing, with DeckDeckGo you could edit your presentation in full screen mode too!</p>
            <p>Correct typo, add slide, change colors etc. all features are also available in full screen mode 😉</p>
            <div class="ion-text-center ion-padding-top"><ion-button size="small" shape="round" color="primary" onClick={() => this.closePopover()}>Got it</ion-button></div>
        </div>
    }

}
