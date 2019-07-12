import {Component, h, Element, Method} from '@stencil/core';

import {get, set} from 'idb-keyval';

import {IonControllerUtils} from '../../../../utils/core/ion-controller-utils';

@Component({
    tag: 'app-help-action'
})
export class AppHelpAction {

    @Element() el: HTMLElement;

    @Method()
    async displayHelp() {
        const helpDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_help');

        if (!helpDisplayedOnce) {
            const button: HTMLIonTabButtonElement = this.el.querySelector('ion-tab-button.get-help-action');

            if (button) {
                button.click();
            }
        }
    }

    private async openGetHelp() {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-get-help',
            mode: 'ios'
        });

        popover.onDidDismiss().then(async () => {
            await set('deckdeckgo_display_help', true);
        });

        await popover.present();
    }

    render() {
        return <ion-tab-button onClick={() => this.openGetHelp()} color="primary" mode="md" class="get-help-action">
            <ion-icon name="help" md="md-help" ios="md-help"></ion-icon>
            <ion-label>Help</ion-label>
        </ion-tab-button>
    }
}
