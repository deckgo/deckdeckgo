import {Component, h, Element, Method} from '@stencil/core';

import {get, set} from 'idb-keyval';

import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

@Component({
    tag: 'app-help',
    styleUrl: 'app-help.scss'
})
export class AppHelp {

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

    private async openGetHelp($event: UIEvent) {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-get-help',
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async () => {
            await set('deckdeckgo_display_help', true);
        });

        await popover.present();
    }

    render() {
        return <ion-tab-button onClick={($event: UIEvent) => this.openGetHelp($event)} color="primary" mode="md" class="get-help-action">
            <ion-icon name="help"></ion-icon>
            <ion-label>Help</ion-label>
        </ion-tab-button>
    }
}
