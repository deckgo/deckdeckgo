import {Component, h, Element} from '@stencil/core';

import {IonControllerUtils} from '../../../../utils/core/ion-controller-utils';

@Component({
    tag: 'app-help-action'
})
export class AppHelpAction {

    @Element() el: HTMLElement;

    private async openGetHelp() {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-get-help',
            mode: 'ios',
            cssClass: 'info'
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
