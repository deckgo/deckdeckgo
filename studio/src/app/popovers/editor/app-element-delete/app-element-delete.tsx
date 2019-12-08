import {Component, Element, h} from '@stencil/core';

@Component({
    tag: 'app-element-delete',
    styleUrl: 'app-element-delete.scss'
})
export class AppElementDelete {

    @Element() el: HTMLElement;

    constructor() {
    }

    async closePopover(confirm: boolean) {
        await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss(confirm);
    }

    render() {
        return [
            <ion-grid class="ion-no-padding ion-margin">
                <ion-row>
                    <h3>Delete?</h3>
                    <p><small>This action cannot be undone.</small></p>
                </ion-row>
            </ion-grid>,
            this.renderActions()
        ]
    }

    private renderActions() {
        return <div class="element-delete-actions">
            <button class="no ion-activatable" onClick={() => this.closePopover(false)}>
                <ion-ripple-effect></ion-ripple-effect>
                <ion-label>No</ion-label>
            </button>

            <button class="yes ion-activatable" onClick={() => this.closePopover(true)}>
                <ion-ripple-effect></ion-ripple-effect>
                <ion-label><strong>Yes</strong></ion-label>
            </button>
        </div>
    }

}
