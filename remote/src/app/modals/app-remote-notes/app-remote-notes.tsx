import {Component, Element, h, Listen} from '@stencil/core';

@Component({
    tag: 'app-remote-notes'
})
export class AppRemoteNotes {
    @Element() el: HTMLElement;

    componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('popstate', {target: 'window'})
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal(false);
    }

    async closeModal(connect: boolean) {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(connect);
    }

    render() {
        return [
            <app-header>
                <ion-buttons slot="start">
                    <ion-button onClick={() => this.closeModal(false)}>
                        <ion-icon name="close"></ion-icon>
                    </ion-button>
                </ion-buttons>
            </app-header>,

            <ion-content class="ion-padding">
                <app-notes></app-notes>
            </ion-content>
        ];
    }
}
