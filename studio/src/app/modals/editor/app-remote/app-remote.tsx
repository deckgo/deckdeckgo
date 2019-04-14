import {Component, Element, Listen, State} from '@stencil/core';


@Component({
    tag: 'app-remote',
    styleUrl: 'app-remote.scss'
})
export class AppRemote {

    @Element() el: HTMLElement;

    @State()
    private remoteEnabled: boolean = false;

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private toggleRemoteEnabled() {
        this.remoteEnabled = !this.remoteEnabled;
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Remote control</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding">

                <p class="ion-padding-start ion-padding-end">Control your presentations using the DeckDeckGo's <a href="https://deckdeckgo.app" target="_blank">remote control <ion-icon name="open"></ion-icon></a></p>

                <ion-list>
                    <ion-item>
                        <ion-label>Remote control {this.remoteEnabled ? 'enabled' : 'disabled'}</ion-label>
                        <ion-toggle slot="end" checked={this.remoteEnabled} onIonChange={() => this.toggleRemoteEnabled()}></ion-toggle>
                    </ion-item>

                    <ion-item>
                        <ion-label class={this.remoteEnabled ? '' : 'disabled'}>Can't connect?</ion-label>
                        <ion-button size="small" color="primary" disabled={!this.remoteEnabled}>Refresh</ion-button>
                    </ion-item>
                </ion-list>
            </ion-content>
        ];
    }

}
