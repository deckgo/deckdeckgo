import {Component, Element, Listen, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import {RemoteService} from '../../../services/editor/remote/remote.service';

@Component({
    tag: 'app-remote',
    styleUrl: 'app-remote.scss'
})
export class AppRemote {

    @Element() el: HTMLElement;

    @State()
    private remoteEnabled: boolean = false;

    private remoteService: RemoteService;

    constructor() {
        this.remoteService = RemoteService.getInstance();
    }

    async componentWillLoad() {
        this.remoteService.watch().pipe(take(1)).subscribe((enable: boolean) => {
            this.remoteEnabled = enable;
        });
    }

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

    private async toggleRemoteEnabled() {
        this.remoteEnabled = !this.remoteEnabled;
        await this.remoteService.switch(this.remoteEnabled);
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

                <ion-list>
                    <ion-item>
                        {this.renderLabel()}
                        <ion-toggle slot="end" checked={this.remoteEnabled} onIonChange={() => this.toggleRemoteEnabled()}></ion-toggle>
                    </ion-item>
                </ion-list>

                <p class="ion-padding-start ion-padding-end">Don't have the remote control app on your phone yet? Scan the following QR Code or get the Progressive Web Apps at <a href="https://deckdeckgo.app" target="_blank">https://deckdeckgo.app <ion-icon name="open"></ion-icon></a></p>

                <div class="qrcode-container">
                    <deckgo-qrcode content="https://deckdeckgo.app">
                        <ion-icon slot="logo" src="/assets/img/deckdeckgo-logo.svg"></ion-icon>
                    </deckgo-qrcode>
                </div>
            </ion-content>
        ];
    }

    private renderLabel() {
        if (this.remoteEnabled) {
            return <ion-label class="ion-text-wrap">Toggle if you wish to disable the remote control</ion-label>
        } else {
            return <ion-label class="ion-text-wrap">Toggle to <strong>enable</strong> the remote control</ion-label>
        }
    }

}
