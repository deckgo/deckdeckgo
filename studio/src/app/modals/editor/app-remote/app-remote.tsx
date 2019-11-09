import {Component, Element, Listen, State, h} from '@stencil/core';

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

    @State()
    private qrCodeURI: string = 'https://deckdeckgo.app';

    constructor() {
        this.remoteService = RemoteService.getInstance();
    }

    async componentWillLoad() {
        this.remoteService.watch().pipe(take(1)).subscribe((enable: boolean) => {
            this.remoteEnabled = enable;
        });

        await this.initQRCodeURI();

        await this.initCloseOnConnected();
    }

    async componentDidUnload() {
        await this.destroyCloseOnConnected();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    private initCloseOnConnected(): Promise<void> {
        return new Promise<void>((resolve) => {
            const deckgoRemoteElement = document.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            deckgoRemoteElement.addEventListener('state', this.onWatchState, { passive: true });

            resolve();
        });
    }

    private destroyCloseOnConnected(): Promise<void> {
        return new Promise<void>((resolve) => {
            const deckgoRemoteElement = document.querySelector('deckgo-remote');

            if (!deckgoRemoteElement) {
                resolve();
                return;
            }

            deckgoRemoteElement.removeEventListener('state', this.onWatchState);

            resolve();
        });
    }

    private onWatchState = async ($event) => {
        // See ConnectionState.CONNECTED which is 3
        if ($event && $event.detail === 3) {
            await this.closeModal();
        }
    };

    @Listen('popstate', { target: 'window' })
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

    private initQRCodeURI(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const room: string = await this.remoteService.getRoom();

            if (room && room !== undefined && room !== '') {
                this.qrCodeURI = `https://deckdeckgo.app/remote/${room}`;
            }

            resolve();
        });
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

                <p class="ion-padding-start ion-padding-end">Remote control your presentation with your phone or any devices. Scan the following QR Code to open directly your deck or get the Progressive Web Apps at <a href="https://deckdeckgo.app" target="_blank">https://deckdeckgo.app <ion-icon name="open"></ion-icon></a></p>

                <div class="qrcode-container">
                    <deckgo-qrcode content={this.qrCodeURI}>
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
