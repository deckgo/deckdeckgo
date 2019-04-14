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

                <p class="ion-padding-start ion-padding-end">Control your presentations using the DeckDeckGo's <a href="https://deckdeckgo.app" target="_blank">remote control <ion-icon name="open"></ion-icon></a></p>

                <ion-list>
                    <ion-item>
                        <ion-label>Remote control {this.remoteEnabled ? 'enabled' : 'disabled'}</ion-label>
                        <ion-toggle slot="end" checked={this.remoteEnabled} onIonChange={() => this.toggleRemoteEnabled()}></ion-toggle>
                    </ion-item>
                </ion-list>
            </ion-content>
        ];
    }

}
