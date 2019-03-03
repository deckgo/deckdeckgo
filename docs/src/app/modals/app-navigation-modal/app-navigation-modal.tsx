import {Component, Element, Listen} from '@stencil/core';

@Component({
    tag: 'app-navigation-modal',
    styleUrl: 'app-navigation-modal.scss'
})
export class AppNavigationModal {
    @Element() el: HTMLElement;

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal(false);
    }

    async closeModal(connect: boolean) {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(connect);
    }

    render() {
        return [
            <ion-header>
              <ion-toolbar>
                <ion-buttons slot="start">
                  <ion-button onClick={() => this.closeModal(false)}>
                    <ion-icon name="close"></ion-icon>
                  </ion-button>
                </ion-buttons>
              </ion-toolbar>
            </ion-header>,

            <ion-content padding>
                <div>
                  <a href="/docs">Docs</a>
                  <a href="https://deckdeckgo.com" padding-top>Demo</a>
                  <a href="https://github.com/deckgo" padding-top>Github</a>
                </div>
            </ion-content>
        ];
    }
}
