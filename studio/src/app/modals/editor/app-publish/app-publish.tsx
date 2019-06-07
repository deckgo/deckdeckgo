import {Component, Element, Listen, h, State} from '@stencil/core';

@Component({
    tag: 'app-publish',
    styleUrl: 'app-publish.scss'
})
export class AppPublish {

    @Element() el: HTMLElement;

    @State()
    private publishedUrl: string;

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('popstate', { target: 'window' })
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private published($event: CustomEvent) {
        if ($event && $event.detail) {
            this.publishedUrl = $event.detail;
        }
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="tertiary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Ready to publish?</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content class="ion-padding fullscreen-padding">
                {this.renderMain()}
            </ion-content>
        ];
    }

    private renderMain() {
        if (this.publishedUrl && this.publishedUrl !== undefined) {
            return <app-publish-done></app-publish-done>
        } else {
            return <app-publish-edit onPublished={($event: CustomEvent) => this.published($event)}></app-publish-edit>;
        }
    }

}
