import {Component, Listen, Element, Prop} from '@stencil/core';

import {DeckdeckgoSlideDefinition} from 'deckdeckgo-types';

@Component({
    tag: 'app-remote-slide-picker',
    styleUrl: 'app-remote-slide-picker.scss'
})
export class AppRemoteSettings {

    @Element() el: HTMLElement;

    @Prop()
    slides: DeckdeckgoSlideDefinition[];

    async componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private async jumpToSlide(index: number) {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(index);
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
                    <ion-title class="ion-text-uppercase">DeckDeckGo</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <ion-list>
                    <ion-list-header class="ion-padding-bottom ion-padding-top">
                        <ion-label>Jump to slide</ion-label>
                    </ion-list-header>

                    {this.renderSlides()}
                </ion-list>
            </ion-content>
        ];
    }

    private renderSlides() {
        if (this.slides && this.slides.length > 0) {
            return (
                this.slides.map((slideDefinition: DeckdeckgoSlideDefinition, i: number) => {

                    const text = 'Slide ' + (i + 1) + (slideDefinition.title ? ': ' + slideDefinition.title : '');

                    return <ion-item ion-item button onClick={() => this.jumpToSlide(i)}>
                        <ion-label class="ion-padding-start">{text}</ion-label>
                    </ion-item>
                })
            );
        } else {
            return undefined;
        }
    }
}
