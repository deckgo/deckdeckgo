import {Component, Element, Prop} from '@stencil/core';

import {ImageAction} from './image-action';

@Component({
    tag: 'app-image',
    styleUrl: 'app-image.scss'
})
export class AppImage {

    @Element() el: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    private applyToAllDeck: boolean = false;

    private async closePopover(action: ImageAction) {
        const data = {
            action: action
        };

        if (this.deckOrSlide) {
            data['applyToAllDeck'] = this.applyToAllDeck;
        }

        await (this.el.closest('ion-popover') as HTMLIonModalElement).dismiss(data);
    }

    render() {
        return [<div class="ion-padding"><h2>{this.deckOrSlide ? 'Background' : 'Image'}</h2></div>,
            <ion-list>
                {this.renderDeckOrSlide()}

                <ion-item class="ion-margin-top action-button">
                    <ion-button shape="round" onClick={() => this.closePopover(ImageAction.OPEN_PHOTOS)} color="primary">
                        <ion-label class="ion-text-uppercase">Add a stock photo</ion-label>
                    </ion-button>
                </ion-item>

                {this.renderDeleteAction()}
            </ion-list>
        ];
    }

    private selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyToAllDeck = $event.detail.value;
        }
    }

    private renderDeckOrSlide() {
        if (!this.deckOrSlide) {
            return undefined;
        } else{
            return [
                <ion-item-divider class="ion-padding-top"><ion-label>Apply change to</ion-label></ion-item-divider>,
                <ion-radio-group onIonChange={($event) => this.selectApplyToAllDeck($event)}>
                    <ion-item>
                        <ion-label>Just this slide</ion-label>
                        <ion-radio slot="start" value={false} checked></ion-radio>
                    </ion-item>
                    <ion-item>
                        <ion-label>The all deck</ion-label>
                        <ion-radio slot="start" value={true}></ion-radio>
                    </ion-item>
                </ion-radio-group>
            ]
        }
    }

    private renderDeleteAction() {
        if (!this.deckOrSlide) {
            return undefined;
        } else {
            return <ion-item class="action-button">
                <ion-button shape="round" onClick={() => this.closePopover(ImageAction.DELETE_PHOTO)} color="medium" fill="outline">
                    <ion-label class="ion-text-uppercase">Delete background</ion-label>
                </ion-button>
            </ion-item>;
        }
    }
}
