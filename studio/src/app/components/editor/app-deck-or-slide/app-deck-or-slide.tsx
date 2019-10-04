import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import {ColorType} from '../../../utils/editor/color-type';
@Component({
    tag: 'app-deck-or-slide',
    styleUrl: 'app-deck-or-slide.scss'
})
export class AppDeckOrSlide {

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    qrCode: boolean = false;

    @Event()
    applyTo: EventEmitter<ColorType>;

    private selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyTo.emit($event.detail.value);
        }
    }

    render() {
        if (!this.deckOrSlide) {
            return undefined;
        } else{
            return <ion-segment mode="md" class="ion-padding-bottom" onIonChange={($event: CustomEvent) => this.selectApplyToAllDeck($event)}>
                {this.renderQRCode()}
                <ion-segment-button value={ColorType.SLIDE} checked={!this.qrCode} mode="md">
                    <ion-label>Slide</ion-label>
                </ion-segment-button>
                <ion-segment-button value={ColorType.DECK} mode="md">
                    <ion-label>Deck</ion-label>
                </ion-segment-button>
            </ion-segment>
        }
    }

    private renderQRCode() {
        if (this.qrCode) {
            return <ion-segment-button value={ColorType.QR_CODE} checked={this.qrCode} mode="md">
                <ion-label>QR code</ion-label>
            </ion-segment-button>
        } else {
            return undefined;
        }
    }
}
