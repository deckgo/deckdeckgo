import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import {TargetElement} from '../../../utils/editor/target-element';

@Component({
    tag: 'app-select-target-element',
    styleUrl: 'app-select-target-element.scss'
})
export class AppSelectTargetElement {

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    qrCode: boolean = false;

    @Prop()
    chart: boolean = false;

    @Event()
    applyTo: EventEmitter<TargetElement>;

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
                {this.renderChart()}
                <ion-segment-button value={TargetElement.SLIDE} checked={!this.qrCode} mode="md">
                    <ion-label>Slide</ion-label>
                </ion-segment-button>
                <ion-segment-button value={TargetElement.DECK} mode="md">
                    <ion-label>Deck</ion-label>
                </ion-segment-button>
            </ion-segment>
        }
    }

    private renderQRCode() {
        if (this.qrCode) {
            return <ion-segment-button value={TargetElement.QR_CODE} checked={this.qrCode} mode="md">
                <ion-label>QR code</ion-label>
            </ion-segment-button>
        } else {
            return undefined;
        }
    }

    private renderChart() {
        if (this.chart) {
            return <ion-segment-button value={TargetElement.CHART} checked={this.chart} mode="md">
                <ion-label>Chart</ion-label>
            </ion-segment-button>
        } else {
            return undefined;
        }
    }
}
