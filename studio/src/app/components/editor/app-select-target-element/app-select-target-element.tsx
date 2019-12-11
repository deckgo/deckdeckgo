import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';

import {TargetElement} from '../../../utils/editor/target-element';

@Component({
    tag: 'app-select-target-element',
    styleUrl: 'app-select-target-element.scss'
})
export class AppSelectTargetElement {

    @Prop()
    slide: boolean = false;

    @Prop()
    qrCode: boolean = false;

    @Prop()
    chart: boolean = false;

    @Prop()
    code: boolean = false;

    // color is a reserved prop word
    @Prop()
    colorTarget: boolean = false;

    @Prop()
    background: boolean = false;

    @Prop()
    transition: boolean = false;

    @Event()
    applyTo: EventEmitter<TargetElement>;

    private selectApplyToAll($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyTo.emit($event.detail.value);
        }
    }

    render() {
        if (!this.colorTarget && !this.slide) {
            if (!this.code) {
                return undefined;
            }

            return <ion-segment mode="md" class="ion-padding-bottom" onIonChange={($event: CustomEvent) => this.selectApplyToAll($event)}>
                <ion-segment-button value={TargetElement.CODE} checked={true} mode="md">
                    <ion-label>Code</ion-label>
                </ion-segment-button>
                <ion-segment-button value={TargetElement.SECTION} mode="md">
                    <ion-label>Section</ion-label>
                </ion-segment-button>
            </ion-segment>
        } else {
            return <ion-segment mode="md" class="ion-padding-bottom" onIonChange={($event: CustomEvent) => this.selectApplyToAll($event)}>
                {this.renderQRCode()}
                {this.renderChart()}
                {this.renderSlide()}
                {this.renderColor()}
                {this.renderBackground()}
                {this.renderTransition()}
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

    private renderSlide() {
        if (this.slide) {
            return <ion-segment-button value={TargetElement.SLIDE} checked={!this.qrCode && !this.chart} mode="md">
                <ion-label>Slide</ion-label>
            </ion-segment-button>;
        } else {
            return undefined;
        }
    }

    private renderColor() {
        if (this.colorTarget) {
            return <ion-segment-button value={TargetElement.COLOR} mode="md" checked={true}>
                <ion-label>Colors</ion-label>
            </ion-segment-button>;
        } else {
            return undefined;
        }
    }

    private renderBackground() {
        if (this.background) {
            return <ion-segment-button value={TargetElement.BACKGROUND} mode="md">
                <ion-label>Background</ion-label>
            </ion-segment-button>;
        } else {
            return undefined;
        }
    }

    private renderTransition() {
        if (this.transition) {
            return <ion-segment-button value={TargetElement.TRANSITION} mode="md">
                <ion-label>Transition</ion-label>
            </ion-segment-button>;
        } else {
            return undefined;
        }
    }
}
