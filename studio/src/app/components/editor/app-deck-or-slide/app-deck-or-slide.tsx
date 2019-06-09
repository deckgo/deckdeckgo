import {Component, Event, EventEmitter, h, Prop} from '@stencil/core';


@Component({
    tag: 'app-deck-or-slide',
    styleUrl: 'app-deck-or-slide.scss'
})
export class AppDeckOrSlide {

    @Prop()
    deckOrSlide: boolean = false;

    @Event() private applyTo: EventEmitter<boolean>;

    private selectApplyToAllDeck($event: CustomEvent) {
        if ($event && $event.detail) {
            this.applyTo.emit($event.detail.value);
        }
    }

    render() {
        if (!this.deckOrSlide) {
            return undefined;
        } else{
            return [
                <ion-item-divider class="ion-padding-top"><ion-label>Apply change to</ion-label></ion-item-divider>,
                <ion-radio-group onIonChange={($event) => this.selectApplyToAllDeck($event)}>
                    <ion-item>
                        <ion-label>Just this slide</ion-label>
                        <ion-radio slot="start" value={false} checked mode="md"></ion-radio>
                    </ion-item>
                    <ion-item>
                        <ion-label>The all deck</ion-label>
                        <ion-radio slot="start" value={true} mode="md"></ion-radio>
                    </ion-item>
                </ion-radio-group>
            ]
        }
    }

}
