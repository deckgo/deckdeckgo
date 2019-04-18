import {Component, Element, Listen, State} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import {take} from 'rxjs/operators';

// Services
import {AccelerometerService} from '../../services/accelerometer/accelerometer.service';

@Component({
    tag: 'app-remote-settings',
    styleUrl: 'app-remote-settings.scss'
})
export class AppRemoteSettings {
    @Element() el: HTMLElement;

    @State()
    private accelerometerEnabled: boolean = false;

    @State()
    private accelerometerFrequency: number = 0;

    @State()
    private accelerometerSensibility: number = 0;

    @State()
    private accelerometerTakeUntil: number = 0;

    @State()
    private accelerometerDelay: number = 0;

    constructor(private accelerometerService: AccelerometerService) {
        this.accelerometerService = AccelerometerService.getInstance();
    }

    componentWillLoad() {
        this.accelerometerService.watchEnabled().pipe(take(1)).subscribe((enabled: boolean) => {
            this.accelerometerEnabled = enabled;
        });

        this.accelerometerFrequency = this.accelerometerService.frequency;
        this.accelerometerSensibility = this.accelerometerService.sensibility;
        this.accelerometerTakeUntil = this.accelerometerService.takeUntil;
        this.accelerometerDelay = this.accelerometerService.delay;
    }

    componentDidLoad() {
        history.pushState({modal: true}, null);
    }

    @Listen('window:popstate')
    async handleHardwareBackbutton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await this.accelerometerService.save();

        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private async toggleAccelerometerSupport() {
        try {
            this.accelerometerEnabled = await this.accelerometerService.toggle();
        } catch (err) {
            this.accelerometerEnabled = false;
        }
    }

    private updateAccelerometerFrequency(e: CustomEvent<RangeChangeEventDetail>) {
        if (e && e.detail && e.detail.value >= 0) {
            this.accelerometerService.frequency = (e.detail.value as number);
            this.accelerometerFrequency = this.accelerometerService.frequency;
        }
    }

    private updateAccelerometerSensibility(e: CustomEvent<RangeChangeEventDetail>) {
        if (e && e.detail && e.detail.value >= 0) {
            this.accelerometerService.sensibility = (e.detail.value as number);
            this.accelerometerSensibility = this.accelerometerService.sensibility;
        }
    }

    private updateAccelerometerTakeUntil(e: CustomEvent<RangeChangeEventDetail>) {
        if (e && e.detail && e.detail.value >= 0) {
            this.accelerometerService.takeUntil = (e.detail.value as number);
            this.accelerometerTakeUntil = this.accelerometerService.takeUntil;
        }
    }

    private updateAccelerometerDelay(e: CustomEvent<RangeChangeEventDetail>) {
        if (e && e.detail && e.detail.value >= 0) {
            this.accelerometerService.delay = (e.detail.value as number);
            this.accelerometerDelay = this.accelerometerService.delay;
        }
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
                        <ion-label>Use the force, Luke</ion-label>
                    </ion-list-header>

                    <p>
                        Enabling this feature will allow you to swipe your slides like a Jedi or like if you were playing tennis with your phone. To detect the direction, your phone's accelerometer will be used. Therefore understand that this is pretty experimental 😅
                    </p>

                    <ion-item>
                        <ion-label>Swipe like a Jedi</ion-label>
                        <ion-toggle slot="end" checked={this.accelerometerEnabled} onIonChange={() => this.toggleAccelerometerSupport()}></ion-toggle>
                    </ion-item>

                    <p style={{color: this.accelerometerEnabled ? 'inherit' : 'var(--ion-color-medium)'}}>
                        <ion-label>Accelerometer's frequency ({this.accelerometerFrequency} measures/seconds)</ion-label>
                    </p>

                    <ion-item>
                        <ion-range min={1} max={300} value={this.accelerometerFrequency} mode="md"
                                   disabled={!this.accelerometerEnabled} color="primary"
                                   onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerFrequency(e)}>
                        </ion-range>
                    </ion-item>

                    <p style={{color: this.accelerometerEnabled ? 'inherit' : 'var(--ion-color-medium)'}}>
                        <ion-label>Debounce (trigger swipe after {(this.accelerometerTakeUntil / this.accelerometerFrequency).toFixed(2)} seconds)</ion-label>
                    </p>

                    <ion-item>
                        <ion-range min={1} max={30} value={this.accelerometerTakeUntil} mode="md"
                                   disabled={!this.accelerometerEnabled} color="primary"
                                   onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerTakeUntil(e)}>
                        </ion-range>
                    </ion-item>

                    <p style={{color: this.accelerometerEnabled ? 'inherit' : 'var(--ion-color-medium)'}}>
                        <ion-label>Sensibility (detect acceleration above velocity {this.accelerometerSensibility})</ion-label>
                    </p>

                    <ion-item>
                        <ion-range min={1} max={30} value={this.accelerometerSensibility} mode="md"
                                   disabled={!this.accelerometerEnabled} color="primary"
                                   onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerSensibility(e)}>
                        </ion-range>
                    </ion-item>

                    <p style={{color: this.accelerometerEnabled ? 'inherit' : 'var(--ion-color-medium)'}}>
                        <ion-label>Delay (after swipe, detect again after {this.accelerometerDelay}ms)</ion-label>
                    </p>

                    <ion-item>
                        <ion-range min={100} max={2500} value={this.accelerometerDelay} mode="md"
                                   disabled={!this.accelerometerEnabled} color="primary"
                                   onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerDelay(e)}>
                        </ion-range>
                    </ion-item>
                </ion-list>
            </ion-content>
        ];
    }

}
