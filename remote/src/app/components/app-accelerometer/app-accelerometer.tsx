import {Component, Prop, State} from '@stencil/core';

import {Subscription} from 'rxjs';

// Services
import {AccelerometerService} from '../../services/accelerometer/accelerometer.service';

@Component({
    tag: 'app-accelerometer',
    styleUrl: 'app-accelerometer.scss'
})
export class AppAccelerometer {

    @Prop()
    hide: boolean = false;

    @State()
    private enabled: boolean = false;

    @State()
    private started: boolean = true;

    private acceleratorEnabledSubscription: Subscription;
    private acceleratorPermissionGrantedSubscription: Subscription;

    constructor(private accelerometerService: AccelerometerService) {
        this.accelerometerService = AccelerometerService.getInstance();
    }

    componentDidLoad() {
        this.acceleratorEnabledSubscription = this.accelerometerService.watchEnabled().subscribe((enabled: boolean) => {
            this.enabled = enabled;
        });
    }

    componentDidUnload() {
        if (this.acceleratorEnabledSubscription) {
            this.acceleratorEnabledSubscription.unsubscribe();
        }

        if (this.acceleratorPermissionGrantedSubscription) {
            this.acceleratorPermissionGrantedSubscription.unsubscribe();
        }
    }

    private async startStop(e: UIEvent) {
        e.stopPropagation();

        try {
            if (this.started) {
                await this.accelerometerService.stop();
            } else {
                await this.accelerometerService.start();
            }

            this.started = !this.started;
        } catch (err) {
            // Shit happens
        }
    }

    render() {
        if (this.hide) {
            return undefined;
        }

        const color: string = this.started ? 'secondary' : 'medium';

        if (this.enabled) {
            return <ion-fab-button size="small" color={color} onClick={(e: UIEvent) => this.startStop(e)}>
                    <ion-icon name="code-working"></ion-icon>
                </ion-fab-button>;
        } else {
            return undefined;
        }
    }
}
