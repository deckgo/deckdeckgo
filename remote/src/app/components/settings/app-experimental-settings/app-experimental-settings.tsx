import {Component, Element, State, h} from '@stencil/core';
import {RangeChangeEventDetail} from '@ionic/core';

import accStore from '../../../stores/accelerometer.store';

// Services
import {AccelerometerService} from '../../../services/accelerometer/accelerometer.service';

@Component({
  tag: 'app-experimental-settings'
})
export class AppExperimentalSettings {
  @Element() el: HTMLElement;

  @State()
  private accelerometerFrequency: number = 0;

  @State()
  private accelerometerSensibility: number = 0;

  @State()
  private accelerometerTakeUntil: number = 0;

  @State()
  private accelerometerDelay: number = 0;

  private accelerometerService: AccelerometerService;

  constructor() {
    this.accelerometerService = AccelerometerService.getInstance();
  }

  componentWillLoad() {
    this.accelerometerFrequency = this.accelerometerService.frequency;
    this.accelerometerSensibility = this.accelerometerService.sensibility;
    this.accelerometerTakeUntil = this.accelerometerService.takeUntil;
    this.accelerometerDelay = this.accelerometerService.delay;
  }

  private async toggleAccelerometerSupport() {
    let destroyListener;

    try {
      destroyListener = accStore.onChange('enable', async () => {
        await this.accelerometerService.start();

        destroyListener();
      });

      await this.accelerometerService.toggle();
    } catch (err) {
      accStore.state.enable = false;

      if (destroyListener) {
        destroyListener();
      }
    }
  }

  private updateAccelerometerFrequency(e: CustomEvent<RangeChangeEventDetail>) {
    if (e && e.detail && e.detail.value >= 0) {
      this.accelerometerService.frequency = e.detail.value as number;
      this.accelerometerFrequency = this.accelerometerService.frequency;
    }
  }

  private updateAccelerometerSensibility(e: CustomEvent<RangeChangeEventDetail>) {
    if (e && e.detail && e.detail.value >= 0) {
      this.accelerometerService.sensibility = e.detail.value as number;
      this.accelerometerSensibility = this.accelerometerService.sensibility;
    }
  }

  private updateAccelerometerTakeUntil(e: CustomEvent<RangeChangeEventDetail>) {
    if (e && e.detail && e.detail.value >= 0) {
      this.accelerometerService.takeUntil = e.detail.value as number;
      this.accelerometerTakeUntil = this.accelerometerService.takeUntil;
    }
  }

  private updateAccelerometerDelay(e: CustomEvent<RangeChangeEventDetail>) {
    if (e && e.detail && e.detail.value >= 0) {
      this.accelerometerService.delay = e.detail.value as number;
      this.accelerometerDelay = this.accelerometerService.delay;
    }
  }

  render() {
    return [
      <h1 class="ion-padding-top">Experimental settings</h1>,
      <ion-list class="ion-padding-top ion-padding-bottom">
        <p>
          Enabling this feature will allow you to swipe your slides like a Jedi or like if you were playing tennis with your phone. To
          detect the direction, your phone's accelerometer will be used. Therefore understand that this is pretty experimental 😅
        </p>

        <ion-item>
          <ion-label>Swipe like a Jedi</ion-label>
          <ion-toggle
            slot="end"
            color="switcher"
            checked={accStore.state.enable}
            onIonChange={() => this.toggleAccelerometerSupport()}></ion-toggle>
        </ion-item>

        <p style={{color: accStore.state.enable ? 'inherit' : 'var(--ion-color-medium)'}}>
          <ion-label>Accelerometer's frequency ({this.accelerometerFrequency} measures/seconds)</ion-label>
        </p>

        <ion-item>
          <ion-range
            min={1}
            max={300}
            value={this.accelerometerFrequency}
            mode="md"
            disabled={!accStore.state.enable}
            color="switcher"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerFrequency(e)}></ion-range>
        </ion-item>

        <p style={{color: accStore.state.enable ? 'inherit' : 'var(--ion-color-medium)'}}>
          <ion-label>
            Debounce (trigger swipe after {(this.accelerometerTakeUntil / this.accelerometerFrequency).toFixed(2)} seconds)
          </ion-label>
        </p>

        <ion-item>
          <ion-range
            min={1}
            max={30}
            value={this.accelerometerTakeUntil}
            mode="md"
            disabled={!accStore.state.enable}
            color="switcher"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerTakeUntil(e)}></ion-range>
        </ion-item>

        <p style={{color: accStore.state.enable ? 'inherit' : 'var(--ion-color-medium)'}}>
          <ion-label>Sensibility (detect acceleration above velocity {this.accelerometerSensibility})</ion-label>
        </p>

        <ion-item>
          <ion-range
            min={1}
            max={30}
            value={this.accelerometerSensibility}
            mode="md"
            disabled={!accStore.state.enable}
            color="switcher"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerSensibility(e)}></ion-range>
        </ion-item>

        <p style={{color: accStore.state.enable ? 'inherit' : 'var(--ion-color-medium)'}}>
          <ion-label>Delay (after swipe, detect again after {this.accelerometerDelay}ms)</ion-label>
        </p>

        <ion-item>
          <ion-range
            min={100}
            max={2500}
            value={this.accelerometerDelay}
            mode="md"
            disabled={!accStore.state.enable}
            color="switcher"
            onIonChange={(e: CustomEvent<RangeChangeEventDetail>) => this.updateAccelerometerDelay(e)}></ion-range>
        </ion-item>
      </ion-list>
    ];
  }
}
