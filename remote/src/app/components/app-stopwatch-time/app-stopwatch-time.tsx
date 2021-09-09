import {Component, h} from '@stencil/core';

import timerStore from '../../stores/timer.store';

@Component({
  tag: 'app-stopwatch-time',
  styleUrl: 'app-stopwatch-time.scss'
})
export class AppStopwatchTime {
  render() {
    let color: string = 'primary';
    if (timerStore.state.remainingTime >= 30000 && timerStore.state.remainingTime < 60000) {
      color = 'secondary';
    } else if (timerStore.state.remainingTime < 30000 && timerStore.state.remainingTime >= 0) {
      color = 'tertiary';
    }

    if (timerStore.state.remainingText) {
      return (
        <ion-chip color="danger" style={{background: `var(--ion-color-${color})`, color: 'var(--ion-color-light)'}}>
          <ion-label>{timerStore.state.remainingText}</ion-label>
        </ion-chip>
      );
    } else {
      return undefined;
    }
  }
}
