import {createStore} from '@stencil/store';

import {addMilliseconds, formatDistanceStrict} from 'date-fns';

export interface TimerInterval {
  timerProgression: number;
  timerRemaining: number;
  timerLength: number;
}

interface TimerStore {
  timer: TimerInterval | undefined;
  remainingTime: number;
  remainingText: string | undefined;
}

const {state, onChange, reset} = createStore({
  timer: undefined,
  remainingTime: 0,
  remainingText: undefined
} as TimerStore);

onChange('timer', (interval: TimerInterval | undefined) => {
  if (interval) {
    state.remainingTime = interval.timerRemaining;

    const end: Date = addMilliseconds(new Date(), interval.timerRemaining);
    state.remainingText = formatDistanceStrict(end, new Date());
  } else {
    state.remainingTime = 0;
    state.remainingText = formatDistanceStrict(new Date(), new Date());
  }
});

export default {state, onChange, reset};
