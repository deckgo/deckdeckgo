import {Component, Prop, State, Watch} from '@stencil/core';

import {formatDistanceStrict, addMilliseconds} from 'date-fns';

@Component({
    tag: 'app-stopwatch',
    styleUrl: 'app-stopwatch.scss',
    shadow: true
})
export class AppStopwatch {

    @Prop()
    length: number;

    @Prop()
    remaining: number;

    @State()
    private remainingProgression: number = 0;

    async componentWillLoad() {
        await this.calculateRemaining();
    }

    @Watch('remaining')
    @Watch('length')
    calculateRemaining(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.length) {
                this.remainingProgression = 0;
                resolve();
                return;
            }

            if (this.remaining === null) {
                this.remainingProgression = 100;
                resolve();
                return;
            }

            if (this.remaining === 0) {
                this.remainingProgression = 0;
                resolve();
                return;
            }

            this.remainingProgression = Math.round((this.remaining / this.length) * 100);

            resolve();
        });
    }


    // Nice circular percentage chart from the article of Sergio Pedercini
    // https://medium.com/@pppped/how-to-code-a-responsive-circular-percentage-chart-with-svg-and-css-3632f8cd7705
    render() {
        const ratio: string = '' + this.remainingProgression + ', 100';

        let stopwatchCircleClass: string = 'stopwatch-circle';
        if (this.remaining >= 30000 && this.remaining < 60000) {
            stopwatchCircleClass += ' stopwatch-circle-warn-one-minute';
        } else if (this.remaining < 30000 && this.remaining >= 0) {
            stopwatchCircleClass += ' stopwatch-circle-warn-ending';
        }

        return <svg viewBox="0 0 36 36" class="stopwatch-circular-chart">
            <path class="stopwatch-circle-bg"
                  d="M18 2.0845
            a 15.9155 15.9155 0 0 1 0 31.831
            a 15.9155 15.9155 0 0 1 0 -31.831"
            />
            <path class={stopwatchCircleClass}
                  stroke-dasharray={ratio}
                  d="M18 2.0845
        a 15.9155 15.9155 0 0 1 0 31.831
        a 15.9155 15.9155 0 0 1 0 -31.831"
            />
            {this.renderText()}
        </svg>
    }

    private renderText() {
        if (this.remaining >= 0) {
            const end: Date = addMilliseconds(new Date(), this.remaining);
            const distance: string = formatDistanceStrict(end, new Date());
            return <text x="18" y="19" className="stopwatch-remaining-time">{distance}</text>;
        } else {
            return undefined;
        }
    }
}
