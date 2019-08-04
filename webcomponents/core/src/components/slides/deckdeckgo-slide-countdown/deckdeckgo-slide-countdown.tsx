import {Component, Method, Prop, h, Event, EventEmitter, Host, State} from '@stencil/core';

import {DeckdeckgoSlide} from '../deckdeckgo-slide';

@Component({
  tag: 'deckgo-slide-countdown',
  styleUrl: 'deckdeckgo-slide-countdown.scss',
  shadow: true
})
export class DeckdeckgoSlideCountdown implements DeckdeckgoSlide {

  @Event()
  slideDidLoad: EventEmitter<void>;

  @Prop()
  hours = 0;

  @Prop()
  minutes = 0;

  @Prop()
  seconds = 0;

  @Prop()
  until: string;

  @State()
  private mHours = 0;

  @State()
  private mMinutes = 0;

  @State()
  private mSeconds = 0;

  private mTotalSeconds = 0;
  private mCountdownInterval = -1;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  async componentDidLoad() {
    await this.clearUp();

    await this.init();
    await this.startCountdown();

    this.slideDidLoad.emit();
  }

  async componentDidUnload() {
    this.clearUp();
  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {
    return Promise.resolve(true);
  }

  @Method()
  afterSwipe(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.clearUp();
      resolve();
    });
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  async start() {
    await this.startCountdown();
  }

  @Method()
  async stop() {
    await this.clearUp();
  }

  /**
   * @internal
   */
  private clearUp(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (this.mCountdownInterval > -1) {
        clearInterval(this.mCountdownInterval);

        this.mCountdownInterval = -1;
      }

      resolve();
    });
  }

  /**
   * @internal
   */
  private init(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.until && this.until !== undefined && this.until !== '') {
        const startAt: Date = new Date(this.until);
        const now: Date = new Date();

        if (startAt && startAt.getTime() > now.getTime()) {

          let diff: number = (startAt.getTime() - now.getTime()) / (60 * 60 * 1000);

          this.mHours = Math.floor(diff);

          diff = (diff % 1) * 60;

          this.mMinutes = Math.floor(diff);

          diff = (diff % 1) * 60;

          this.mSeconds = Math.floor(diff);

          this.mTotalSeconds = ((this.mHours * 60 * 60) + (this.mMinutes * 60) + this.mSeconds);

          resolve();
          return;
        }
      }

      this.mHours = this.hours;
      this.mMinutes = this.minutes;
      this.mSeconds = this.seconds;

      this.mTotalSeconds = ((this.mHours * 60 * 60) + (this.mMinutes * 60) + this.mSeconds);

      resolve();
    })
  }

  /**
   * @internal
   */
  private startCountdown(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.mCountdownInterval = setInterval(async () => {
        if (this.mTotalSeconds > 0) {

          --this.mSeconds;

          if (this.mMinutes >= 0 && this.mSeconds < 0) {

            this.mSeconds = 59;
            --this.mMinutes;

          }

          if (this.mHours >= 0 && this.mMinutes < 0) {

            this.mMinutes = 59;
            --this.mHours;

          }

          --this.mTotalSeconds;

        } else {

          clearInterval(this.mCountdownInterval);
          this.mCountdownInterval = -1;

        }

      }, 1000);

      resolve();
    });
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
      <div class="deckgo-slide">
        <slot name="title"></slot>
        <div class="deckgo-countdown-container">
          {this.renderCountdown('hours', this.mHours > 99 ? 99 : this.mHours)}
          {this.renderCountdown('minutes', this.mMinutes)}
          {this.renderCountdown('seconds', this.mSeconds)}
        </div>
        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </Host>
  }

  private renderCountdown(slotName: string, value: number) {
    return <div class="time-container">
      <slot name={slotName}></slot>

      <div class="figure-container">

        <div class="figure tens">
          <span>{`${value >= 10 ? Math.floor(value / 10) % 10 : 0}`}</span>
        </div>

        <div class="figure unit">
          <span>{`${value % 10}`}</span>
        </div>

      </div>

    </div>
  }

}
