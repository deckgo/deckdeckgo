import { Component, Method, Prop, h, Watch, Event, EventEmitter, Host } from '@stencil/core';
import { TweenMax, Power3 } from 'gsap';

import { DeckdeckgoSlide } from '../deckdeckgo-slide';

@Component({
  tag: 'deckgo-slide-countdown',
  styleUrl: 'deckdeckgo-slide-countdown.scss',
  shadow: true
})
export class DeckdeckgoSlideCountdown implements DeckdeckgoSlide {

  @Event()
  slideDidLoad: EventEmitter<void>;

  /**
   * Hours input for the timer.
   */
  @Prop()
  public hours = 0;

  /**
   * Minutes input for the timer.
   */
  @Prop()
  public minutes = 0;

  /**
   * Seconds input for the timer.
   */
  @Prop()
  public seconds = 0;

  @Watch('hours')
  hoursChangeHandler(newValue: number, _oldValue: number) {

    this.hours = newValue;

    this.clearUp();

  }

  @Watch('minutes')
  minutesChangeHandler(newValue: number, _oldValue: number) {

    this.minutes = newValue;

    this.clearUp();

  }

  @Watch('seconds')
  secondsChangeHandler(newValue: number, _oldValue: number) {

    this.seconds = newValue;

    this.clearUp();

  }

  private mHours = 0;
  private mMinutes = 0;
  private mSeconds = 0;

  private mHoursElement: HTMLElement;
  private mMinutesElement: HTMLElement;
  private mSecondsElement: HTMLElement;

  private mTotalSeconds = 0;
  private mCountdownInterval = -1;

  public componentDidLoad(): void {

    this.slideDidLoad.emit();

  }

  public componentDidRender(): void {

    this.init();
    this.startCountdown();

  }

  public disconnectedCallback(): void {

    this.clearUp();

  }

  public render(): any {

    return <Host class={{ 'deckgo-slide-container': true }}>

      <div class="deckgo-slide">
        <slot name="title"></slot>
        <div class="deckgo-countdown-container">
          {this.renderCountdown()}
        </div>
      </div>

    </Host>

  }

  @Method()
  beforeSwipe(_enter: boolean): Promise<boolean> {

    return Promise.resolve(true);

  }

  @Method()
  afterSwipe(): Promise<void> {

    this.clearUp();

    return Promise.resolve();

  }

  @Method()
  lazyLoadContent(): Promise<void> {

    return Promise.resolve();

  }

  /**
   * @internal
   */
  private init(): void {

    this.mHours = this.hours;
    this.mMinutes = this.minutes;
    this.mSeconds = this.seconds;

    this.mTotalSeconds = ((this.mHours * 60 * 60) + (this.mMinutes * 60) + this.mSeconds);

    // Update time.
    // Hours.
    this.checkAndUpdateTime(this.mHours, TimeUnit.Hours);

    // Minutes.
    this.checkAndUpdateTime(this.mMinutes, TimeUnit.Minutes);

    // Seconds.
    this.checkAndUpdateTime(this.mSeconds, TimeUnit.Seconds);

  }

  /**
   * @internal
   */
  private clearUp() {

    if (this.mCountdownInterval > -1) {

      clearInterval(this.mCountdownInterval);

      this.mCountdownInterval = -1;

    }

  }

  /**
   * @internal
   */
  private startCountdown(): void {

    this.mCountdownInterval = setInterval(() => {

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

        // Update time.
        // Hours.
        this.checkAndUpdateTime(this.mHours, TimeUnit.Hours);

        // Minutes.
        this.checkAndUpdateTime(this.mMinutes, TimeUnit.Minutes);

        // Seconds.
        this.checkAndUpdateTime(this.mSeconds, TimeUnit.Seconds);

        --this.mTotalSeconds;

      } else {

        clearInterval(this.mCountdownInterval);
        this.mCountdownInterval = -1;

      }

    }, 1000);

  }

  /**
   * @internal
   */
  private checkAndUpdateTime(value: number, type: TimeUnit): void {

    const valueStr = value.toString();

    if (value >= 10) {

      this.animateTimeSlot(valueStr.charAt(0), valueStr.charAt(1), type);

    } else if (value >= 0) {

      this.animateTimeSlot('0', valueStr.charAt(0), type);

    }

  }

  /**
   * @internal
   */
  private animateTimeSlot(tensValue: string, unitValue: string, type: TimeUnit): void {

    switch (type) {

      case TimeUnit.Hours:
        this.animateFigure(this.mHoursElement, tensValue, unitValue);
        break;

      case TimeUnit.Minutes:
        this.animateFigure(this.mMinutesElement, tensValue, unitValue);
        break;

      case TimeUnit.Seconds:
        this.animateFigure(this.mSecondsElement, tensValue, unitValue);
        break;

    }

  }

  /**
   * @internal
   */
  private animateFigure(timeSlotElem: HTMLElement, tensValue: string, unitValue: string): void {

    const figureTensElem = timeSlotElem.querySelector('.figure.tens');
    const figureUnitElem = timeSlotElem.querySelector('.figure.unit');

    this.animateTopAndBottom(figureTensElem, tensValue);
    this.animateTopAndBottom(figureUnitElem, unitValue);

  }

  /**
   * @internal
   */
  private animateTopAndBottom(figureElem: Element, value: string): void {

    const topElem = figureElem.querySelector('.top');
    const bottomElem = figureElem.querySelector('.bottom');
    const topBackElem = figureElem.querySelector('.top-back');
    const bottomBackElem = figureElem.querySelector('.bottom-back');

    if (topElem.innerHTML === value) {
      return;
    }

    topBackElem.querySelector('span').innerHTML = value;
    bottomBackElem.querySelector('span').innerHTML = value;

    TweenMax.to(topElem, 0.8, {
      rotationX: '-180deg',
      transformPerspective: 300,
      ease: Power3.easeOut,
      onComplete: () => {

        topElem.innerHTML = value;

        bottomElem.innerHTML = value;

        TweenMax.set(topElem, { rotationX: 0 });

      }
    });

    TweenMax.to(topBackElem, 0.8, {
      rotationX: 0,
      transformPerspective: 300,
      ease: Power3.easeOut,
      clearProps: 'all'
    });

  }

  private renderCountdown(): any {

    return <div class="countdown">

      <div class="time-container hours" ref={(elem) => this.mHoursElement = elem}>

        <span class="count-title">Hours</span>

        <div class="figure hours tens">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>

        <div class="figure hours unit">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>
      </div>

      <div class="time-container min" ref={(elem) => this.mMinutesElement = elem}>

        <span class="count-title">Minutes</span>

        <div class="figure min tens">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>

        <div class="figure min unit">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>

      </div>

      <div class="time-container sec" ref={(elem) => this.mSecondsElement = elem}>

        <span class="count-title">Seconds</span>

        <div class="figure sec tens">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>

        <div class="figure sec unit">
          <span class="top">0</span>
          <span class="top-back">
            <span>0</span>
          </span>
          <span class="bottom">0</span>
          <span class="bottom-back">
            <span>0</span>
          </span>
        </div>

      </div>

    </div>

  }

}

/**
 * @internal
 */
enum TimeUnit {

  Hours = 'Hours',
  Minutes = 'Minutes',
  Seconds = 'Seconds'

}
