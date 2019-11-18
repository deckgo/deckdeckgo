import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State} from '@stencil/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime, filter, take} from 'rxjs/operators';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';
import {DeckdeckgoBarChartData, DeckdeckgoBarChartDataValue, DeckdeckgoPollQuestion} from '@deckdeckgo/types';

import '@deckdeckgo/charts';

import {CommunicationService} from '../../services/communication/communication.service';

@Component({
  tag: 'deckgo-slide-poll',
  styleUrl: 'deckdeckgo-slide-poll.scss',
  shadow: true
})
export class DeckdeckgoSlidePoll implements DeckdeckgoSlideResize {

  @Element() el: HTMLElement;

  @Event() slideDidLoad: EventEmitter<void>;

  @Prop({reflectToAttr: true}) link: string;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  @Prop({reflectToAttr: true}) imgSrc: string;
  @Prop({reflectToAttr: true}) imgAlt: string;

  @Prop() countAnswers: number = 5;

  private answerSlots: number[];

  @State()
  private chartWidth: number;

  @State()
  private chartHeight: number;

  @State()
  private chartData: DeckdeckgoBarChartData[];

  @State()
  private pollKey: string;

  private communicationService: CommunicationService = CommunicationService.getInstance();

  private answers = {};

  @State()
  private answeredOnce: boolean = false;

  private updateChart: Subject<void> = new Subject<void>();

  private subscription: Subscription;
  private updateChartSubscription: Subscription;

  componentWillLoad() {
    this.answerSlots = Array.from({length: this.countAnswers}, (_v, i) => i);

    this.communicationService.watchPollKey().pipe(filter((key: string) => key !== undefined), take(1)).subscribe(async (key: string) => {
      this.pollKey = key;

      await this.updateSlotHowToText();
    });

    this.subscription = this.communicationService.watchVote().subscribe((answer: string) => {
      this.answeredOnce = true;

      this.answers[answer]++;

      this.updateChart.next();
    });

    this.updateChartSubscription = this.updateChart.pipe(debounceTime(500)).subscribe(async () => {
      await this.updateChartData();
    });
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();

    this.chartData = await this.initChartData();
  }

  async componentDidUpdate() {
    const img: HTMLImageElement = this.el.shadowRoot.querySelector('img');

    if (img && this.imgSrc) {
      await this.lazyLoadContent();
    }
  }

  async componentDidUnload() {
    await this.communicationService.disconnect();

    if (this.subscription) {
      this.subscription.unsubscribe();
    }

    if (this.updateChartSubscription) {
      this.updateChartSubscription.unsubscribe();
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.init();

    const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-qrcode');

    if (element) {
      await (element as any).generate();
    }
  };

  private async init() {
    await this.initQRCodeSize();
    await this.initSizeAndDraw();
  }

  private initQRCodeSize(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-poll-qrcode');

      if (container) {
        const width: number = container.clientWidth;
        const height: number = container.clientHeight;

        const qrCode: HTMLElement = container.querySelector('deckgo-qrcode');

        const slotHowToHeight: number = await this.getSlotHowToHeight();

        const paddingBottom: number = await this.getSlideContainerPaddingBottom();

        if (qrCode && width > 0 && height > 0) {
          qrCode.style.setProperty('--deckgo-qrcode-size', width > height ? `calc(${height}px - ${slotHowToHeight}px - ${paddingBottom}px)` : `calc(${width}px - ${slotHowToHeight}px - ${paddingBottom}px)`);
        }
      }

      resolve();
    });
  }

  private initSizeAndDraw(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-poll-chart');

      if (container) {
        this.chartWidth = container.clientWidth - 128;
        this.chartHeight = this.chartWidth * 9 / 16;

        const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-bar-chart');

        if (element) {
          await (element as any).draw(this.chartWidth, this.chartHeight);
        }
      }

      resolve();
    });
  }

  private getSlideContainerPaddingBottom(): Promise<number> {
    return new Promise<number>((resolve) => {
      const slideContainer: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide');

      if (!slideContainer || !window) {
        resolve(0);
        return;
      }

      const css: CSSStyleDeclaration = window.getComputedStyle(slideContainer);

      resolve(css ? parseInt(css.paddingBottom) : 0);
    });
  }

  private getSlotHowToHeight(): Promise<number> {
    return new Promise<number>((resolve) => {
      const howToElement: HTMLElement = this.el.querySelector(':scope > [slot=\'how_to\']');

      if (!howToElement || !window) {
        resolve(0);
        return;
      }

      const css: CSSStyleDeclaration = window.getComputedStyle(howToElement);

      const marginTop: number = css ? parseInt(css.marginTop) : 0;
      const marginBottom: number = css ? parseInt(css.marginBottom) : 0;
      const paddingTop: number = css ? parseInt(css.paddingTop) : 0;
      const paddingBottom: number = css ? parseInt(css.paddingBottom) : 0;

      resolve(howToElement.offsetHeight + marginBottom + marginTop + paddingBottom + paddingTop);
    });
  }

  private initChartData(): Promise<DeckdeckgoBarChartData[]> {
    return new Promise<DeckdeckgoBarChartData[]>(async (resolve) => {
      if (this.countAnswers <= 0 || !this.answerSlots || this.answerSlots.length <= 0) {
        resolve(null);
        return;
      }

      const promises = [];
      Array.from(this.answerSlots).forEach((answer: number) => {

        this.answers[`answer-${answer + 1}`] = 0;

        promises.push(this.initChartDataBar(`answer-${answer + 1}`));
      });

      const bars: DeckdeckgoBarChartDataValue[] = await Promise.all(promises);

      if (!bars || bars.length <= 0) {
        resolve(null);
        return;
      }

      const question: HTMLElement = this.el.querySelector(`:scope > [slot=\'question\']`);

      resolve([{
        label: question ? question.innerHTML : 'Poll',
        values: bars
      }]);
    });
  }

  private updateChartData(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!this.chartData || this.chartData.length < 1) {
        resolve();
        return;
      }

      if (!this.answers) {
        resolve();
        return;
      }

      const keys: string[] = Object.keys(this.answers);

      if (!keys || keys.length <= 0) {
        resolve();
        return;
      }

      keys.forEach((key: string) => {
        const dataBar = this.chartData[0].values.find((value) => {
          return value.key === key;
        });

        dataBar.value = this.answers[key];
      });

      this.chartData = [...this.chartData];

      resolve();
    });
  }

  private initChartDataBar(answerSlotName: string): Promise<DeckdeckgoBarChartDataValue> {
    return new Promise<DeckdeckgoBarChartDataValue>((resolve) => {
      const element: HTMLElement = this.el.querySelector(`:scope > [slot=\'${answerSlotName}\']`);

      if (!element) {
        resolve(undefined);
        return;
      }

      resolve({
        key: answerSlotName,
        label: element.innerHTML,
        value: Math.floor((Math.random() * 10) + 1)
      });
    });
  }

  private async initPoll() {
    if (this.chartData && this.chartData.length >= 1) {
      await this.communicationService.connect(this.chartData[0] as DeckdeckgoPollQuestion);
    }
  }

  private updateSlotHowToText(): Promise<void> {
    return new Promise<void>((resolve) => {
      const howToElement: HTMLElement = this.el.querySelector(':scope > [slot=\'how_to\']');

      if (!howToElement || !howToElement.innerHTML || howToElement.innerHTML === undefined || howToElement.innerHTML.indexOf('{0}') === -1) {
        resolve();
        return;
      }

      const replaceWith: string = this.pollKey ? this.pollKey.toString().replace(/\B(?=(\d{2})+(?!\d))/g, ' ') : this.pollKey;

      howToElement.innerHTML = howToElement.innerHTML.replace(/\{0\}/g, replaceWith);

      resolve();
    });
  }

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true)
    });
  }

  @Method()
  afterSwipe(): Promise<void> {
    return afterSwipe();
  }

  @Method()
  lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const promises = [];
      promises.push(lazyLoadContent(this.el));
      promises.push(this.init());
      promises.push(this.initPoll());

      await Promise.all(promises);

      resolve();
    });
  }

  @Method()
  revealContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  hideContent(): Promise<void> {
    return Promise.resolve();
  }

  @Method()
  resizeContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.onResizeContent();

      resolve();
    });
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
      <style>{`
        ::slotted(*:not([slot="question"]):not([slot="how_to"]):nth-child(-n+${this.countAnswers + 1})) {
          display: none;
        }
      `}</style>
      <div class="deckgo-slide">
        <slot name="question"></slot>
        {this.renderAnswers()}
        {this.renderPoll()}
        <slot name="notes"></slot>
        <slot name="actions"></slot>
        <slot name="background"></slot>
      </div>
    </Host>;
  }

  private renderPoll() {
    if (!this.pollKey) {
      return undefined;
    }

    return <div class="deckgo-slide-poll">
      <div class="deckgo-slide-poll-qrcode">
        <deckgo-qrcode content={this.link}>
          {this.renderLogo()}
        </deckgo-qrcode>
        <p>
          <slot name="how_to"></slot>
        </p>
      </div>

      <div class="deckgo-slide-poll-chart">
        {this.renderChart()}
        {this.renderNoVotes()}
      </div>
    </div>;
  }

  private renderLogo() {
    if (this.imgSrc) {
      return <img slot="logo" data-src={this.imgSrc} alt={this.imgAlt}/>;
    } else {
      return undefined;
    }
  }

  private renderAnswers() {
    return (
      this.answerSlots.map((i: number) => {
        return <slot name={`answer-${i + 1}`}></slot>
      })
    );
  }

  private renderChart() {
    if (this.chartWidth <= 0 || this.chartHeight <= 0) {
      return undefined;
    }

    return <deckgo-bar-chart width={this.chartWidth} height={this.chartHeight} data={this.chartData}
                             animation={true} yAxis={false}></deckgo-bar-chart>
  }

  private renderNoVotes() {
    if (this.answeredOnce) {
      return undefined;
    }

    return <slot name="awaiting_votes"></slot>;
  }
}
