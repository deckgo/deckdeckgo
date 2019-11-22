import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State} from '@stencil/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

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

  @Prop() socketUrl: string;
  @Prop() socketPath: string = '/poll';

  @Prop() connectPollSocket: boolean = true;

  @Prop() pollLink: string;

  @Prop({reflectToAttr: true}) customActions: boolean = false;
  @Prop({reflectToAttr: true}) customBackground: boolean = false;

  private answerSlots: string[];

  @State()
  private chartWidth: number;

  @State()
  private chartHeight: number;

  @State()
  private chartData: DeckdeckgoBarChartData[];

  @State()
  private pollKey: string;

  private communicationService: CommunicationService = new CommunicationService();

  private answers = {};

  @State()
  private answeredOnce: boolean = false;

  private updateChart: Subject<void> = new Subject<void>();

  private pollKeySubscription: Subscription;
  private voteSubscription: Subscription;
  private updateChartSubscription: Subscription;

  async componentWillLoad() {
    await this.initAnswerSlots();

    this.voteSubscription = this.communicationService.watchVote().subscribe((answer: string) => {
      this.answeredOnce = true;

      this.answers[answer]++;

      this.updateChart.next();
    });

    this.updateChartSubscription = this.updateChart.pipe(debounceTime(500)).subscribe(async () => {
      await this.updateChartAnswersData();

      if (this.chartData && this.chartData.length >= 1) {
        const element: HTMLElement = this.el.shadowRoot.querySelector('deckgo-bar-chart');

        if (element) {
          await (element as any).updateCurrentBar(this.chartData[0].values);
        }
      }
    });
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.pollKeySubscription = this.communicationService.watchPollKey().subscribe(async (key: string) => {
      this.pollKey = key;

      await this.initHowTo();
    });

    this.slideDidLoad.emit();
  }

  async componentDidUnload() {
    await this.communicationService.disconnect();

    if (this.voteSubscription) {
      this.voteSubscription.unsubscribe();
    }

    if (this.updateChartSubscription) {
      this.updateChartSubscription.unsubscribe();
    }

    if (this.pollKeySubscription) {
      this.pollKeySubscription.unsubscribe();
    }
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    const qrCodeElement: HTMLElement = this.el.shadowRoot.querySelector('deckgo-qrcode');

    if (qrCodeElement) {
      await (qrCodeElement as any).generate();
    }

    await this.drawChart();
  };

  private async drawChart() {
    const chartElement: HTMLElement = this.el.shadowRoot.querySelector('deckgo-bar-chart');

    if (chartElement) {
      await (chartElement as any).draw(this.chartWidth, this.chartHeight);
    }
  }

  private async initSize() {
    await this.initQRCodeSize();
    await this.initChartSize();
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

  private initChartSize(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-poll-chart');

      if (container) {
        this.chartWidth = container.clientWidth * 0.75;
        this.chartHeight = this.chartWidth * 9 / 16;
      }

      resolve();
    });
  }

  private async initAnswerSlots() {
    const answers: string[] | undefined = await this.initAnswerSlotsList();

    this.answerSlots = answers ? [...answers] : undefined;
  }

  private initAnswerSlotsList(): Promise<string[] | undefined> {
    return new Promise<string[] | undefined>((resolve) => {
      const slots: NodeListOf<HTMLElement> = this.el.querySelectorAll(':scope > [slot^=\'answer\']');

      if (!slots || slots.length <= 0) {
        resolve(undefined);
        return;
      }

      const answers: string[] = Array.from(slots).map((slot: HTMLElement) => {
        return slot.getAttribute('slot');
      });

      resolve(answers);
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
      const howToElement: HTMLElement = this.el.querySelector(':scope > [slot=\'how-to\']');

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
      if (!this.answerSlots || this.answerSlots.length <= 0) {
        resolve(null);
        return;
      }

      const promises = [];
      Array.from(this.answerSlots).forEach((answer: string) => {
        this.answers[answer] = 0;
        promises.push(this.initChartDataBar(answer));
      });

      const bars: DeckdeckgoBarChartDataValue[] = await Promise.all(promises);

      if (!bars || bars.length <= 0) {
        resolve(null);
        return;
      }

      const activeBars: DeckdeckgoBarChartDataValue[] = bars.filter((value: DeckdeckgoBarChartDataValue) => {
        return value !== undefined;
      });

      const question: HTMLElement = this.el.querySelector(`:scope > [slot=\'question\']`);

      resolve([{
        label: question ? question.innerHTML : 'Poll',
        values: activeBars
      }]);
    });
  }

  private updateChartAnswersData(): Promise<void> {
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
    if (!this.connectPollSocket) {
      return;
    }

    await this.communicationService.disconnect();

    if (this.chartData && this.chartData.length >= 1) {
      await this.communicationService.connect(this.socketUrl, this.socketPath, this.chartData[0] as DeckdeckgoPollQuestion);
    }
  }

  private initHowTo(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const howToSlotElement: HTMLElement = this.el.querySelector(':scope > [slot=\'how-to\']');

      if (!howToSlotElement) {
        resolve();
        return;
      }

      const container: HTMLElement = this.el.shadowRoot.querySelector('div.deckgo-slide-poll-qrcode');

      if (!container) {
        resolve();
        return;
      }

      const howTo: HTMLElement = container.querySelector('.how-to');

      if (howTo) {
        container.removeChild(howTo);
      }

      const element: HTMLElement = await this.cloneHowTo(howToSlotElement);
      container.appendChild(element);

      resolve();
    });
  }

  private cloneHowTo(howToSlotElement: HTMLElement): Promise<HTMLElement> {
    return new Promise<HTMLElement>((resolve) => {
      const element: HTMLElement = howToSlotElement.cloneNode(true) as HTMLElement;
      element.removeAttribute('slot');
      element.classList.add('how-to');

      if (!element.innerHTML || element.innerHTML === undefined || element.innerHTML.indexOf('{0}') === -1) {
        resolve(element);
        return;
      }

      const replaceWith: string = this.pollKey ? this.pollKey.toString().replace(/\B(?=(\d{2})+(?!\d))/g, ' ') : '{0}';

      element.innerHTML = element.innerHTML.replace(/\{0\}/g, replaceWith);

      resolve(element);
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
      promises.push(this.initSize());
      promises.push(this.initDataAndPoll(true));

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

  @Method()
  async update() {
    // Poll in progress should not be updated
    if (this.answeredOnce) {
      return;
    }

    await this.initAnswerSlots();

    await this.initDataAndPoll(false);
  }

  private initDataAndPoll(once: boolean): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (once && this.chartData && this.chartData.length > 0) {
        resolve();
        return;
      }

      await this.initChartSize();

      this.chartData = await this.initChartData();

      this.chartData = this.chartData ? [...this.chartData] : undefined;

      await this.initPoll();

      resolve();
    });
  }

  @Method()
  async isAnswered() {
    return this.answeredOnce;
  }

  render() {
    return <Host class={{'deckgo-slide-container': true}}>
      <style>{`
        ::slotted([slot^=\'answer\']) {
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
    return <div class="deckgo-slide-poll">
      <div class="deckgo-slide-poll-qrcode">
        <deckgo-qrcode content={this.pollLink}>
        </deckgo-qrcode>
        <slot name="how-to"></slot>
      </div>

      <div class="deckgo-slide-poll-chart">
        {this.renderChart()}
        {this.renderNoVotes()}
      </div>
    </div>;
  }

  private renderAnswers() {
    return (
      this.answerSlots.map((answer: string) => {
        return <slot name={answer}></slot>
      })
    );
  }

  private renderChart() {
    if (this.chartWidth <= 0 || this.chartHeight <= 0) {
      return undefined;
    }

    if (!this.chartData || this.chartData.length <= 0) {
      return undefined;
    }

    return <deckgo-bar-chart width={this.chartWidth} height={this.chartHeight} data={this.chartData}
                             margin-top={0} margin-bottom={0} margin-left={0} margin-right={0}
                             animation={true} yAxis={false}></deckgo-bar-chart>
  }

  private renderNoVotes() {
    if (this.answeredOnce) {
      return undefined;
    }

    return <slot name="awaiting-votes"></slot>;
  }
}
