import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';

import '@deckdeckgo/charts';
import {CommunicationService} from '../../services/communication/communication.service';
import {filter, take} from 'rxjs/operators';

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

  @Prop() answers: number = 5;

  private answerSlots: number[];

  @State()
  private chartWidth: number;

  @State()
  private chartHeight: number;

  @State()
  private chartData: HTMLDeckgoBarChartElement['data'];

  @State()
  private pollKey: string;

  private communicationService: CommunicationService = CommunicationService.getInstance();

  componentWillLoad() {
    this.answerSlots = Array.from({length: this.answers}, (_v, i) => i);

    this.communicationService.watchPollKey().pipe(filter((key: string) => key !== undefined), take(1)).subscribe((key: string) => {
      this.pollKey = key;
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
    await this.drawChart();
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

  private drawChart(): Promise<void> {
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

  private initChartData(): Promise<HTMLDeckgoBarChartElement['data']> {
    return new Promise<HTMLDeckgoBarChartElement["data"]>(async (resolve) => {
      if (this.answers <= 0 || !this.answerSlots || this.answerSlots.length <= 0) {
        resolve(null);
        return;
      }

      const promises = [];
      Array.from(this.answerSlots).forEach((answer: number) => {
        promises.push(this.initChartDataBar(`answer-${answer + 1}`));
      });

      const bars: any[] = await Promise.all(promises);

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

  private initChartDataBar(answerSlotName: string): Promise<any> {
    return new Promise<any>((resolve) => {
      const element: HTMLElement = this.el.querySelector(`:scope > [slot=\'${answerSlotName}\']`);

      if (!element) {
        resolve(undefined);
        return;
      }

      resolve({
        key: answerSlotName,
        title: element.innerHTML,
        value: 5
      });
    });
  }

  private async initPoll() {
    if (this.chartData && this.chartData.length >= 1) {
      await this.communicationService.connect(this.chartData[0]);
    }
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
        ::slotted(*:not([slot="question"]):not([slot="how_to"]):nth-child(-n+${this.answers + 1})) {
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
          <slot name="how_to"></slot> {this.pollKey.toString().replace(/\B(?=(\d{2})+(?!\d))/g, ' ')}
        </p>
      </div>

      <div class="deckgo-slide-poll-chart">
        {this.renderChart()}
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

}
