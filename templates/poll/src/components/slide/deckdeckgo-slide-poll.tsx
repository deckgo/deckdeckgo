import {Component, Element, Event, EventEmitter, Method, Prop, h, Host, State} from '@stencil/core';

import {debounce} from '@deckdeckgo/utils';
import {DeckdeckgoSlideResize, hideLazyLoadImages, afterSwipe, lazyLoadContent} from '@deckdeckgo/slide-utils';
import {DeckdeckgoBarChartData, DeckdeckgoBarChartDataValue, DeckdeckgoPoll} from '@deckdeckgo/types';

import {drawChart, initChartDataBar, initChartSize, updateCurrentBar} from '../../utils/deckdeckgo-slide-poll.chart.utils';
import {generateQRCode, initQRCodeSize} from '../../utils/deckdeckgo-slide-poll.qrcode.utils';
import {initAnswerSlotsList} from '../../utils/deckdeckgo-slide-poll.answer.utils';
import {initHowTo} from '../../utils/deckdeckgo-slide-poll.howto.utils';

import {CommunicationService} from '../../services/communication/communication.service';

/**
 * @slot question - A question
 * @slot answer-x - X possible answers to the question
 * @slot how-to - Explain how can attendees vote. Note also that if you provide a string 0 in the content of your slot how-to, the information will be automatically converted to the real key of your poll (the key your audience could use to reach it and vote).
 * @slot awaiting-votes - A message displayed until first vote
 * @slot notes - Some notes related to this slide
 * @slot actions - Custom actions for this slide
 * @slot background - A custom background for this slide
 * @slot header - A custom header for this slide
 * @slot footer - A custom footer for this slide
 */
@Component({
  tag: 'deckgo-slide-poll',
  styleUrl: 'deckdeckgo-slide-poll.scss',
  shadow: true,
})
export class DeckdeckgoSlidePoll implements DeckdeckgoSlideResize {
  @Element() el: HTMLElement;

  /**
   * Triggered when the slide is loaded
   */
  @Event() slideDidLoad: EventEmitter<void>;

  @Event()
  private pollUpdated: EventEmitter<void>;

  /**
   * The url of the socket (server) where the poll (chat room) is going to be created
   */
  @Prop({reflect: true}) socketUrl: string = 'https://api.deckdeckgo.com';
  /**
   * The path to reach the socket server
   */
  @Prop({reflect: true}) socketPath: string = '/poll';

  /**
   * In case you would not like that the template try to reach the socket server
   */
  @Prop() connectPollSocket: boolean = true;

  /**
   * The url which leads to the voting application respectively where your audience will be available to make their voice heard aka where they will be able to vote
   */
  @Prop({reflect: true}) pollLink: string = 'https://app.deckdeckgo.com/poll';

  /**
   * Per default the template will always try to create a new poll but if you set this value, it will try to retrieve an existing poll
   */
  @Prop({reflect: true, mutable: true})
  pollKey: string;

  private answerSlots: string[];

  @State()
  private chartWidth: number;

  @State()
  private chartHeight: number;

  @State()
  private chartData: DeckdeckgoBarChartData[];

  private communicationService: CommunicationService = new CommunicationService();

  private answers = {};

  @State()
  private answeredOnce: boolean = false;

  private readonly debounceUpdateChart: Function;
  private readonly debounceUpdatePoll: Function;

  /**
   * If you define a background for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customBackground: boolean = false;

  /**
   * If you provide actions for the all deck but, a specific one for this slide, set this option to true
   */
  @Prop({reflect: true})
  customActions: boolean = false;

  constructor() {
    this.debounceUpdateChart = debounce(this.updateChartCallback, 500);
    this.debounceUpdatePoll = debounce(this.updatePollCallback);
  }

  async componentWillLoad() {
    await this.initAnswerSlots();
  }

  async componentDidLoad() {
    await hideLazyLoadImages(this.el);

    this.initWindowResize();

    this.slideDidLoad.emit();
  }

  async disconnectedCallback() {
    await this.communicationService.disconnect(this.pollKey);
  }

  componentDidUpdate() {
    this.pollUpdated.emit();
  }

  private initWindowResize() {
    if (window) {
      window.addEventListener('resize', debounce(this.onResizeContent));
    }
  }

  private onResizeContent = async () => {
    await this.initSize();

    await generateQRCode(this.el);

    await drawChart(this.el, this.chartWidth, this.chartHeight);
  };

  private async initSize() {
    await initQRCodeSize(this.el);

    await this.initChartSize();
  }

  private async initChartSize() {
    const size: {width: number; height: number} = await initChartSize(this.el);

    if (size && size !== undefined) {
      this.chartWidth = size.width;
      this.chartHeight = size.height;
    }
  }

  private async initAnswerSlots() {
    const answers: string[] | undefined = await initAnswerSlotsList(this.el);

    this.answerSlots = answers ? [...answers] : undefined;
  }

  private initChartDataAndAnswers(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.answerSlots || this.answerSlots.length <= 0) {
        this.chartData = undefined;

        resolve();
        return;
      }

      const promises = [];
      Array.from(this.answerSlots).forEach((answer: string, i: number) => {
        this.answers[answer] = 0;
        promises.push(initChartDataBar(this.el, answer, i));
      });

      const bars: DeckdeckgoBarChartDataValue[] = await Promise.all(promises);

      if (!bars || bars.length <= 0) {
        this.chartData = undefined;

        resolve();
        return;
      }

      const activeBars: DeckdeckgoBarChartDataValue[] = bars.filter((value: DeckdeckgoBarChartDataValue) => {
        return value !== undefined;
      });

      const question: HTMLElement = this.el.querySelector(`:scope > [slot=\'question\']`);

      this.chartData = [
        {
          label: question ? question.innerHTML : 'Poll',
          values: activeBars,
        },
      ];

      this.chartData = [...this.chartData];

      resolve();
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
          return `answer-${value.key}` === key;
        });

        dataBar.value = this.answers[key];
      });

      resolve();
    });
  }

  private initAnswersData(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.answers = {};

      if (!this.chartData || this.chartData.length < 1) {
        resolve();
        return;
      }

      if (!this.chartData[0].values || this.chartData[0].values.length <= 0) {
        resolve();
        return;
      }

      this.chartData[0].values.forEach((value: DeckdeckgoBarChartDataValue) => {
        this.answers[`answer-${value.key}`] = value.value;
      });

      resolve();
    });
  }

  private async initPoll() {
    if (!this.connectPollSocket) {
      return;
    }

    await this.communicationService.disconnect(this.pollKey);

    if (this.chartData && this.chartData.length >= 1) {
      await this.communicationService.connect(
        this.socketUrl,
        this.socketPath,
        {
          label: this.chartData[0].label as string,
          values: this.chartData[0].values,
          answered: this.answeredOnce,
        },
        this.updatePollKeyCallback,
        this.updateVoteCallback
      );
    }
  }

  private async retrievePoll() {
    if (!this.connectPollSocket || !this.pollKey) {
      return;
    }

    await this.communicationService.disconnect(this.pollKey);

    await this.communicationService.retrieve(this.socketUrl, this.socketPath, this.pollKey, this.debounceUpdatePoll);
  }

  private async updatePoll() {
    if (!this.connectPollSocket) {
      return;
    }

    if (this.chartData && this.chartData.length >= 1) {
      await this.communicationService.update(
        {
          label: this.chartData[0].label as string,
          values: this.chartData[0].values,
          answered: this.answeredOnce,
        },
        this.pollKey
      );
    }
  }

  private updateChartCallback = async () => {
    await this.updateChartAnswersData();

    if (this.chartData && this.chartData.length >= 1) {
      await updateCurrentBar(this.el, this.chartData[0].values);

      await this.updatePoll();
    }
  };

  private updatePollKeyCallback = async (key: string) => {
    if (key) {
      this.pollKey = key;

      await initHowTo(this.el, this.pollKey);
    }
  };

  private updateVoteCallback = async (answer: string) => {
    this.answeredOnce = true;

    this.answers[`answer-${answer}`]++;

    this.debounceUpdateChart();
  };

  private updatePollCallback = async (poll: DeckdeckgoPoll) => {
    if (poll && poll.poll) {
      this.answeredOnce = poll.poll.answered;

      this.chartData = [];
      this.chartData.push({
        label: poll.poll.label,
        values: poll.poll.values as DeckdeckgoBarChartDataValue[],
      });

      await this.initAnswersData();

      if (!this.chartData || this.chartData.length < 1) {
        return;
      }

      await drawChart(this.el, this.chartWidth, this.chartHeight);

      await initHowTo(this.el, this.pollKey);
    }

    this.pollUpdated.emit();
  };

  @Method()
  beforeSwipe(_enter: boolean, _reveal: boolean): Promise<boolean> {
    return new Promise<boolean>((resolve) => {
      resolve(true);
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
      promises.push(this.initChartAndPoll());

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

    await this.updateChartAndPoll();
  }

  private initChartAndPoll(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.chartData && this.chartData.length > 0) {
        resolve();
        return;
      }

      await this.initChartSize();

      await this.initChartDataAndAnswers();

      if (this.pollKey) {
        await this.retrievePoll();
      } else {
        await this.initPoll();
      }

      resolve();
    });
  }

  private updateChartAndPoll(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.initChartSize();

      await this.initChartDataAndAnswers();

      if (this.pollKey) {
        await this.updatePoll();
      } else {
        await this.initPoll();
      }

      resolve();
    });
  }

  @Method()
  async isAnswered() {
    return this.answeredOnce;
  }

  render() {
    return (
      <Host class={{'deckgo-slide-container': true}}>
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
          <slot name="header"></slot>
          <slot name="footer"></slot>
        </div>
      </Host>
    );
  }

  private renderPoll() {
    let url: string = this.pollLink;

    if (!this.pollLink.match(/\/$/)) {
      url += '/';
    }

    if (this.pollKey) {
      url += this.pollKey;
    }

    return (
      <div class="deckgo-slide-poll">
        <div class="deckgo-slide-poll-qrcode">
          <deckgo-qrcode content={url}></deckgo-qrcode>
          <slot name="how-to"></slot>
        </div>

        <div class="deckgo-slide-poll-chart">
          {this.renderChart()}
          {this.renderNoVotes()}
        </div>
      </div>
    );
  }

  private renderAnswers() {
    return this.answerSlots.map((answer: string) => {
      return <slot name={answer}></slot>;
    });
  }

  private renderChart() {
    if (this.chartWidth <= 0 || this.chartHeight <= 0) {
      return undefined;
    }

    if (!this.chartData || this.chartData.length <= 0) {
      return undefined;
    }

    return (
      <deckgo-bar-chart
        width={this.chartWidth}
        height={this.chartHeight}
        data={this.chartData}
        margin-top={0}
        margin-bottom={0}
        margin-left={0}
        margin-right={0}
        animation={true}
        yAxis={false}></deckgo-bar-chart>
    );
  }

  private renderNoVotes() {
    if (this.answeredOnce) {
      return undefined;
    }

    return <slot name="awaiting-votes"></slot>;
  }
}
