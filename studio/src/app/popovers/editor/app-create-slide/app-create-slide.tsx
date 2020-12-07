import {Component, Element, Event, EventEmitter, Fragment, h, JSX, State} from '@stencil/core';

import {SegmentChangeEventDetail} from '@ionic/core';

import deckStore from '../../../stores/deck.store';
import authStore from '../../../stores/auth.store';
import userStore from '../../../stores/user.store';

import {SlideAttributes, SlideChartType, SlideSplitType, SlideTemplate} from '../../../models/data/slide';

import {Deck} from '../../../models/data/deck';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';
import {SlotType} from '../../../utils/editor/slot-type';

import {AssetsService} from '../../../services/core/assets/assets.service';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../../services/core/environment/environment-config';

enum ComposeTemplate {
  TITLE,
  CONTENT,
  SPLIT_HORIZONTAL,
  SPLIT_VERTICAL,
  CHART,
}

@Component({
  tag: 'app-create-slide',
  styleUrl: 'app-create-slide.scss',
})
export class AppCreateSlide {
  @Element() el: HTMLElement;

  @State()
  private assets: Assets | undefined = undefined;

  @State()
  private navigatorOnline: boolean = navigator.onLine;

  @State()
  private composeTemplate: ComposeTemplate | undefined = undefined;

  @State()
  private elements: SlotType[] | undefined = undefined;

  @State()
  private templatesCategory: 'default' | 'community' | 'user' = 'default';

  @Event() signIn: EventEmitter<void>;

  private timerInterval: NodeJS.Timeout;
  private timerCounter: number = 0;

  private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  async componentWillLoad() {
    this.assets = await AssetsService.getInstance().assets();
  }

  async componentDidLoad() {
    await this.lazyLoadContent();
    await this.drawChart();
    await this.updatePollChart();
  }

  async componentDidUpdate() {
    await this.lazyLoadAllCharts();
  }

  disconnectedCallback() {
    this.unsubscribeTimer();
  }

  private unsubscribeTimer() {
    if (this.timerInterval) {
      clearInterval(this.timerInterval);
    }
  }

  private lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif.showcase');
      const slideAuthor: HTMLElement = this.el.querySelector('deckgo-slide-author.showcase');
      const slideQRCode: HTMLElement = this.el.querySelector('deckgo-slide-qrcode.showcase');
      const slidesChart: HTMLElement = this.el.querySelector('deckgo-slide-chart.showcase');
      const slidesPoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

      const slides: HTMLElement[] = this.navigatorOnline
        ? [slideGif, slideAuthor, slideQRCode, slidesChart, slidesPoll]
        : [slideQRCode, slidesChart, slidesPoll];

      if (!slides || slides.length <= 0) {
        resolve();
        return;
      }

      const promises = [];
      Array.from(slides).forEach((slide: HTMLElement) => {
        promises.push((slide as any).lazyLoadContent());
      });

      await Promise.all(promises);

      resolve();
    });
  }

  private drawChart(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slideChart: HTMLElement = this.el.querySelector('deckgo-slide-chart.showcase');

      if (!slideChart) {
        resolve();
        return;
      }

      await (slideChart as any).draw();

      resolve();
    });
  }

  private updatePollChart(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slidePoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

      if (!slidePoll) {
        resolve();
        return;
      }

      await (slidePoll as any).update();

      resolve();
    });
  }

  private async addSlide(template: SlideTemplate, deck?: Deck) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({template: template, elements: this.elements}, deck, userStore.state.user);
    await this.closePopover(template, slide);
  }

  private async addSlideSplit(template: SlideTemplate, attributes: SlideAttributes = undefined) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideSplit(this.elements, attributes);
    await this.closePopover(template, slide);
  }

  private async addSlideQRCode() {
    await this.addSlide(SlideTemplate.QRCODE, deckStore.state.deck);
  }

  // We need the data in the user account (like twitter, profile image etc.) to generate the author slide
  private async addRestrictedSlide(template: SlideTemplate) {
    if (authStore.state.anonymous) {
      this.signIn.emit();
      await this.closePopover(null);
      return;
    }

    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide({template: template}, null, userStore.state.user);
    await this.closePopover(template, slide);
  }

  // User will need an account to upload her/his data
  private async closePopoverRestricted(template: SlideTemplate, attributes: SlideAttributes) {
    if (authStore.state.anonymous) {
      this.signIn.emit();
      await this.closePopover(null);
      return;
    }

    await this.closePopover(template, null, attributes);
  }

  private async closePopoverWithoutResults() {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
  }

  private async closePopover(template: SlideTemplate, slide?: JSX.IntrinsicElements, attributes?: SlideAttributes) {
    await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss({
      template: template,
      slide: slide,
      attributes: attributes,
    });
  }

  private async lazyLoadAllCharts() {
    const slidesCharts: HTMLElement[] = Array.from(this.el.querySelectorAll('deckgo-slide-chart.showcase'));

    if (!slidesCharts || slidesCharts.length <= 0) {
      return;
    }

    const promises = [];
    Array.from(slidesCharts).forEach((slide: HTMLElement) => {
      promises.push((slide as any).lazyLoadContent());
    });

    await Promise.all(promises);
  }

  private async selectUnselectCharts() {
    this.composeTemplate = ComposeTemplate.CHART;

    this.unsubscribeTimer();

    this.timerInterval = setInterval(async () => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-slide-chart[animation]');

      if (elements) {
        for (const element of Array.from(elements)) {
          await (element as any).beforeSwipe(this.timerCounter % 2 === 0, true);
        }
      }

      this.timerCounter++;
    }, 2000);
  }

  private async selectElement(slotType: SlotType | null) {
    if (this.elements === undefined && !slotType) {
      return;
    }

    if (this.elements === undefined) {
      this.elements = [slotType];
    } else {
      // We might just want only one element
      if (slotType) {
        this.elements.push(slotType);
        this.elements = [...this.elements];
      }

      // We've got all, or at least one, the elements, go we can create the slide
      if (this.composeTemplate === ComposeTemplate.SPLIT_VERTICAL) {
        await this.addSlideSplit(SlideTemplate.SPLIT, {vertical: true});
      } else if (this.composeTemplate === ComposeTemplate.SPLIT_HORIZONTAL) {
        await this.addSlideSplit(SlideTemplate.SPLIT);
      } else if (this.composeTemplate === ComposeTemplate.CONTENT) {
        await this.addSlide(SlideTemplate.CONTENT);
      } else {
        await this.addSlide(SlideTemplate.TITLE);
      }
    }
  }

  private backCompose() {
    if (this.elements === undefined) {
      this.composeTemplate = undefined;
    } else {
      this.elements = undefined;
    }
  }

  render() {
    return (
      <Fragment>
        <ion-toolbar>
          {this.renderToolbarTitle()}
          {this.renderToolbarAction()}
        </ion-toolbar>

        {this.renderTemplatesCategory()}

        <div class={`container ion-margin-bottom ${this.composeTemplate !== undefined && this.composeTemplate !== ComposeTemplate.CHART ? ' compose' : ''}`}>
          {this.renderTemplatesDefault()}
          {this.renderTemplatesCommunity()}
          {this.renderTemplatesUser()}
          {this.renderCompose()}
        </div>
      </Fragment>
    );
  }

  private renderTemplatesCategory() {
    return (
      <ion-segment
        mode="md"
        value={this.templatesCategory}
        color="dark"
        class="ion-padding-bottom"
        onIonChange={($event: CustomEvent<SegmentChangeEventDetail>) => (this.templatesCategory = $event?.detail?.value as 'default' | 'community' | 'user')}
        disabled={this.composeTemplate !== undefined}>
        <ion-segment-button mode="md" value="default">
          <ion-label>Default</ion-label>
        </ion-segment-button>

        <ion-segment-button mode="md" value="community">
          <ion-label>Community</ion-label>
        </ion-segment-button>

        <ion-segment-button mode="md" value="user">
          <ion-label>Yours</ion-label>
        </ion-segment-button>
      </ion-segment>
    );
  }

  private renderToolbarTitle() {
    if (this.composeTemplate == undefined) {
      return <h2>Add a new slide</h2>;
    }

    return <h2>{this.composeTemplate === ComposeTemplate.CHART ? 'Select a chart' : 'Compose your slide'}</h2>;
  }

  private renderToolbarAction() {
    if (this.composeTemplate == undefined) {
      return <app-close-menu slot="end" onClose={() => this.closePopoverWithoutResults()}></app-close-menu>;
    }

    return (
      <app-close-menu slot="start" onClose={() => this.backCompose()}>
        <ion-icon aria-label="Back to all slides" src="/assets/icons/ionicons/arrow-back.svg"></ion-icon>
      </app-close-menu>
    );
  }

  private renderTemplatesCommunity() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'community') {
      return undefined;
    }

    return <app-templates-community onNavigateTemplates={() => this.closePopoverWithoutResults()}></app-templates-community>;
  }

  private renderTemplatesUser() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'user') {
      return undefined;
    }

    return <app-templates-user onNavigateTemplates={() => this.closePopoverWithoutResults()}></app-templates-user>;
  }

  private renderTemplatesDefault() {
    if (this.composeTemplate !== undefined) {
      return undefined;
    }

    if (this.templatesCategory !== 'default') {
      return undefined;
    }

    return (
      <Fragment>
        {this.renderTitle()}
        {this.renderContent()}

        {this.renderSplit()}
        {this.renderVertical()}

        {this.renderDemo()}
        {this.renderPlayground()}

        {this.renderYoutube()}

        {this.renderShapes()}

        <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.POLL)}>
          <deckgo-slide-poll
            class="showcase"
            poll-link={EnvironmentConfigService.getInstance().get('deckdeckgo').pollUrl}
            poll-server={EnvironmentConfigService.getInstance().get('deckdeckgo').pollServerUrl}
            count-answers={3}
            connectPollSocket={false}>
            <p slot="question">Engage Your Audience / Poll</p>
            <p slot="answer-1">Yes</p>
            <p slot="answer-2">No</p>
            <p slot="answer-3">Don't know</p>
            <p slot="awaiting-votes">Live Votes With Mobile Devices</p>
          </deckgo-slide-poll>
        </div>

        {this.renderGif()}
        {this.renderChart()}

        {this.renderQRCode()}
        {this.renderAuthor()}
      </Fragment>
    );
  }

  private renderCompose() {
    if (this.composeTemplate === undefined) {
      return undefined;
    }

    if (this.composeTemplate === ComposeTemplate.CHART) {
      return this.renderCharts();
    }

    return [this.renderComposeSlide(), this.renderSlotType()];
  }

  private renderComposeSlide() {
    if (this.composeTemplate === ComposeTemplate.CONTENT) {
      return this.renderContent();
    } else if (this.composeTemplate === ComposeTemplate.SPLIT_HORIZONTAL) {
      return this.renderSplit();
    } else if (this.composeTemplate === ComposeTemplate.SPLIT_VERTICAL) {
      return this.renderVertical();
    } else {
      return this.renderTitle();
    }
  }

  private renderSlotType() {
    const skip: boolean = this.elements !== undefined && (this.composeTemplate === ComposeTemplate.CONTENT || this.composeTemplate === ComposeTemplate.TITLE);

    return <app-slot-type skip={skip} onSelectType={($event: CustomEvent<SlotType>) => this.selectElement($event.detail)}></app-slot-type>;
  }

  private renderChart() {
    if (this.assets === undefined || !this.assets.chart) {
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectUnselectCharts()}>
        <deckgo-slide-chart
          class="showcase"
          type="line"
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          marginTop={0}
          marginBottom={0}
          marginLeft={0}
          marginRight={0}
          width={204}
          height={68}
          src={this.assets.chart.lineCompareSrc}
          custom-loader={true}>
          <p slot="title">Charts</p>
        </deckgo-slide-chart>
      </div>
    );
  }

  private renderCharts() {
    return [
      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE})}>
        {/* Pie */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          src={this.assets.chart.pieSrc}
          custom-loader={true}>
          <p slot="title">Pie</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE, innerRadius: 100})}>
        {/* Donut */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          inner-radius={16}
          src={this.assets.chart.pieSrc}
          custom-loader={true}>
          <p slot="title">Donut</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE, animation: true})}>
        {/* Animated Pie */}
        <deckgo-slide-chart
          class="showcase"
          type="pie"
          animation={true}
          marginTop={8}
          marginBottom={8}
          marginLeft={8}
          marginRight={8}
          width={68}
          height={68}
          src={this.assets.chart.barCompareSrc}
          custom-loader={true}>
          <p slot="title">Pie comparison</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE})}>
        {/* Area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          src={this.assets.chart.lineCompareSrc}
          custom-loader={true}>
          <p slot="title">Area</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, smooth: false})}>
        {/* Sharp area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          smooth={'false'}
          src={this.assets.chart.lineSrc}
          custom-loader={true}>
          <p slot="title">Sharp area</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, area: false})}>
        {/* Lines */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          area={'false'}
          src={this.assets.chart.lineNoDatesSrc}
          custom-loader={true}>
          <p slot="title">Lines</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, animation: true})}>
        {/* Animated area */}
        <deckgo-slide-chart
          class="showcase"
          type="line"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={53}
          y-axis-domain="extent"
          date-pattern="dd.MM.yyyy"
          animation={true}
          src={this.assets.chart.lineMultipleSrc}
          custom-loader={true}>
          <p slot="title">Line graph comparison</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR})}>
        {/* Bar */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          src={this.assets.chart.pieSrc}
          custom-loader={true}>
          <p slot="title">Bar</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR})}>
        {/* Grouped bars */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          src={this.assets.chart.barCompareSrc}
          style={{
            '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
            '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)',
          }}
          custom-loader={true}>
          <p slot="title">Grouped bars</p>
        </deckgo-slide-chart>
      </div>,

      <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR, animation: true})}>
        {/* Animation bars */}
        <deckgo-slide-chart
          class="showcase"
          type="bar"
          marginTop={0}
          marginBottom={1}
          marginLeft={0}
          marginRight={0}
          width={88}
          height={68}
          animation={true}
          src={this.assets.chart.barCompareSrc}
          style={{
            '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
            '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
            '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)',
          }}
          custom-loader={true}>
          <p slot="title">Bar comparison</p>
        </deckgo-slide-chart>
      </div>,
    ];
  }

  private renderShapes() {
    return (
      <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate['ASPECT-RATIO'])}>
        <deckgo-slide-aspect-ratio class="showcase" grid={true}>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/robot-solid.svg`} aria-label="Robot" class="robot"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/desktop-solid.svg`} aria-label="Desktop" class="desktop"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`} aria-label="Arrow" class="arrow-start"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/cloud-solid.svg`} aria-label="Cloud" class="cloud"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`}
            aria-label="Arrow"
            class="arrow-end-top"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`}
            aria-label="Arrow"
            class="arrow-end-bottom"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/database-solid.svg`} aria-label="Database" class="database"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src={`${this.config.globalAssetsUrl}/img/shapes/envelope-solid.svg`} aria-label="Envelope" class="envelope"></deckgo-lazy-img>
          <span>Diagrams</span>
        </deckgo-slide-aspect-ratio>
      </div>
    );
  }

  private renderTitle() {
    const classTitle = this.composeTemplate === ComposeTemplate.TITLE && this.elements === undefined ? 'highlight' : undefined;
    const classContent = this.composeTemplate === ComposeTemplate.TITLE && this.elements !== undefined ? 'highlight' : undefined;

    return (
      <div class="item" custom-tappable onClick={() => (this.composeTemplate = ComposeTemplate.TITLE)}>
        <deckgo-slide-title class="showcase">
          <p slot="title">
            <ion-skeleton-text style={{width: '60%'}} class={classTitle}></ion-skeleton-text>
          </p>
          <p slot="content">
            <ion-skeleton-text style={{width: '80%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classContent}></ion-skeleton-text>
          </p>
        </deckgo-slide-title>
      </div>
    );
  }

  private renderContent() {
    const classTitle = this.composeTemplate === ComposeTemplate.CONTENT && this.elements === undefined ? 'highlight' : undefined;
    const classContent = this.composeTemplate === ComposeTemplate.CONTENT && this.elements !== undefined ? 'highlight' : undefined;

    return (
      <div class="item" custom-tappable onClick={() => (this.composeTemplate = ComposeTemplate.CONTENT)}>
        <deckgo-slide-content class="showcase">
          <p slot="title">
            <ion-skeleton-text style={{width: '60%'}} class={classTitle}></ion-skeleton-text>
          </p>
          <p slot="content">
            <ion-skeleton-text style={{width: '80%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '82%'}} class={classContent}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '64%'}} class={classContent}></ion-skeleton-text>
          </p>
        </deckgo-slide-content>
      </div>
    );
  }

  private renderSplit() {
    const classStart = this.composeTemplate === ComposeTemplate.SPLIT_HORIZONTAL && this.elements === undefined ? 'highlight' : undefined;
    const classEnd = this.composeTemplate === ComposeTemplate.SPLIT_HORIZONTAL && this.elements !== undefined ? 'highlight' : undefined;

    return (
      <div class="item" custom-tappable onClick={() => (this.composeTemplate = ComposeTemplate.SPLIT_HORIZONTAL)}>
        <deckgo-slide-split class="showcase">
          <p slot="start">
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
          </p>
          <p slot="end">
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
          </p>
        </deckgo-slide-split>
      </div>
    );
  }

  private renderVertical() {
    const classStart = this.composeTemplate === ComposeTemplate.SPLIT_VERTICAL && this.elements === undefined ? 'highlight' : undefined;
    const classEnd = this.composeTemplate === ComposeTemplate.SPLIT_VERTICAL && this.elements !== undefined ? 'highlight' : undefined;

    return (
      <div class="item" custom-tappable onClick={() => (this.composeTemplate = ComposeTemplate.SPLIT_VERTICAL)}>
        <deckgo-slide-split vertical={true} class="showcase">
          <p slot="start">
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classStart}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classStart}></ion-skeleton-text>
          </p>
          <p slot="end">
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}} class={classEnd}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}} class={classEnd}></ion-skeleton-text>
          </p>
        </deckgo-slide-split>
      </div>
    );
  }

  private renderGif() {
    if (this.assets === undefined || !this.assets.gif || !this.assets.gif.exampleSrc) {
      return undefined;
    }

    if (!this.navigatorOnline) {
      // For the Gif template, we need to select a Gif in Tenor, which is not accessible offline
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.GIF)}>
        <deckgo-slide-gif class="showcase" src={this.assets.gif.exampleSrc} alt="Slide Gif">
          <p slot="top">
            <ion-skeleton-text style={{width: '60%'}}></ion-skeleton-text>
          </p>
          <p slot="bottom">
            <ion-skeleton-text style={{width: '40%'}}></ion-skeleton-text>
          </p>
        </deckgo-slide-gif>
      </div>
    );
  }

  private renderYoutube() {
    if (!this.navigatorOnline) {
      // The youtube slide can't be use offline as we cannot browse youtube
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.YOUTUBE)}>
        <deckgo-slide-content class="showcase gif">
          <p slot="title">YouTube</p>
          <p slot="content">
            <ion-icon name="logo-youtube"></ion-icon>
          </p>
        </deckgo-slide-content>
      </div>
    );
  }

  private renderPlayground() {
    if (!this.navigatorOnline) {
      // The youtube slide can't be use offline as we cannot browse youtube
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.PLAYGROUND)}>
        <deckgo-slide-content class="showcase gif">
          <p slot="title">Playground</p>
          <app-playground-placeholder slot="content"></app-playground-placeholder>
        </deckgo-slide-content>
      </div>
    );
  }

  private renderAuthor() {
    if (!this.navigatorOnline) {
      // The author slide need the user data to be added which we don't have offline
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.addRestrictedSlide(SlideTemplate.AUTHOR)}>
        <deckgo-slide-author
          class="showcase"
          img-src={
            userStore.state.user && userStore.state.user.data && userStore.state.user.data.photo_url
              ? userStore.state.user.data.photo_url
              : 'https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg'
          }
          img-alt="Author">
          <p slot="title">Author</p>
          <p slot="author">
            <ion-skeleton-text style={{width: '80%'}}></ion-skeleton-text>
          </p>
          <p slot="social-link">Twitter</p>
          <p slot="social-link">LinkedIn</p>
          <p slot="social-link">Dev</p>
        </deckgo-slide-author>
      </div>
    );
  }

  private renderQRCode() {
    return (
      <div class="item" custom-tappable onClick={() => this.addSlideQRCode()}>
        <deckgo-slide-qrcode
          class="showcase"
          content={EnvironmentConfigService.getInstance().get('deckdeckgo').appUrl}
          img-src={`${EnvironmentConfigService.getInstance().get('deckdeckgo').globalAssetsUrl}/img/deckdeckgo-logo.svg`}>
          <p slot="title">QR Code Deep Linking</p>
        </deckgo-slide-qrcode>
      </div>
    );
  }

  private renderDemo() {
    return (
      <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.SPLIT, null, {type: SlideSplitType.DEMO})}>
        <deckgo-slide-split class="showcase" type="demo">
          <p slot="start">
            <ion-skeleton-text style={{width: '80%'}}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '60%'}}></ion-skeleton-text>
            <ion-skeleton-text style={{width: '80%'}}></ion-skeleton-text>
          </p>
          <div slot="end">
            <deckgo-demo></deckgo-demo>
            <ion-label>Showcase Your Apps</ion-label>
          </div>
        </deckgo-slide-split>
      </div>
    );
  }
}
