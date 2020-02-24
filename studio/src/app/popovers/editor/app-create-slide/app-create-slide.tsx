import {Component, Element, Event, EventEmitter, h, JSX, State} from '@stencil/core';

import {interval, Subscription} from 'rxjs';
import {take} from 'rxjs/operators';

import {SlideAttributes, SlideChartType, SlideTemplate} from '../../../models/data/slide';

import {User} from '../../../models/data/user';
import {Deck} from '../../../models/data/deck';

import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';

import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';
import {UserService} from '../../../services/data/user/user.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
  tag: 'app-create-slide',
  styleUrl: 'app-create-slide.scss'
})
export class AppCreateSlide {
  @Element() el: HTMLElement;

  @State()
  private photoUrl: string;

  @State()
  private chartsCollapsed: boolean = true;

  private user: User;

  private userService: UserService;
  private anonymousService: AnonymousService;
  private deckEditorService: DeckEditorService;

  @Event() signIn: EventEmitter<void>;

  private timerSubscription: Subscription;

  constructor() {
    this.userService = UserService.getInstance();
    this.anonymousService = AnonymousService.getInstance();
    this.deckEditorService = DeckEditorService.getInstance();
  }

  async componentWillLoad() {
    this.userService
      .watch()
      .pipe(take(1))
      .subscribe(async (user: User) => {
        this.user = user;
        this.photoUrl =
          user && user.data && user.data.photo_url ? user.data.photo_url : 'https://pbs.twimg.com/profile_images/941274539979366400/bTKGkd-O_400x400.jpg';
      });
  }

  async componentDidLoad() {
    await this.lazyLoadContent();
    await this.drawChart();
    await this.updatePollChart();
  }

  componentDidUnload() {
    this.unsubscribeTimer();
  }

  private unsubscribeTimer() {
    if (this.timerSubscription) {
      this.timerSubscription.unsubscribe();
    }
  }

  private lazyLoadContent(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif.showcase');
      const slideAuthor: HTMLElement = this.el.querySelector('deckgo-slide-author.showcase');
      const slideQRCode: HTMLElement = this.el.querySelector('deckgo-slide-qrcode.showcase');
      const slidesChart: HTMLElement[] = Array.from(this.el.querySelectorAll('deckgo-slide-chart.showcase'));
      const slidesPoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

      const slides: HTMLElement[] = [slideGif, slideAuthor, slideQRCode, ...slidesChart, slidesPoll];

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
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide(template, deck, this.user);
    await this.closePopover(template, slide);
  }

  private async addSlideSplit(template: SlideTemplate, attributes: SlideAttributes = undefined) {
    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideSplit(attributes);
    await this.closePopover(template, slide);
  }

  private addSlideQRCode() {
    this.deckEditorService
      .watch()
      .pipe(take(1))
      .subscribe(async (deck: Deck) => {
        await this.addSlide(SlideTemplate.QRCODE, deck);
      });
  }

  // We need the data in the user account (like twitter, profile image etc.) to generate the author slide
  private async addRestrictedSlide(template: SlideTemplate) {
    const isAnonymous: boolean = await this.anonymousService.isAnonymous();

    if (isAnonymous) {
      this.signIn.emit();
      await this.closePopover(null);
      return;
    }

    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlide(template, null, this.user);
    await this.closePopover(template, slide);
  }

  // User will need an account to upload her/his data
  private async closePopoverRestricted(template: SlideTemplate, attributes: SlideAttributes) {
    const isAnonymous: boolean = await this.anonymousService.isAnonymous();

    if (isAnonymous) {
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
      attributes: attributes
    });
  }

  private async selectUnselectCharts() {
    this.chartsCollapsed = !this.chartsCollapsed;

    this.unsubscribeTimer();

    if (this.chartsCollapsed) {
      return;
    }

    this.timerSubscription = interval(2000).subscribe(async (val: number) => {
      const elements: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-slide-chart[animation]');

      if (elements) {
        for (const element of Array.from(elements)) {
          await (element as any).beforeSwipe(val % 2 === 0, true);
        }
      }
    });
  }

  render() {
    return [
      <ion-toolbar>
        <h2>Add a slide</h2>
        <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
          <ion-icon name="close"></ion-icon>
        </ion-router-link>
      </ion-toolbar>,
      <div class="container ion-margin-bottom ion-padding">
        <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.TITLE)}>
          <deckgo-slide-title class="showcase">
            <p slot="title">Title</p>
            <p slot="content">Content</p>
          </deckgo-slide-title>
        </div>
        <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate.CONTENT)}>
          <deckgo-slide-content class="showcase">
            <p slot="title">Title</p>
            <p slot="content">Content</p>
          </deckgo-slide-content>
        </div>
        <div class="item" custom-tappable onClick={() => this.addSlideSplit(SlideTemplate.SPLIT)}>
          <deckgo-slide-split class="showcase">
            <p slot="start">Content</p>
            <p slot="end">Content</p>
          </deckgo-slide-split>
        </div>
        <div class="item" custom-tappable onClick={() => this.addSlideSplit(SlideTemplate.SPLIT, {vertical: true})}>
          <deckgo-slide-split vertical={true} class="showcase">
            <p slot="start">Content</p>
            <p slot="end">Content</p>
          </deckgo-slide-split>
        </div>
        <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.GIF)}>
          <deckgo-slide-gif class="showcase" src={EnvironmentConfigService.getInstance().get('gifExampleSrc')} alt="Slide Gif">
            <p
              slot="header"
              style={{
                'font-size': 'var(--font-size-very-small)',
                padding: '2px',
                'border-radius': '4px'
              }}>
              Gif with header
            </p>
            <p
              slot="footer"
              style={{
                'font-size': 'var(--font-size-very-small)',
                padding: '2px',
                'border-radius': '4px'
              }}>
              and footer
            </p>
          </deckgo-slide-gif>
        </div>
        <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.YOUTUBE)}>
          <deckgo-slide-content class="showcase gif">
            <p slot="title">Youtube</p>
            <p slot="content">
              <ion-icon name="logo-youtube"></ion-icon>
            </p>
          </deckgo-slide-content>
        </div>

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
            src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
            <p slot="title">Chart</p>
          </deckgo-slide-chart>
        </div>

        {this.renderCharts()}

        <div class="item" custom-tappable onClick={() => this.addSlideQRCode()}>
          <deckgo-slide-qrcode
            class="showcase"
            content={EnvironmentConfigService.getInstance().get('deckdeckgo').appUrl}
            img-src={`${EnvironmentConfigService.getInstance().get('deckdeckgo').globalAssetsUrl}/img/deckdeckgo-logo.svg`}>
            <p slot="title">QR code</p>
          </deckgo-slide-qrcode>
        </div>
        <div class="item" custom-tappable onClick={() => this.addRestrictedSlide(SlideTemplate.AUTHOR)}>
          <deckgo-slide-author class="showcase" img-src={this.photoUrl} img-alt="Author">
            <p slot="title">Author</p>
            <p slot="author">About yourself</p>
            <p slot="social-link">Twitter</p>
            <p slot="social-link">LinkedIn</p>
            <p slot="social-link">Dev</p>
          </deckgo-slide-author>
        </div>

        <div class="item" custom-tappable onClick={() => this.closePopover(SlideTemplate.POLL)}>
          <deckgo-slide-poll
            class="showcase"
            poll-link={EnvironmentConfigService.getInstance().get('deckdeckgo').pollUrl}
            poll-server={EnvironmentConfigService.getInstance().get('deckdeckgo').pollServerUrl}
            count-answers={3}
            connectPollSocket={false}>
            <p slot="question">Poll: engage your audience</p>
            <p slot="answer-1">Yes</p>
            <p slot="answer-2">No</p>
            <p slot="answer-3">Don't know</p>
            <p slot="awaiting-votes">Live votes with mobile devices</p>
          </deckgo-slide-poll>
        </div>
        {this.renderShapes()}
      </div>
    ];
  }

  private renderCharts() {
    const chartsClass: string = `expand-charts ${this.chartsCollapsed ? 'collapsed' : ''}`;

    return (
      <div class={chartsClass}>
        <div class="arrow"></div>

        <div class="list">
          {/* Pie */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE})}>
            <deckgo-slide-chart
              class="showcase"
              type="pie"
              marginTop={0}
              marginBottom={0}
              marginLeft={0}
              marginRight={0}
              width={68}
              height={68}
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
              <p slot="title">Pie</p>
            </deckgo-slide-chart>
          </div>

          {/* Donut */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE, innerRadius: 100})}>
            <deckgo-slide-chart
              class="showcase"
              type="pie"
              marginTop={0}
              marginBottom={0}
              marginLeft={0}
              marginRight={0}
              width={68}
              height={68}
              inner-radius={16}
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
              <p slot="title">Donut</p>
            </deckgo-slide-chart>
          </div>

          {/* Animated Pie */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.PIE, animation: true})}>
            <deckgo-slide-chart
              class="showcase"
              type="pie"
              animation={true}
              marginTop={0}
              marginBottom={0}
              marginLeft={0}
              marginRight={0}
              width={68}
              height={68}
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv">
              <p slot="title">Pie comparison</p>
            </deckgo-slide-chart>
          </div>

          {/* Area */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE})}>
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
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-to-compare.csv">
              <p slot="title">Area</p>
            </deckgo-slide-chart>
          </div>

          {/* Sharp area */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, smooth: false})}>
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
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart.csv">
              <p slot="title">Sharp area</p>
            </deckgo-slide-chart>
          </div>

          {/* Lines */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, area: false})}>
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
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-chart-no-dates.csv">
              <p slot="title">Lines</p>
            </deckgo-slide-chart>
          </div>

          {/* Animated area */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.LINE, animation: true})}>
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
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-line-multiple.csv">
              <p slot="title">Line graph comparison</p>
            </deckgo-slide-chart>
          </div>

          {/* Bar */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR})}>
            <deckgo-slide-chart
              class="showcase"
              type="bar"
              marginTop={0}
              marginBottom={1}
              marginLeft={0}
              marginRight={0}
              width={88}
              height={68}
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-pie-chart.csv">
              <p slot="title">Bar</p>
            </deckgo-slide-chart>
          </div>

          {/* Grouped bars */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR})}>
            <deckgo-slide-chart
              class="showcase"
              type="bar"
              marginTop={0}
              marginBottom={1}
              marginLeft={0}
              marginRight={0}
              width={88}
              height={68}
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
              style={{
                '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
                '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
                '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'
              }}>
              <p slot="title">Grouped bars</p>
            </deckgo-slide-chart>
          </div>

          {/* Animation bars */}
          <div class="item" custom-tappable onClick={() => this.closePopoverRestricted(SlideTemplate.CHART, {type: SlideChartType.BAR, animation: true})}>
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
              src="https://raw.githubusercontent.com/deckgo/deckdeckgo/master/webcomponents/charts/showcase/data-bar-chart-to-compare.csv"
              style={{
                '--deckgo-chart-fill-color-1': 'var(--ion-color-primary)',
                '--deckgo-chart-fill-color-2': 'var(--ion-color-secondary)',
                '--deckgo-chart-fill-color-3': 'var(--ion-color-tertiary)'
              }}>
              <p slot="title">Bar comparison</p>
            </deckgo-slide-chart>
          </div>
        </div>
      </div>
    );
  }

  // TODO: Assets path

  private renderShapes() {
    return (
      <div class="item" custom-tappable onClick={() => this.addSlide(SlideTemplate['ASPECT-RATIO'])}>
        <deckgo-slide-aspect-ratio class="showcase" grid={true}>
          <deckgo-lazy-img svg-src="/assets/img/shapes/robot-solid.svg" aria-label="Robot" class="robot"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/desktop-solid.svg" aria-label="Desktop" class="desktop"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/arrow-right-solid.svg" aria-label="Arrow" class="arrow-start"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/cloud-solid.svg" aria-label="Cloud" class="cloud"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/arrow-right-solid.svg" aria-label="Arrow" class="arrow-end-top"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/arrow-right-solid.svg" aria-label="Arrow" class="arrow-end-bottom"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/database-solid.svg" aria-label="Database" class="database"></deckgo-lazy-img>
          <deckgo-lazy-img svg-src="/assets/img/shapes/envelope-solid.svg" aria-label="Envelope" class="envelope"></deckgo-lazy-img>
        </deckgo-slide-aspect-ratio>
      </div>
    );
  }
}
