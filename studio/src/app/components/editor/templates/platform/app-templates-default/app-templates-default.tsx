import {Component, Element, Event, EventEmitter, Fragment, h, State} from '@stencil/core';

import userStore from '../../../../../stores/user.store';

import {SlideAttributes, SlideSplitType, SlideTemplate} from '../../../../../models/data/slide';
import {Template} from '../../../../../models/data/template';

import {InitTemplate} from '../../../../../utils/editor/create-slides.utils';

import {EnvironmentConfigService} from '../../../../../services/core/environment/environment-config.service';
import {EnvironmentDeckDeckGoConfig} from '../../../../../services/core/environment/environment-config';

import {AssetsService} from '../../../../../services/core/assets/assets.service';

@Component({
  tag: 'app-templates-default',
})
export class AppTemplatesDefault {
  @Element() el: HTMLElement;

  @State()
  private assets: Assets | undefined = undefined;

  @State()
  private navigatorOnline: boolean = navigator.onLine;

  private config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

  @Event()
  selectedTemplate: EventEmitter<{template: SlideTemplate | Template; attributes?: SlideAttributes}>;

  @Event()
  addSlideQRCode: EventEmitter<void>;

  @Event()
  selectCharts: EventEmitter<void>;

  @Event()
  addSlideAuthor: EventEmitter<void>;

  @Event()
  addSlideAspectRatio: EventEmitter<void>;

  @Event()
  composeTemplate: EventEmitter<InitTemplate>;

  async componentWillLoad() {
    this.assets = await AssetsService.getInstance().assets();
  }

  async componentDidLoad() {
    await this.lazyLoadContent();
    await this.drawChart();
    await this.updatePollChart();
  }

  private async lazyLoadContent() {
    const slideGif: HTMLElement = this.el.querySelector('deckgo-slide-gif.showcase');
    const slideAuthor: HTMLElement = this.el.querySelector('deckgo-slide-author.showcase');
    const slideQRCode: HTMLElement = this.el.querySelector('deckgo-slide-qrcode.showcase');
    const slidesChart: HTMLElement = this.el.querySelector('deckgo-slide-chart.showcase');
    const slidesPoll: HTMLElement = this.el.querySelector('deckgo-slide-poll.showcase');

    const slides: HTMLElement[] = this.navigatorOnline ? [slideGif, slideAuthor, slideQRCode, slidesChart, slidesPoll] : [slideQRCode, slidesChart, slidesPoll];

    if (!slides || slides.length <= 0) {
      return;
    }

    const promises = [];
    Array.from(slides).forEach((slide: HTMLElement) => {
      promises.push((slide as any).lazyLoadContent());
    });

    await Promise.all(promises);
  }

  private async drawChart() {
    const slideChart: HTMLDeckgoSlideChartElement = this.el.querySelector('deckgo-slide-chart.showcase');

    if (!slideChart) {
      return;
    }

    await slideChart.draw();
  }

  private async updatePollChart() {
    const slidePoll: HTMLDeckgoSlidePollElement = this.el.querySelector('deckgo-slide-poll.showcase');

    if (!slidePoll) {
      return;
    }

    await slidePoll.update();
  }

  render() {
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

        {this.renderPoll()}

        {this.renderGif()}
        {this.renderChart()}

        {this.renderQRCode()}
        {this.renderAuthor()}
      </Fragment>
    );
  }

  private renderTitle() {
    return <app-templates-title custom-tappable onClick={() => this.composeTemplate.emit({template: SlideTemplate.TITLE})}></app-templates-title>;
  }

  private renderContent() {
    return <app-templates-content custom-tappable onClick={() => this.composeTemplate.emit({template: SlideTemplate.CONTENT})}></app-templates-content>;
  }

  private renderSplit() {
    return <app-templates-split custom-tappable onClick={() => this.composeTemplate.emit({template: SlideTemplate.SPLIT})}></app-templates-split>;
  }

  private renderVertical() {
    return (
      <app-templates-split
        custom-tappable
        vertical={true}
        onClick={() => this.composeTemplate.emit({template: SlideTemplate.SPLIT, attributes: {vertical: true}})}></app-templates-split>
    );
  }

  private renderPoll() {
    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.POLL})}>
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
    );
  }

  private renderChart() {
    if (this.assets === undefined || !this.assets.chart) {
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectCharts.emit()}>
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

  private renderShapes() {
    return (
      <div class="item" custom-tappable onClick={() => this.addSlideAspectRatio.emit()}>
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

  private renderGif() {
    if (this.assets === undefined || !this.assets.gif || !this.assets.gif.exampleSrc) {
      return undefined;
    }

    if (!this.navigatorOnline) {
      // For the Gif template, we need to select a Gif in Tenor, which is not accessible offline
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.GIF})}>
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
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.YOUTUBE})}>
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
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.PLAYGROUND})}>
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
      <div class="item" custom-tappable onClick={() => this.addSlideAuthor.emit()}>
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
      <div class="item" custom-tappable onClick={() => this.addSlideQRCode.emit()}>
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
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.SPLIT, attributes: {type: SlideSplitType.DEMO}})}>
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
