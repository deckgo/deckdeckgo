import {Component, Element, Event, EventEmitter, Fragment, h} from '@stencil/core';

import userStore from '../../../../../stores/user.store';
import assetsStore from '../../../../../stores/assets.store';
import i18n from '../../../../../stores/i18n.store';
import offlineStore from '../../../../../stores/offline.store';

import {SlideAttributes, SlideSplitType, SlideTemplate} from '../../../../../models/data/slide';
import {Template} from '../../../../../models/data/template';

import {InitTemplate} from '../../../../../utils/editor/create-slides.utils';
import {tenorEnabled} from '../../../../../utils/core/environment.utils';

import {EnvironmentConfigService} from '../../../../../services/environment/environment-config.service';

import {EnvironmentDeckDeckGoConfig} from '../../../../../types/core/environment-config';

import {AppTemplatesFixed} from '../app-templates-fixed/app-templates-fixed';
import {AppIcon} from '../../../../core/app-icon/app-icon';

@Component({
  tag: 'app-templates-default'
})
export class AppTemplatesDefault {
  @Element() el: HTMLElement;

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

  private tenorEnabled: boolean = tenorEnabled();

  async componentDidLoad() {
    await this.lazyLoadContent();
    await this.drawChart();
    await this.updatePollChart();
  }

  private async lazyLoadContent() {
    const slideGif: HTMLElement | null = this.el.querySelector('deckgo-slide-gif.showcase');
    const slideAuthor: HTMLElement | null = this.el.querySelector('deckgo-slide-author.showcase');
    const slideQRCode: HTMLElement | null = this.el.querySelector('deckgo-slide-qrcode.showcase');
    const slidesChart: HTMLElement | null = this.el.querySelector('deckgo-slide-chart.showcase');
    const slidesPoll: HTMLElement | null = this.el.querySelector('deckgo-slide-poll.showcase');

    const slides: HTMLElement[] = [slideGif, slideAuthor, slideQRCode, slidesChart, slidesPoll].filter((element: HTMLElement | null) => element !== null);

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

  private selectTemplate = async (template: InitTemplate) => {
    this.composeTemplate.emit(template);
  };

  render() {
    return (
      <Fragment>
        <AppTemplatesFixed selectTemplate={this.selectTemplate}></AppTemplatesFixed>

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

  private renderPoll() {
    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.POLL})}>
        <deckgo-slide-poll
          class="showcase"
          poll-link={EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').pollUrl}
          count-answers={3}
          connectPollSocket={false}>
          <p slot="question">{i18n.state.templates.poll}</p>
          <p slot="answer-1">{i18n.state.core.yes}</p>
          <p slot="answer-2">{i18n.state.core.no}</p>
          <p slot="answer-3">{i18n.state.templates.do_not_know}</p>
          <p slot="awaiting-votes">{i18n.state.templates.live_votes}</p>
        </deckgo-slide-poll>
      </div>
    );
  }

  private renderChart() {
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
          src={assetsStore.state.chart.lineCompareSrc}
          custom-loader={true}>
          <p slot="title">{i18n.state.templates.charts}</p>
        </deckgo-slide-chart>
      </div>
    );
  }

  private renderShapes() {
    return (
      <div class="item" custom-tappable onClick={() => this.addSlideAspectRatio.emit()}>
        <deckgo-slide-aspect-ratio class="showcase" grid={true}>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/robot-solid.svg`}
            aria-label={i18n.state.shapes.robot}
            class="robot"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/desktop-solid.svg`}
            aria-label={i18n.state.shapes.desktop}
            class="desktop"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`}
            aria-label={i18n.state.shapes.arrow}
            class="arrow-start"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/cloud-solid.svg`}
            aria-label={i18n.state.shapes.cloud}
            class="cloud"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`}
            aria-label={i18n.state.shapes.arrow}
            class="arrow-end-top"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/arrow-right-solid.svg`}
            aria-label={i18n.state.shapes.arrow}
            class="arrow-end-bottom"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/database-solid.svg`}
            aria-label={i18n.state.shapes.database}
            class="database"></deckgo-lazy-img>
          <deckgo-lazy-img
            svg-src={`${this.config.globalAssetsUrl}/img/shapes/envelope-solid.svg`}
            aria-label={i18n.state.shapes.envelope}
            class="envelope"></deckgo-lazy-img>
          <span>{i18n.state.templates.diagrams}</span>
        </deckgo-slide-aspect-ratio>
      </div>
    );
  }

  private renderGif() {
    if (!offlineStore.state.online) {
      // For the Gif template, we need to select a Gif in Tenor, which is not accessible offline
      return undefined;
    }

    if (!this.tenorEnabled) {
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.GIF})}>
        <deckgo-slide-gif class="showcase" src={assetsStore.state.gif.exampleSrc} alt={i18n.state.editor.gif}>
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
    if (!offlineStore.state.online) {
      // The youtube slide can't be use offline as we cannot browse youtube
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.YOUTUBE})}>
        <deckgo-slide-content class="showcase gif">
          <p slot="title">YouTube</p>
          <p slot="content">
            <AppIcon name="youtube" ariaLabel="" ariaHidden={true}></AppIcon>
          </p>
        </deckgo-slide-content>
      </div>
    );
  }

  private renderPlayground() {
    if (!offlineStore.state.online) {
      // The youtube slide can't be use offline as we cannot browse youtube
      return undefined;
    }

    return (
      <div class="item" custom-tappable onClick={() => this.selectedTemplate.emit({template: SlideTemplate.PLAYGROUND})}>
        <deckgo-slide-content class="showcase gif">
          <p slot="title">{i18n.state.templates.playground}</p>
          <app-playground-placeholder slot="content"></app-playground-placeholder>
        </deckgo-slide-content>
      </div>
    );
  }

  private renderAuthor() {
    if (!offlineStore.state.online) {
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
          <p slot="title">{i18n.state.templates.author}</p>
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
          content={EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').appUrl}
          img-src={`${EnvironmentConfigService.getInstance().get<EnvironmentDeckDeckGoConfig>('deckdeckgo').globalAssetsUrl}/img/deckdeckgo-logo.svg`}>
          <p slot="title">{i18n.state.templates.qr_code}</p>
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
            <ion-label>{i18n.state.templates.showcase}</ion-label>
          </div>
        </deckgo-slide-split>
      </div>
    );
  }
}
