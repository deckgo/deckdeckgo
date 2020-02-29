import {Component, Element, EventEmitter, h, JSX, Listen, Prop, State, Event} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {isIPad} from '@deckdeckgo/utils';

import {get, set} from 'idb-keyval';

import {SlideAttributes, SlideTemplate} from '../../../../../models/data/slide';

import {MoreAction} from '../../../../../utils/editor/more-action';

import {CreateSlidesUtils} from '../../../../../utils/editor/create-slides.utils';

import {AnonymousService} from '../../../../../services/editor/anonymous/anonymous.service';
import {OfflineService} from '../../../../../services/editor/offline/offline.service';

@Component({
  tag: 'app-actions-deck',
  shadow: false
})
export class AppActionsDeck {
  @Element() el: HTMLElement;

  @Prop()
  fullscreen: boolean = false;

  @Prop()
  slides: JSX.IntrinsicElements[] = [];

  @Prop()
  blockSlide: EventEmitter;

  @Prop()
  signIn: EventEmitter;

  @Prop()
  addSlide: EventEmitter;

  @Prop()
  animatePrevNextSlide: EventEmitter;

  @Prop()
  slideTo: EventEmitter;

  @Prop()
  toggleFullScreen: EventEmitter;

  @Prop()
  actionPublish: EventEmitter;

  @Prop()
  openShare: EventEmitter;

  @Prop()
  deckDidChange: EventEmitter;

  @Event()
  private selectDeck: EventEmitter<void>;

  @State()
  private fullscreenEnable: boolean = true;

  private anonymousService: AnonymousService;
  private offlineService: OfflineService;

  constructor() {
    this.anonymousService = AnonymousService.getInstance();
    this.offlineService = OfflineService.getInstance();
  }

  componentWillLoad() {
    this.fullscreenEnable = !isIPad();
  }

  async onActionOpenSlideAdd($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

    if (!couldAddSlide) {
      this.signIn.emit();
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-create-slide',
      event: $event.detail,
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu popover-menu-wide'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.template === SlideTemplate.GIF) {
          await this.openGifPicker();
        } else if (detail.data.template === SlideTemplate.YOUTUBE) {
          await this.openYoutube();
        } else if (detail.data.template === SlideTemplate.CHART) {
          await this.openChart(detail.data.attributes);
        } else if (detail.data.template === SlideTemplate.POLL) {
          await this.openPoll();
        }

        if (detail.data.slide) {
          this.addSlide.emit(detail.data.slide);
        }
      }
    });

    await popover.present();
  }

  private async openGifPicker() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-gif'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlideGif(detail.data);
    });

    await modal.present();
  }

  private async openYoutube() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-youtube'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlideYoutube(detail.data);
    });

    await modal.present();
  }

  private async openPoll() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-poll-options'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlidePoll(detail.data.question, detail.data.answers);

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openChart(attributes: SlideAttributes) {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-custom-data'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        await this.addSlideChart(detail.data, attributes);
      }
    });

    await modal.present();
  }

  private addSlideGif(gif: TenorGif): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!gif || !gif.media || gif.media.length <= 0 || !gif.media[0].gif || !gif.media[0].gif.url) {
        resolve();
        return;
      }

      const url: string = gif.media[0].gif.url;
      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideGif(url);

      this.addSlide.emit(slide);

      resolve();
    });
  }

  private addSlideYoutube(youtubeUrl: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
        resolve();
        return;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideYoutube(youtubeUrl);

      this.addSlide.emit(slide);

      resolve();
    });
  }

  private addSlidePoll(question: string, answers: string[]): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!question || question === undefined || question === '' || !answers || answers.length <= 0) {
        resolve();
        return;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlidePoll(question, answers);

      this.addSlide.emit(slide);

      resolve();
    });
  }

  private addSlideChart(dataFile: StorageFile, attributes: SlideAttributes): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!dataFile) {
        resolve();
        return;
      }

      const url: string = dataFile.downloadUrl;

      if (!url || url === undefined || url === '') {
        resolve();
        return;
      }

      if (!attributes) {
        attributes = {
          src: url
        };
      } else {
        attributes.src = url;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideChart(attributes);

      this.addSlide.emit(slide);

      resolve();
    });
  }

  @Listen('pagerClick', {target: 'document'})
  async onPagerClick() {
    await this.openSlideNavigate();
  }

  private async openSlideNavigate() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-slide-navigate'
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail.data >= 0) {
        this.slideTo.emit(detail.data);
      }
    });

    await modal.present();
  }

  private async openRemoteControl() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-remote-connect'
    });

    await modal.present();
  }

  private async openEmbed() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-embed'
    });

    await modal.present();
  }

  private async openMoreActions($event: UIEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-more-deck-actions',
      event: $event,
      mode: 'ios'
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail && detail.data) {
        if (detail.data.action === MoreAction.FULLSCREEN) {
          await this.toggleFullScreenMode();
        } else if (detail.data.action === MoreAction.JUMP_TO) {
          await this.openSlideNavigate();
        } else if (detail.data.action === MoreAction.REMOTE) {
          await this.openRemoteControl();
        } else if (detail.data.action === MoreAction.SHARE) {
          this.openShare.emit();
        } else if (detail.data.action === MoreAction.PUBLISH) {
          this.actionPublish.emit();
        } else if (detail.data.action === MoreAction.STYLE) {
          await this.openDeckStyle();
        } else if (detail.data.action === MoreAction.EMBED) {
          await this.openEmbed();
        }
      }
    });

    await popover.present();
  }

  private toggleFullScreenMode(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.toggleFullScreen.emit();

      await this.openFullscreenInfo();

      resolve();
    });
  }

  private async openFullscreenInfo() {
    const infoDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_fullscreen_info');

    if (infoDisplayedOnce) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-fullscreen-info',
      mode: 'ios',
      cssClass: 'info'
    });

    popover.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
      await set('deckdeckgo_display_fullscreen_info', true);
    });

    await popover.present();
  }

  async openDeckStyle() {
    this.selectDeck.emit();

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-deck-style',
      componentProps: {
        signIn: this.signIn,
        blockSlide: this.blockSlide,
        deckDidChange: this.deckDidChange
      },
      mode: 'md',
      cssClass: 'popover-menu popover-menu-wide'
    });

    await popover.present();
  }

  private async goOffline() {
    // TODO not here
    await this.offlineService.save();
  }

  render() {
    return (
      <ion-toolbar>
        <ion-buttons slot="start">
          <app-action-busy iconSrc="/assets/icons/ionicons/add.svg" onActionReady={($event: CustomEvent) => this.onActionOpenSlideAdd($event)}>
            <ion-label>Add slide</ion-label>
          </app-action-busy>

          <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(false)} color="primary" mode="md">
            <ion-icon name="arrow-back"></ion-icon>
            <ion-label>Previous</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(true)} color="primary" mode="md">
            <ion-icon name="arrow-forward"></ion-icon>
            <ion-label>Next</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={() => this.openSlideNavigate()} color="primary" class="wider-devices" mode="md">
            <ion-icon src="/assets/icons/ionicons/md-list.svg"></ion-icon>
            <ion-label>Slides</ion-label>
          </ion-tab-button>

          <app-action-busy iconName="brush-outline" class="wider-devices" onActionReady={() => this.openDeckStyle()}>
            <ion-label>Style</ion-label>
          </app-action-busy>

          {this.renderFullscreenButton()}

          <ion-tab-button onClick={() => this.openRemoteControl()} color="primary" class="wider-devices" mode="md">
            <ion-icon name="phone-portrait-outline"></ion-icon>
            <ion-label>Remote</ion-label>
          </ion-tab-button>

          <app-action-share class="wider-devices" onOpenEmbed={() => this.openEmbed()}></app-action-share>

          <ion-tab-button onClick={() => this.goOffline()} color="primary" class="wider-devices" mode="md">
            <ion-icon name="cloud-offline-outline"></ion-icon>
            <ion-label>Go offline</ion-label>
          </ion-tab-button>

          <ion-tab-button onClick={(e: UIEvent) => this.openMoreActions(e)} color="primary" class="small-devices" mode="md">
            <ion-icon src="/assets/icons/ionicons/ellipsis-vertical.svg"></ion-icon>
            <ion-label>More</ion-label>
          </ion-tab-button>
        </ion-buttons>

        <ion-buttons slot="end">
          <app-action-help></app-action-help>
        </ion-buttons>
      </ion-toolbar>
    );
  }

  private renderFullscreenButton() {
    if (this.fullscreenEnable) {
      return (
        <ion-tab-button onClick={() => this.toggleFullScreenMode()} color="primary" class="wider-devices" mode="md">
          {this.renderFullscreen()}
        </ion-tab-button>
      );
    } else {
      return undefined;
    }
  }

  private renderFullscreen() {
    if (this.fullscreen) {
      return [<ion-icon name="contract"></ion-icon>, <ion-label>Exit fullscreen</ion-label>];
    } else {
      return [<ion-icon name="expand"></ion-icon>, <ion-label>Fullscreen</ion-label>];
    }
  }
}
