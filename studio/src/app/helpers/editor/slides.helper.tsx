import {EventEmitter, JSX} from '@stencil/core';
import {modalController, OverlayEventDetail, popoverController} from '@ionic/core';

import {SlideAttributes, SlideSplitType, SlideTemplate} from '../../models/data/slide';

import {CreateSlidesUtils} from '../../utils/editor/create-slides.utils';
import {PlaygroundAction} from '../../utils/editor/playground-action';
import {DemoAction} from '../../utils/editor/demo-action';

export class SlidesHelper {
  constructor(private addSlide: EventEmitter<JSX.IntrinsicElements>, private blockSlide: EventEmitter<boolean>) {}

  async openSlideAdd($event: CustomEvent | UIEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-create-slide',
      event: $event.detail,
      mode: 'md',
      showBackdrop: false,
      cssClass: 'popover-menu popover-menu-wide',
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
        } else if (detail.data.template === SlideTemplate.SPLIT && detail.data.attributes && detail.data.attributes.type === SlideSplitType.DEMO) {
          await this.openDemo();
        } else if (detail.data.template === SlideTemplate.PLAYGROUND) {
          await this.openPlayground();
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
      component: 'app-gif',
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlideGif(detail.data);
    });

    await modal.present();
  }

  private async openYoutube() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-youtube',
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlideYoutube(detail.data);

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openPlayground() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-playground',
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlidePlayground(detail.data);

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openDemo() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-demo',
    });

    modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      await this.addSlideDemo(detail.data);

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openPoll() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-poll-options',
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
      component: 'app-custom-data',
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

  private async addSlidePlayground(playground: PlaygroundAction) {
    if (!playground || !playground.src || playground.src === undefined || playground.src === '') {
      return;
    }

    const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlidePlayground(playground.src, playground.theme);

    this.addSlide.emit(slide);
  }

  private addSlideDemo(demo: DemoAction): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!demo || !demo.src || demo.src === undefined || demo.src === '') {
        resolve();
        return;
      }

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideDemo(demo.src, demo.mode);

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
          src: url,
        };
      } else {
        attributes.src = url;
      }

      attributes.customLoader = true;

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideChart(attributes);

      this.addSlide.emit(slide);

      resolve();
    });
  }
}
