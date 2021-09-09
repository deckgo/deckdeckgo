import {Component, Element, Event, EventEmitter, h, JSX, Prop} from '@stencil/core';

import type {OverlayEventDetail} from '@ionic/core';

import i18n from '../../../../../stores/i18n.store';

import {SlideAttributes, SlideSplitType, SlideTemplate} from '../../../../../models/data/slide';

import {CreateSlidesUtils} from '../../../../../utils/editor/create-slides.utils';
import {PlaygroundAction} from '../../../../../types/editor/playground-action';
import {DemoAction} from '../../../../../types/editor/demo-action';

import {modalController, popoverController} from '../../../../../utils/ionic/ionic.overlay';

@Component({
  tag: 'app-action-add-slide',
  shadow: false
})
export class AppActionAddSlide {
  @Element() el: HTMLElement;

  @Prop()
  slidesLength: number | undefined;

  @Prop()
  popoverCssClass: string;

  @Event({bubbles: true})
  private addSlide: EventEmitter<JSX.IntrinsicElements>;

  @Event({bubbles: true})
  private blockSlide: EventEmitter<boolean>;

  async onActionOpenSlideAdd($event: CustomEvent) {
    if (!$event || !$event.detail) {
      return;
    }

    const popover: HTMLIonPopoverElement = await popoverController.create({
      component: 'app-create-slide',
      mode: 'ios',
      showBackdrop: false,
      cssClass: `popover-menu popover-menu-wide ${this.popoverCssClass}`
    });

    popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
      if (detail?.data) {
        if (detail.data.template === SlideTemplate.GIF) {
          await this.openGifPicker();
        } else if (detail.data.template === SlideTemplate.YOUTUBE) {
          await this.openYoutube();
        } else if (detail.data.template === SlideTemplate.CHART) {
          await this.openChart(detail.data.attributes);
        } else if (detail.data.template === SlideTemplate.POLL) {
          await this.openPoll();
        } else if (detail.data.template === SlideTemplate.SPLIT && detail.data.attributes?.type === SlideSplitType.DEMO) {
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

      this.blockSlide.emit(false);
    });

    this.blockSlide.emit(true);

    await modal.present();
  }

  private async openPlayground() {
    const modal: HTMLIonModalElement = await modalController.create({
      component: 'app-playground'
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
      component: 'app-demo'
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

      const url: string | undefined = dataFile.downloadUrl;

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

      attributes.customLoader = true;

      const slide: JSX.IntrinsicElements = await CreateSlidesUtils.createSlideChart(attributes);

      this.addSlide.emit(slide);

      resolve();
    });
  }

  render() {
    return (
      <app-action-busy
        aria-label={i18n.state.editor.add_slide}
        iconName="add"
        onActionReady={($event: CustomEvent) => this.onActionOpenSlideAdd($event)}>
        <ion-label aria-hidden="true">{i18n.state.editor.add_slide}</ion-label>
      </app-action-busy>
    );
  }
}
