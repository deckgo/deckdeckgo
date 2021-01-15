import {Component, Element, Listen, h, State} from '@stencil/core';

import {PublishService} from '../../../services/editor/publish/publish.service';

@Component({
  tag: 'app-publish',
  styleUrl: 'app-publish.scss',
})
export class AppPublish {
  @Element() el: HTMLElement;

  @State()
  private publishedUrl: string;

  private publishService: PublishService;

  private unsubscribeSnapshot: () => void | undefined;

  constructor() {
    this.publishService = PublishService.getInstance();
  }

  async componentWillLoad() {
    this.unsubscribeSnapshot = await this.publishService.snapshot();
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    if (this.unsubscribeSnapshot) {
      this.unsubscribeSnapshot();
    }

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async published($event: CustomEvent) {
    if ($event && $event.detail) {
      this.publishedUrl = $event.detail;

      await this.updateSlidesQRCode();
    }
  }

  private updateSlidesQRCode(): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!document) {
        resolve();
        return;
      }

      const slides: NodeListOf<HTMLElement> = document.querySelectorAll('deckgo-slide-qrcode');

      if (!slides) {
        resolve();
        return;
      }

      for (const slide of Array.from(slides)) {
        if (!slide.hasAttribute('custom-qrcode')) {
          slide.setAttribute('content', this.publishedUrl);
        }
      }

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="tertiary">
          <ion-buttons slot="start">
            <ion-button onClick={() => this.closeModal()}>
              <ion-icon aria-label="Close" src="/assets/icons/ionicons/close.svg"></ion-icon>
            </ion-button>
          </ion-buttons>
          {this.renderTitle()}
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding fullscreen-padding">
        <main class={this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '' ? 'published ion-padding' : 'ion-padding'}>
          {this.renderMain()}
        </main>
      </ion-content>,
    ];
  }

  private renderTitle() {
    if (this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '') {
      return <ion-title class="ion-text-uppercase">Published</ion-title>;
    } else {
      return <ion-title class="ion-text-uppercase">Ready to share?</ion-title>;
    }
  }

  private renderMain() {
    if (this.publishedUrl && this.publishedUrl !== undefined && this.publishedUrl !== '') {
      return <app-publish-done publishedUrl={this.publishedUrl}></app-publish-done>;
    } else {
      return <app-publish-edit onPublished={($event: CustomEvent) => this.published($event)}></app-publish-edit>;
    }
  }
}
