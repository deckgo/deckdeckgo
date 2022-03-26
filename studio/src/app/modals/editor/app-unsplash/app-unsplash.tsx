import {UnsplashPhoto, UnsplashSearchResponse} from '@deckdeckgo/editor';
import {Component, Element, Fragment, h, Listen, State} from '@stencil/core';
import {AppIcon} from '../../../components/core/app-icon/app-icon';
import {getUnsplashPhotos, registerUnsplashDownload} from '../../../providers/unsplash/unsplash.provider';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';
import i18n from '../../../stores/i18n.store';
import {unsplash} from '../../../utils/core/environment.utils';

@Component({
  tag: 'app-unsplash',
  styleUrl: 'app-unsplash.scss'
})
export class AppUnsplash {
  @Element() el: HTMLElement;

  private imageHistoryService: ImageHistoryService;

  @State()
  private photos: UnsplashPhoto[];

  @State()
  private searchTerm: string;

  private previousSearchTerm: string;

  @State()
  private disableInfiniteScroll = false;

  private paginationNext: number = 1;

  @State()
  private searching: boolean = false;

  private input: HTMLIonSearchbarElement | undefined;

  constructor() {
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

  componentDidLoad() {
    history.pushState({modal: true}, null);

    setTimeout(async () => await this.input?.setFocus(), 500);
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async selectPhoto(photo: UnsplashPhoto) {
    await registerUnsplashDownload(photo);

    await this.imageHistoryService.push(photo);

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(photo);
  }

  private clear() {
    this.searchTerm = undefined;

    this.photos = undefined;

    this.disableInfiniteScroll = false;

    this.paginationNext = 1;
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.searchTerm = ($event.target as InputTargetEvent).value;
  }

  private async search() {
    if (!this.searchTerm || this.searchTerm.length <= 0) {
      this.clear();
      return;
    }

    if (!unsplash()) {
      return;
    }

    this.searching = this.photos === undefined;

    const unsplashResponse: UnsplashSearchResponse | undefined = await getUnsplashPhotos({
      searchTerm: this.searchTerm,
      paginationNext: this.paginationNext
    });

    this.searching = false;

    if (!unsplashResponse) {
      return;
    }

    const photos: UnsplashPhoto[] = unsplashResponse.results;

    if (!photos || photos.length <= 0) {
      this.emptyPhotos();
      return;
    }

    if (!this.photos) {
      this.photos = [];
    }

    const newSearchTerm: boolean = !this.previousSearchTerm || this.searchTerm !== this.previousSearchTerm;

    if (newSearchTerm) {
      this.photos = [];
    }

    this.photos = [...this.photos, ...photos];

    if (!this.paginationNext || this.paginationNext === 0 || newSearchTerm) {
      // We just put a small delay because of the repaint
      setTimeout(async () => {
        await this.autoScrollToTop();
      }, 100);
    }

    this.disableInfiniteScroll = this.paginationNext * 10 >= unsplashResponse.total;

    this.paginationNext++;

    this.previousSearchTerm = this.searchTerm;
  }

  private async autoScrollToTop() {
    const content: HTMLIonContentElement | null = this.el.querySelector('ion-content');
    await content?.scrollToTop();
  }

  private emptyPhotos() {
    this.photos = [];
    this.disableInfiniteScroll = true;
  }

  private async searchNext($event: CustomEvent<void>) {
    await this.search();

    await ($event.target as HTMLIonInfiniteScrollElement).complete();
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="primary">
            <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.editor.stock_photo}</ion-title>
          </ion-toolbar>
        </ion-header>

        <ion-content class="ion-padding">
          <main>
            {this.renderPhotos()}

            {this.renderPhotosPlaceHolder()}

            <ion-infinite-scroll
              threshold="100px"
              disabled={this.disableInfiniteScroll}
              onIonInfinite={async ($event: CustomEvent<void>) => await this.searchNext($event)}>
              <ion-infinite-scroll-content loadingText={i18n.state.core.loading}></ion-infinite-scroll-content>
            </ion-infinite-scroll>
          </main>
        </ion-content>

        <ion-footer>
          <ion-toolbar>
            <ion-searchbar
              debounce={500}
              placeholder={i18n.state.editor.search_term}
              value={this.searchTerm}
              ref={(el) => (this.input = el as HTMLIonSearchbarElement)}
              onIonClear={() => this.clear()}
              onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
              onIonChange={async () => {
                await this.search();
              }}></ion-searchbar>
          </ion-toolbar>
        </ion-footer>
      </Fragment>
    );
  }

  private renderCloseButton() {
    if (!this.searchTerm || this.searchTerm.length <= 0 || this.searching) {
      return (
        <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
          <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
        </ion-button>
      );
    } else {
      return (
        <ion-button onClick={() => this.clear()}>
          <AppIcon name="arrow-back" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-button>
      );
    }
  }

  private renderPhotosPlaceHolder() {
    if (this.photos?.length > 0 || !this.searching) {
      return undefined;
    }

    return (
      <ion-label class="empty">
        {i18n.state.editor.searching} <ion-spinner color="dark"></ion-spinner>
      </ion-label>
    );
  }

  private renderPhotos() {
    if (!this.photos || this.photos.length <= 0) {
      return undefined;
    }

    return this.photos.map((photo: UnsplashPhoto, index: number) => {
      return (
        <article custom-tappable onClick={async () => await this.selectPhoto(photo)} key={`unsplash-${index}`}>
          <app-asset-image image={photo}></app-asset-image>
        </article>
      );
    });
  }
}
