import {Component, Element, Listen, State, h, Fragment} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {TenorProvider} from '../../../providers/tenor/tenor.provider';

import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

import {AppIcon} from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-gif',
  styleUrl: 'app-gif.scss'
})
export class AppGif {
  @Element() el: HTMLElement;

  private tenorProvider: TenorProvider;

  @State()
  private categories: TenorCategory[];

  @State()
  private gifs: TenorGif[];

  @State()
  private searchTerm: string;

  private previousSearchTerm: string;

  @State()
  private disableInfiniteScroll = false;

  private paginationNext: string | number = 0;

  private imageHistoryService: ImageHistoryService;

  @State()
  private searching: boolean = false;

  constructor() {
    this.tenorProvider = TenorProvider.getInstance();
    this.imageHistoryService = ImageHistoryService.getInstance();
  }

  async componentDidLoad() {
    history.pushState({modal: true}, null);

    await this.fetchCategories();
  }

  @Listen('popstate', {target: 'window'})
  async handleHardwareBackButton(_e: PopStateEvent) {
    await this.closeModal();
  }

  async closeModal() {
    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
  }

  private async selectGif(gif: TenorGif) {
    await this.tenorProvider.registerShare(gif.id);

    await this.imageHistoryService.push(gif);

    await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(gif);
  }

  private async fetchCategories() {
    this.searching = true;

    const categories: TenorCategory[] = await this.tenorProvider.getCategories();

    this.searching = false;

    if (!categories || categories.length <= 0) {
      return;
    }

    this.categories = [...categories.filter(({image}: TenorCategory) => image !== undefined && image !== null)];
  }

  private selectCategory(searchTerm: string) {
    this.searchTerm = searchTerm;
  }

  private async search() {
    if (!this.searchTerm || this.searchTerm.length <= 0) {
      this.clear();
      return;
    }

    this.searching = true;

    const tenorResponse: TenorSearchResponse | undefined = await this.tenorProvider.getGifs(this.searchTerm, this.paginationNext);

    this.searching = false;

    if (!tenorResponse) {
      this.emptyGifs();
      return;
    }

    const gifs: TenorGif[] = tenorResponse.results;

    if (!gifs || gifs.length <= 0) {
      this.emptyGifs();
      return;
    }

    if (!this.gifs) {
      this.gifs = [];
    }

    const newSearchTerm: boolean = !this.previousSearchTerm || this.searchTerm !== this.previousSearchTerm;

    if (newSearchTerm) {
      this.gifs = [];
    }

    this.gifs = [...this.gifs, ...gifs];

    if (!this.paginationNext || this.paginationNext === 0 || newSearchTerm) {
      // We just put a small delay because of the repaint
      setTimeout(async () => {
        await this.autoScrollToTop();
      }, 100);
    }

    this.paginationNext = tenorResponse.next;

    this.previousSearchTerm = this.searchTerm;
  }

  private async searchNext($event: CustomEvent<void>) {
    await this.search();

    await ($event.target as HTMLIonInfiniteScrollElement).complete();
  }

  private emptyGifs() {
    this.gifs = [];

    this.disableInfiniteScroll = true;
  }

  private clear() {
    this.searchTerm = undefined;

    this.gifs = undefined;

    this.disableInfiniteScroll = false;

    this.paginationNext = 0;
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.searchTerm = ($event.target as InputTargetEvent).value;
  }

  private async autoScrollToTop() {
    const content: HTMLIonContentElement | null = this.el.querySelector('ion-content');
    await content?.scrollToTop();
  }

  render() {
    return (
      <Fragment>
        <ion-header>
          <ion-toolbar color="secondary">
            <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
            <ion-title class="ion-text-uppercase">{i18n.state.editor.gifs}</ion-title>
          </ion-toolbar>
        </ion-header>

        <ion-content class="ion-padding">
          <main>
            {this.renderCategories()}

            {this.renderGifs()}

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
              placeholder={i18n.state.editor.search_tenor}
              value={this.searchTerm}
              onIonClear={() => this.clear()}
              onIonInput={($event: CustomEvent<KeyboardEvent>) => this.handleInput($event)}
              onIonChange={async () => {
                await this.search();
              }}></ion-searchbar>
          </ion-toolbar>
        </ion-footer>
      </Fragment>
    );
  }

  private renderCloseButton() {
    if (this.gifs && !this.searching) {
      return (
        <ion-button onClick={() => this.clear()}>
          <AppIcon name="arrow-back" ariaLabel="" ariaHidden={true}></AppIcon>
        </ion-button>
      );
    } else {
      return (
        <ion-button onClick={() => this.closeModal()} aria-label={i18n.state.core.close}>
          <AppIcon name="close" ariaHidden={true} ariaLabel=""></AppIcon>
        </ion-button>
      );
    }
  }

  private renderCategories() {
    if (this.gifs?.length > 0) {
      return undefined;
    }

    if (!this.categories || this.categories.length <= 0) {
      return undefined;
    }

    return this.categories.map((category: TenorCategory, index: number) => {
      return (
        <article custom-tappable key={`category-${index}`} onClick={() => this.selectCategory(category.searchterm)}>
          <app-asset-image image={category}></app-asset-image>
        </article>
      );
    });
  }

  private renderGifs() {
    if (!this.gifs || this.gifs.length <= 0) {
      return undefined;
    }

    return this.gifs.map((gif: TenorGif, index: number) => {
      return (
        <article custom-tappable onClick={async () => await this.selectGif(gif)} key={`gif-${index}`}>
          <app-asset-image image={gif}></app-asset-image>
        </article>
      );
    });
  }
}
