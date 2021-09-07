import {Component, Element, Listen, State, h} from '@stencil/core';

import i18n from '../../../stores/i18n.store';

import {GifService} from '../../../providers/tenor/gif/gif.service';
import {ImageHistoryService} from '../../../services/editor/image-history/image-history.service';

import { AppIcon } from '../../../components/core/app-icon/app-icon';

@Component({
  tag: 'app-gif',
  styleUrl: 'app-gif.scss'
})
export class AppGif {
  @Element() el: HTMLElement;

  private gifService: GifService;

  @State()
  private categoriesOdd: TenorCategory[];

  @State()
  private categoriesEven: TenorCategory[];

  @State()
  private gifsOdd: TenorGif[];

  @State()
  private gifsEven: TenorGif[];

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
    this.gifService = GifService.getInstance();
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

  private selectGif($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail) {
        resolve();
        return;
      }

      const gif: TenorGif = $event.detail;

      await this.gifService.registerShare(gif.id);

      await this.imageHistoryService.push(gif);

      await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(gif);

      resolve();
    });
  }

  private fetchCategories(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.searching = true;

      const categories: TenorCategory[] = await this.gifService.getCategories();

      this.searching = false;

      if (!categories || categories.length <= 0) {
        resolve();
        return;
      }

      this.categoriesOdd = categories.filter((_a, i) => i % 2);
      this.categoriesEven = categories.filter((_a, i) => !(i % 2));

      resolve();
    });
  }

  private selectCategory(searchTerm: string) {
    this.searchTerm = searchTerm;
  }

  private search(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.searchTerm || this.searchTerm.length <= 0) {
        await this.clear();
        resolve();
        return;
      }

      this.searching = true;

      const tenorResponse: TenorSearchResponse | undefined = await this.gifService.getGifs(this.searchTerm, this.paginationNext);

      this.searching = false;

      if (!tenorResponse) {
        this.emptyGifs();

        resolve();
        return;
      }

      const gifs: TenorGif[] = tenorResponse.results;

      if (!gifs || gifs.length <= 0) {
        this.emptyGifs();

        resolve();
        return;
      }

      if (!this.gifsOdd) {
        this.gifsOdd = [];
      }

      if (!this.gifsEven) {
        this.gifsEven = [];
      }

      const newSearchTerm: boolean = !this.previousSearchTerm || this.searchTerm !== this.previousSearchTerm;

      if (newSearchTerm) {
        this.gifsOdd = [];
        this.gifsEven = [];
      }

      this.gifsOdd = [...this.gifsOdd, ...gifs.filter((_a, i) => !(i % 2))];
      this.gifsEven = [...this.gifsEven, ...gifs.filter((_a, i) => i % 2)];

      if (!this.paginationNext || this.paginationNext === 0 || newSearchTerm) {
        // We just put a small delay because of the repaint
        setTimeout(async () => {
          await this.autoScrollToTop();
        }, 100);
      }

      this.paginationNext = tenorResponse.next;

      this.previousSearchTerm = this.searchTerm;

      resolve();
    });
  }

  private searchNext(e: CustomEvent<void>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.search();

      (e.target as HTMLIonInfiniteScrollElement).complete();

      resolve();
    });
  }

  private emptyGifs() {
    this.gifsOdd = [];
    this.gifsEven = [];

    this.disableInfiniteScroll = true;
  }

  private clear(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.searchTerm = undefined;

      this.gifsOdd = null;
      this.gifsEven = null;

      this.disableInfiniteScroll = false;

      this.paginationNext = 0;

      resolve();
    });
  }

  private handleInput($event: CustomEvent<KeyboardEvent>) {
    this.searchTerm = ($event.target as InputTargetEvent).value;
  }

  private autoScrollToTop(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const content: HTMLIonContentElement = this.el.querySelector('ion-content');

      if (!content) {
        resolve();
        return;
      }

      await content.scrollToTop();

      resolve();
    });
  }

  render() {
    return [
      <ion-header>
        <ion-toolbar color="secondary">
          <ion-buttons slot="start">{this.renderCloseButton()}</ion-buttons>
          <ion-title class="ion-text-uppercase">{i18n.state.editor.gifs}</ion-title>
        </ion-toolbar>
      </ion-header>,
      <ion-content class="ion-padding">
        {this.renderCategories()}

        <app-image-columns
          imagesOdd={this.gifsOdd}
          imagesEven={this.gifsEven}
          onSelectImage={($event: CustomEvent) => this.selectGif($event)}></app-image-columns>

        <ion-infinite-scroll threshold="100px" disabled={this.disableInfiniteScroll} onIonInfinite={(e: CustomEvent<void>) => this.searchNext(e)}>
          <ion-infinite-scroll-content loadingText={i18n.state.core.loading}></ion-infinite-scroll-content>
        </ion-infinite-scroll>
      </ion-content>,
      <ion-footer>
        <ion-toolbar>
          <ion-searchbar
            debounce={500}
            placeholder={i18n.state.editor.search_tenor}
            value={this.searchTerm}
            onIonClear={() => this.clear()}
            onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
            onIonChange={() => {
              this.search();
            }}></ion-searchbar>
        </ion-toolbar>
      </ion-footer>
    ];
  }

  private renderCloseButton() {
    if ((this.gifsEven || this.gifsOdd) && !this.searching) {
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
    if (this.gifsEven || this.gifsOdd) {
      return undefined;
    }

    return (
      <div class="gifs-container">
        <div class="gifs-column">{this.renderCategoriesColumn(this.categoriesOdd)}</div>
        <div class="gifs-column">{this.renderCategoriesColumn(this.categoriesEven)}</div>
      </div>
    );
  }

  private renderCategoriesColumn(categories: TenorCategory[]) {
    if (categories && categories.length > 0) {
      return categories.map((category: TenorCategory) => {
        if (category.image) {
          return (
            <div custom-tappable class="gifs-category ion-padding" onClick={() => this.selectCategory(category.searchterm)}>
              <div class="gifs-category-container">
                <deckgo-lazy-img imgSrc={category.image}></deckgo-lazy-img>
                <div class="gifs-category-placeholder">
                  <h2 class="ion-no-margin">{category.name}</h2>
                </div>
              </div>
            </div>
          );
        } else {
          return undefined;
        }
      });
    } else {
      return undefined;
    }
  }
}
