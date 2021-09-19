import {Component, Fragment, h, JSX, State} from '@stencil/core';

import {convertStyle} from '@deckdeckgo/deck-utils';
import {debounce} from '@deckdeckgo/utils';

import {Deck, Slide, AuthUser} from '@deckdeckgo/editor';

import authStore from '../../../stores/auth.store';
import navStore, {NavDirection} from '../../../stores/nav.store';
import errorStore from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';
import syncStore from '../../../stores/sync.store';

import {signIn} from '../../../utils/core/signin.utils';
import {renderI18n} from '../../../utils/core/i18n.utils';
import {ParseDeckSlotsUtils} from '../../../utils/editor/parse-deck-slots.utils';
import {ParseSlidesUtils} from '../../../utils/editor/parse-slides.utils';
import {TemplateUtils} from '../../../utils/editor/template.utils';
import {loadingController} from '../../../utils/ionic/ionic.overlay';
import {formatDate} from '../../../utils/core/date.utils';

import {decks} from '../../../providers/data/deck/deck.provider';
import {getSlide} from '../../../providers/data/slide/slide.provider';
import {initTemplates} from '../../../providers/data/template/template.provider';

import {ImageEventsHandler} from '../../../handlers/core/events/image/image-events.handler';
import {ChartEventsHandler} from '../../../handlers/core/events/chart/chart-events.handler';

import {EnvironmentAppConfig} from '../../../types/core/environment-config';
import {EnvironmentConfigService} from '../../../services/environment/environment-config.service';
import {loadAndImport} from '../../../utils/core/dashboard.utils';

interface DeckAndFirstSlide {
  deck: Deck;
  slide: JSX.IntrinsicElements | undefined;
  style: any;
  background: JSX.IntrinsicElements | undefined;
  header: JSX.IntrinsicElements | undefined;
  footer: JSX.IntrinsicElements | undefined;
}

@Component({
  tag: 'app-dashboard',
  styleUrl: 'app-dashboard.scss'
})
export class AppDashboard {
  @State()
  private filteredDecks: DeckAndFirstSlide[] = null;

  @State()
  private loading: boolean;

  @State()
  private decksLoading: boolean = true;

  private readonly debounceDecksLoading: () => void;

  private decks: DeckAndFirstSlide[] = null;

  private imageEventsHandler: ImageEventsHandler = new ImageEventsHandler();
  private chartEventsHandler: ChartEventsHandler = new ChartEventsHandler();

  private destroyListener;

  private cloud: 'offline' | 'firebase' | 'ic' = EnvironmentConfigService.getInstance().get<EnvironmentAppConfig>('app').cloud;

  constructor() {
    this.debounceDecksLoading = debounce(async () => {
      this.decksLoading = false;
    }, 150);
  }

  async componentWillLoad() {
    await this.imageEventsHandler.init();
    await this.chartEventsHandler.init();

    const params = new URLSearchParams(window.location.search);
    this.loading = params.get('signin') === 'success';
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initDashboard();
    });

    await this.initDashboard();
  }

  private async initDashboard() {
    if (!authStore.state.authUser || authStore.state.anonymous) {
      return;
    }

    this.destroyListener();

    this.loading = true;

    try {
      const userDecks: Deck[] = await decks(authStore.state.authUser.uid);

      await initTemplates();

      this.decks = await this.fetchFirstSlides(userDecks);
      await this.filterDecks(null);
    } catch (err) {
      errorStore.state.error = 'Cannot init your dashboard.';
    }

    this.loading = false;
  }

  disconnectedCallback() {
    if (this.destroyListener) {
      this.destroyListener();
    }

    this.imageEventsHandler.destroy();
    this.chartEventsHandler.destroy();
  }

  private async fetchFirstSlides(decks: Deck[]): Promise<DeckAndFirstSlide[]> {
    if (!decks || decks.length <= 0) {
      return [];
    }

    const promises: Promise<DeckAndFirstSlide>[] = decks
      .filter((deck: Deck) => deck.data && deck.data.slides && deck.data.slides.length)
      .map((deck: Deck) => this.initDeckAndFirstSlide(deck, deck.data.slides[0]));

    return Promise.all(promises);
  }

  private initDeckAndFirstSlide(deck: Deck, slideId: string): Promise<DeckAndFirstSlide> {
    return new Promise<DeckAndFirstSlide>(async (resolve) => {
      try {
        console.log('About to request slide in IC');

        const slide: Slide = await getSlide(deck.id, slideId);

        console.log('Slide request done', slide);

        const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(deck, slide, false);

        const style: any = await this.convertStyle(deck);

        const background: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.background, 'background');
        const header: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.header, 'header');
        const footer: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.footer, 'footer');

        await TemplateUtils.loadSlideTemplate(slide);

        resolve({
          deck,
          slide: element,
          style,
          background,
          header,
          footer
        });
      } catch (err) {
        resolve(undefined);
      }
    });
  }

  private async convertStyle(deck: Deck): Promise<any> {
    let style: any;
    if (deck.data?.attributes?.style) {
      style = await convertStyle(deck.data.attributes.style);
    } else {
      style = undefined;
    }

    return style;
  }

  private filterDecks(value: string): Promise<void> {
    return new Promise<void>((resolve) => {
      if (!value || value === undefined || value === '') {
        this.filteredDecks = this.decks ? [...this.decks] : [];

        resolve();
        return;
      }

      if (!this.decks || this.decks.length <= 0) {
        this.filteredDecks = this.decks ? [...this.decks] : [];

        resolve();
        return;
      }

      const matchingDecks: DeckAndFirstSlide[] = this.decks.filter((matchDeck: DeckAndFirstSlide) => {
        return matchDeck.deck?.data?.name?.toLowerCase().indexOf(value.toLowerCase()) > -1;
      });

      this.filteredDecks = [...matchingDecks];

      resolve();
    });
  }

  private async filterDecksOnChange(e: CustomEvent) {
    if (e && e.detail) {
      await this.filterDecks(e.detail.value);
    } else {
      await this.filterDecks(null);
    }
  }

  private async onSlidesDidLoad($event: CustomEvent) {
    await ($event?.target as HTMLDeckgoDeckElement).blockSlide(true);

    this.debounceDecksLoading();
  }

  private async navigateDeck(deck: DeckAndFirstSlide) {
    if (!deck || !deck.deck || !deck.deck.id || deck.deck.id === undefined || deck.deck.id === '') {
      return;
    }

    const loading: HTMLIonLoadingElement = await loadingController.create({});

    await loading.present();

    try {
      await loadAndImport(deck.deck);

      this.navigateReloadEditor();
    } catch (err) {
      errorStore.state.error = err;
    }

    await loading.dismiss();
  }

  private navigateReloadEditor() {
    navStore.state.nav = {
      url: '/',
      direction: NavDirection.RELOAD
    };
  }

  private removeDeletedDeck($event: CustomEvent): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!$event || !$event.detail || $event.detail === undefined || $event.detail === '') {
        resolve();
        return;
      }

      const deckId: string = $event.detail;

      const index: number = await this.findDeckIndex(deckId);

      if (index < 0) {
        resolve();
        return;
      }

      this.decks.splice(index, 1);

      await this.filterDecks(null);

      resolve();
    });
  }

  private findDeckIndex(id: string): Promise<number> {
    return new Promise<number>(async (resolve) => {
      if (!this.decks || this.decks.length < 0) {
        resolve(-1);
        return;
      }

      if (!id || id === undefined || id === '') {
        resolve(-1);
        return;
      }

      const index: number = this.decks.findIndex((matchDeck: DeckAndFirstSlide) => {
        return matchDeck?.deck?.id === id;
      });

      resolve(index);
    });
  }

  render() {
    if (this.loading) {
      return this.renderLoading();
    }

    if (authStore.state.anonymous) {
      return this.renderAnonymousContent();
    }

    return this.renderGuardedContent();
  }

  private renderLoading() {
    return <app-spinner></app-spinner>;
  }

  private renderAnonymousContent() {
    return (
      <main class="ion-padding fit">
        <h1>{i18n.state.nav.dashboard}</h1>

        {this.renderNotLoggedInContent()}
      </main>
    );
  }

  private renderNotLoggedInContent() {
    return renderI18n(i18n.state.settings.access_dashboard, {
      placeholder: '{0}',
      value: (
        <button type="button" class="app-button" onClick={() => signIn()}>
          {i18n.state.nav.sign_in}
        </button>
      )
    });
  }

  private renderGuardedContent() {
    return <main class="ion-padding fit">{this.renderYourPresentations()}</main>;
  }

  private renderYourPresentations() {
    return (
      <Fragment>
        <h1>{i18n.state.dashboard.your_presentations}</h1>

        {this.renderDecksFilter()}

        {this.renderDecks()}
      </Fragment>
    );
  }

  private renderDecksFilter() {
    if (this.filteredDecks?.length > 0) {
      return (
        <ion-searchbar
          debounce={500}
          animated={false}
          placeholder={i18n.state.dashboard.filter}
          onClick={($event) => $event.stopImmediatePropagation()}
          onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
          class="ion-no-padding ion-margin-top ion-margin-bottom"
        />
      );
    }

    if (['pending', 'in_progress'].includes(syncStore.state.sync)) {
      return <p>{i18n.state.dashboard.sync_slides}</p>;
    }

    return <p>{i18n.state.dashboard.no_slides}</p>;
  }

  private renderDecks() {
    if (this.filteredDecks && this.filteredDecks.length > 0) {
      return <div class={`container ${this.decksLoading ? 'loading' : ''}`}>{this.renderDecksCards()}</div>;
    } else {
      return undefined;
    }
  }

  private renderDecksCards() {
    return this.filteredDecks.map((deck: DeckAndFirstSlide) => {
      if (deck === undefined) {
        return undefined;
      }

      return (
        <article key={deck.deck.id} onClick={() => this.navigateDeck(deck)}>
          <app-dashboard-deck-actions
            deck={deck.deck}
            cloud={this.cloud}
            onDeckDeleted={($event: CustomEvent) => this.removeDeletedDeck($event)}
            onDeckCloned={() => this.navigateReloadEditor()}></app-dashboard-deck-actions>

          <ion-card class="item ion-no-margin">{this.renderDeck(deck)}</ion-card>

          {this.renderDeckInfo(deck)}
        </article>
      );
    });
  }

  private renderDeckInfo(deck: DeckAndFirstSlide) {
    return (
      <aside>
        <p>{deck.deck.data.name}</p>
        <p>{formatDate(deck.deck.data.updated_at)}</p>
      </aside>
    );
  }

  private renderDeck(deck: DeckAndFirstSlide) {
    if (!deck) {
      return undefined;
    } else {
      return (
        <deckgo-deck
          embedded={true}
          keyboard={false}
          direction="horizontal"
          direction-mobile="horizontal"
          style={deck.style}
          onSlidesDidLoad={($event: CustomEvent) => this.onSlidesDidLoad($event)}>
          {deck.slide}
          {deck.background}
          {deck.header}
          {deck.footer}
        </deckgo-deck>
      );
    }
  }
}
