import {Component, ComponentInterface, Fragment, h, JSX, State} from '@stencil/core';

import {loadingController} from '@ionic/core';

import {convertStyle} from '@deckdeckgo/editor';
import {debounce} from '@deckdeckgo/utils';

import {Deck, Slide, AuthUser, formatDate} from '@deckdeckgo/editor';

import authStore from '../../../stores/auth.store';
import errorStore from '../../../stores/error.store';
import i18n from '../../../stores/i18n.store';
import syncStore from '../../../stores/sync.store';

import {Editor} from '../../../types/editor/editor';

import {ParseDeckSlotsUtils} from '../../../utils/editor/parse-deck-slots.utils';
import {ParseSlidesUtils} from '../../../utils/editor/parse-slides.utils';
import {TemplateUtils} from '../../../utils/editor/template.utils';
import {loadAndImportDeck, navigateReloadEditor} from '../../../utils/core/dashboard.utils';
import {getEdit} from '../../../utils/editor/editor.utils';

import {decks} from '../../../providers/data/deck/deck.provider';
import {getSlide} from '../../../providers/data/slide/slide.provider';
import {initTemplates} from '../../../providers/data/template/template.provider';

import {ImageEvents} from '../../../events/core/image/image.events';
import {ChartEvents} from '../../../events/core/chart/chart.events';
import {AppAnonymousContent} from '../../../components/core/app-anonymous-content/app-anonymous-content';

interface DeckAndFirstSlide {
  deck: Deck;
  slide: JSX.IntrinsicElements | undefined;
  style: Record<string, string> | undefined;
  background: JSX.IntrinsicElements | undefined;
  header: JSX.IntrinsicElements | undefined;
  footer: JSX.IntrinsicElements | undefined;
}

@Component({
  tag: 'app-decks',
  styleUrl: 'app-decks.scss'
})
export class AppDecks implements ComponentInterface {
  @State()
  private filteredDecks: DeckAndFirstSlide[] = null;

  @State()
  private loading: boolean = true;

  @State()
  private decksLoading: boolean = true;

  @State()
  private currentDeckId: string | undefined;

  private readonly debounceLoading: () => void = debounce(() => (this.loading = false), 750);
  private readonly debounceDecksLoading: () => void = debounce(() => (this.decksLoading = false), 150);

  private decks: DeckAndFirstSlide[] = null;

  private imageEvents: ImageEvents = new ImageEvents();
  private chartEvents: ChartEvents = new ChartEvents();

  private destroyListener;

  componentWillLoad() {
    this.imageEvents.init();
    this.chartEvents.init();
  }

  async componentDidLoad() {
    this.destroyListener = authStore.onChange('authUser', async (_authUser: AuthUser | null) => {
      await this.initDashboard();
    });

    await this.initDashboard();

    const editor: Editor | undefined = await getEdit();
    this.currentDeckId = editor?.type === 'deck' ? editor?.id : undefined;
  }

  private async initDashboard() {
    if (!authStore.state.loggedIn) {
      this.debounceLoading();
      return;
    }

    this.destroyListener();

    try {
      const userDecks: Deck[] = await decks(authStore.state.authUser.uid);

      await initTemplates();

      this.decks = await this.fetchFirstSlides(userDecks);
      this.filterDecks(null);
    } catch (err) {
      errorStore.state.error = 'Cannot init your dashboard.';
    }

    this.debounceLoading();
  }

  disconnectedCallback() {
    this.destroyListener?.();

    this.imageEvents.destroy();
    this.chartEvents.destroy();
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

  private async initDeckAndFirstSlide(deck: Deck, slideId: string): Promise<DeckAndFirstSlide> {
    try {
      const slide: Slide = await getSlide(deck.id, slideId);

      const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(slide, false);

      const style: Record<string, string | undefined> | undefined = this.convertStyle(deck);

      const background: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.background, 'background');
      const header: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.header, 'header');
      const footer: JSX.IntrinsicElements | undefined = await ParseDeckSlotsUtils.convert(deck.data.footer, 'footer');

      await TemplateUtils.loadSlideTemplate(slide);

      return {
        deck,
        slide: element,
        style,
        background,
        header,
        footer
      };
    } catch (err) {
      return undefined;
    }
  }

  private convertStyle(deck: Deck): Record<string, string | undefined> | undefined {
    let style: Record<string, string> | undefined = undefined;
    if (deck.data?.attributes?.style) {
      style = convertStyle(deck.data.attributes.style);
    }

    return style;
  }

  private filterDecks(value: string | null) {
    if (!value || value === undefined || value === '') {
      this.filteredDecks = this.decks ? [...this.decks] : [];
      return;
    }

    if (!this.decks || this.decks.length <= 0) {
      this.filteredDecks = this.decks ? [...this.decks] : [];
      return;
    }

    const matchingDecks: DeckAndFirstSlide[] = this.decks.filter((matchDeck: DeckAndFirstSlide) => {
      return matchDeck.deck?.data?.name?.toLowerCase().indexOf(value.toLowerCase()) > -1;
    });

    this.filteredDecks = [...matchingDecks];
  }

  private filterDecksOnChange($event: CustomEvent) {
    if ($event && $event.detail) {
      this.filterDecks($event.detail.value);
      return;
    }

    this.filterDecks(null);
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
      await loadAndImportDeck(deck.deck);

      navigateReloadEditor();
    } catch (err) {
      errorStore.state.error = err;
    }

    await loading.dismiss();
  }

  private removeDeletedDeck($event: CustomEvent) {
    if (!$event || !$event.detail || $event.detail === undefined || $event.detail === '') {
      return;
    }

    const deckId: string = $event.detail;

    const index: number = this.findDeckIndex(deckId);

    if (index < 0) {
      return;
    }

    this.decks.splice(index, 1);

    this.filterDecks(null);
  }

  private findDeckIndex(id: string): number {
    if (!this.decks || this.decks.length < 0) {
      return -1;
    }

    if (!id || id === undefined || id === '') {
      return -1;
    }

    return this.decks.findIndex((matchDeck: DeckAndFirstSlide) => {
      return matchDeck?.deck?.id === id;
    });
  }

  render() {
    return (
      <Fragment>
        <app-navigation></app-navigation>
        <ion-content class="ion-padding">{this.renderContent()}</ion-content>
      </Fragment>
    );
  }

  private renderContent() {
    if (this.loading) {
      return <app-spinner></app-spinner>;
    }

    if (!authStore.state.authUser) {
      return <AppAnonymousContent title={i18n.state.menu.presentations} text={i18n.state.settings.access_decks}></AppAnonymousContent>;
    }

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
          onIonChange={($event: CustomEvent) => this.filterDecksOnChange($event)}
          class="ion-no-padding ion-margin-top ion-margin-bottom"
        />
      );
    }

    if (syncStore.state.dirty) {
      return <p>{i18n.state.dashboard.sync_slides}</p>;
    }

    return <p>{i18n.state.dashboard.no_slides}</p>;
  }

  private renderDecks() {
    if (this.filteredDecks?.length > 0) {
      return <div class={`container ${this.decksLoading ? 'loading' : ''}`}>{this.renderDecksCards()}</div>;
    }

    return undefined;
  }

  private renderDecksCards() {
    return this.filteredDecks.map((deck: DeckAndFirstSlide) => {
      if (deck === undefined) {
        return undefined;
      }

      return (
        <article key={deck.deck.id}>
          <ion-card custom-tappable class="item ion-no-margin" onClick={() => this.navigateDeck(deck)}>
            {this.renderDeck(deck)}
          </ion-card>

          {this.renderAside(deck)}
        </article>
      );
    });
  }

  private renderAside(deck: DeckAndFirstSlide) {
    return (
      <aside>
        <p>{deck.deck.data.name}</p>
        <p>{formatDate(deck.deck.data.updated_at)}</p>

        <app-dashboard-actions
          data={{deck: deck.deck}}
          disableDelete={deck.deck.id === this.currentDeckId}
          onDeleted={($event: CustomEvent) => this.removeDeletedDeck($event)}
          onCloned={() => navigateReloadEditor()}></app-dashboard-actions>
      </aside>
    );
  }

  private renderDeck(deck: DeckAndFirstSlide) {
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
