import {Component, h, JSX, State} from '@stencil/core';

import {filter, take} from 'rxjs/operators';

import {convertStyle} from '@deckdeckgo/deck-utils';

import {AuthUser} from '../../../models/auth/auth.user';
import {Deck} from '../../../models/data/deck';
import {Slide} from '../../../models/data/slide';

import {AuthService} from '../../../services/auth/auth.service';
import {DeckService} from '../../../services/data/deck/deck.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {ParseSlidesUtils} from '../../../utils/editor/parse-slides.utils';
import {SlideService} from '../../../services/data/slide/slide.service';
import {ParseBackgroundUtils} from '../../../utils/editor/parse-background.utils';

interface DeckAndFirstSlide {
    deck: Deck;
    slide: JSX.IntrinsicElements;
    style: any;
    background: any;
}

@Component({
    tag: 'app-dashboard',
    styleUrl: 'app-dashboard.scss'
})
export class AppDashboard {

    @State()
    private authUser: AuthUser;

    @State()
    private filteredDecks: DeckAndFirstSlide[] = null;

    private decks: DeckAndFirstSlide[] = null;

    private authService: AuthService;

    private navService: NavService;

    private deckService: DeckService;
    private slideService: SlideService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
        this.deckService = DeckService.getInstance();
        this.slideService = SlideService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined && !authUser.anonymous),
            take(1)).subscribe(async (authUser: AuthUser) => {
            this.authUser = authUser;

            const userDecks: Deck[] = await this.deckService.getUserDecks(authUser.uid);
            this.decks = await this.fetchFirstSlides(userDecks);
            await this.filterDecks(null);
        });
    }

    private fetchFirstSlides(decks: Deck[]): Promise<DeckAndFirstSlide[]> {
        return new Promise<DeckAndFirstSlide[]>(async (resolve) => {
            if (!decks || decks.length <= 0) {
                resolve([]);
                return;
            }

            const promises = [];
            decks.forEach((deck: Deck) => {
                if (deck && deck.data && deck.data.slides && deck.data.slides.length >= 1) {
                    promises.push(this.initDeckAndFirstSlide(deck, deck.data.slides[0]));
                }
            });

            const results: DeckAndFirstSlide[] = await Promise.all(promises);

            resolve(results);
        });
    }

    private initDeckAndFirstSlide(deck: Deck, slideId: string): Promise<DeckAndFirstSlide> {
        return new Promise<DeckAndFirstSlide>(async (resolve) => {
            try {
                const slide: Slide = await this.slideService.get(deck.id, slideId);
                const element: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide(deck, slide, false);

                let style: any;
                if (deck.data && deck.data.attributes && deck.data.attributes.style) {
                    style = await convertStyle(deck.data.attributes.style);
                } else {
                    style = undefined;
                }

                const background: any = await ParseBackgroundUtils.convertBackground(deck.data.background, false);

                resolve({
                    deck: deck,
                    slide: element,
                    style: style,
                    background: background
                });
            } catch (err) {
                resolve(undefined);
            }
        });
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
                return matchDeck.deck && matchDeck.deck.data && matchDeck.deck.data.name && matchDeck.deck.data.name.toLowerCase().indexOf(value.toLowerCase()) > -1
            });

            this.filteredDecks = [...matchingDecks];

            resolve();
        });
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    private async filterDecksOnChange(e: CustomEvent) {
        if (e && e.detail) {
            await this.filterDecks(e.detail.value);
        } else {
            await this.filterDecks(null);
        }
    }

    private blockSlide($event: CustomEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            if ($event && $event.target) {
                ($event.target as any).blockSlide(true);
            }

            resolve();
        })
    }

    private navigateDeck(deck: DeckAndFirstSlide) {
        if (!deck || !deck.deck || !deck.deck.id || deck.deck.id === undefined || deck.deck.id === '') {
            return;
        }

        const url: string = `/editor/${deck.deck.id}`;

        this.navService.navigate({
            url: url,
            direction: NavDirection.ROOT
        });
    }

    private async navigateEditor() {
        this.navService.navigate({
            url: '/editor',
            direction: NavDirection.ROOT
        });
    }

    private removeDeletedDeck($event: CustomEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !$event.detail || $event.detail === undefined || $event.detail === '') {
                resolve();
                return;
            }

            const deckId: string = $event.detail;

            if (!this.decks || this.decks.length < 0) {
                resolve();
                return;
            }

            const index: number = this.decks.findIndex((matchDeck: DeckAndFirstSlide) => {
                return matchDeck.deck && matchDeck.deck.id === deckId
            });

            if (index < 0) {
                resolve();
                return;
            }

            this.decks.splice(index, 1);

            await this.filterDecks(null);

            resolve();
        });
    }

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                {this.renderGuardedContent()}

            </ion-content>
        ];
    }

    private renderGuardedContent() {
        if (!this.authUser) {
            return this.renderNotLoggedInContent();
        } else {
            return this.renderContent();
        }
    }

    private renderNotLoggedInContent() {
        return <main class="ion-padding">
            <h1>Oh, hi! Good to have you.</h1>
            <p class="ion-padding-top">
                <ion-router-link onClick={() => this.signIn()}>Sign in</ion-router-link> to access to your presentations.</p>
        </main>
    }

    private renderContent() {
        if (!this.filteredDecks) {
            return undefined;
        }

        return <main class="ion-padding">
            {this.renderTitle()}
            {this.renderDecksFilter()}
            {this.renderCreateButton()}
            {this.renderDecks()}
        </main>
    }

    private renderTitle() {
        if (this.filteredDecks.length > 0) {
            return <h1>Your presentations</h1>;
        } else {
            return <h1>You don't have any presentation yet</h1>;
        }
    }

    private renderDecksFilter() {
        if (this.filteredDecks.length > 0) {
            return <ion-searchbar debounce={500} animated={false} placeholder="Filter your presentations"
                                onClick={($event) => $event.stopImmediatePropagation()}
                                onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
                                class="ion-no-padding ion-margin-top ion-margin-bottom" />
        } else {
            return undefined;
        }
    }

    private renderCreateButton() {
        if (this.filteredDecks.length === 0) {
            return  <ion-grid>
                <ion-row class="ion-justify-content-center">
                    <ion-column>
                        <ion-button slot="end" shape="round" fill="outline" 
                                    onClick={() => this.navigateEditor()} 
                                    class="ion-margin-top">
                            <ion-label>Start one now 🚀</ion-label>
                        </ion-button>   
                    </ion-column>
                </ion-row>
            </ion-grid>;
        } else {
            return undefined;
        }
    }

    private renderDecks() {
        if (this.filteredDecks && this.filteredDecks.length > 0) {
            return <div class="container">
                {this.renderDecksCards()}
            </div>
        } else {
            return undefined;
        }
    }

    private renderDecksCards() {
        return (
            this.filteredDecks.map((deck: DeckAndFirstSlide) => {
                return <ion-card class="item ion-no-margin" onClick={() => this.navigateDeck(deck)} key={deck.deck.id}>
                    {this.renderDeck(deck)}

                    <app-delete-deck-action deck={deck.deck} onDeckDeleted={($event: CustomEvent) => this.removeDeletedDeck($event)}></app-delete-deck-action>
                </ion-card>;
            })
        );
    }

    private renderDeck(deck: DeckAndFirstSlide) {
        if (!deck) {
            return undefined;
        } else {
            return <deckgo-deck embedded={true} keyboard={false} style={deck.style} onSlidesDidLoad={($event: CustomEvent) => this.blockSlide($event)}>
                {deck.slide}
                {deck.background}
            </deckgo-deck>
        }
    }

}
