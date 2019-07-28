import {Component, h, State} from '@stencil/core';

import {filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth/auth.user';
import {Deck} from '../../../models/data/deck';

import {DeckUtils} from '../../../utils/core/deck-utils';

import {AuthService} from '../../../services/auth/auth.service';
import {DeckService} from '../../../services/data/deck/deck.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

@Component({
    tag: 'app-dashboard',
    styleUrl: 'app-dashboard.scss'
})
export class AppDashboard {

    @State()
    private authUser: AuthUser;

    @State()
    private filteredDecks: Deck[] = null;

    private authService: AuthService;

    private navService: NavService;
    private deckService: DeckService;

    private decks: Deck[] = null;

    private presentationUrl: string = EnvironmentConfigService.getInstance().get('presentationUrl');

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();
        this.deckService = DeckService.getInstance();
    }

    componentWillLoad() {
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined && !authUser.anonymous),
            take(1)).subscribe(async (authUser: AuthUser) => {
            this.authUser = authUser;

            this.decks = await this.deckService.getUserDecks(authUser.uid);
            this.filteredDecks = await DeckUtils.filterDecks(null, this.decks);
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
            this.filteredDecks = await DeckUtils.filterDecks(e.detail.value, this.decks);
        } else {
            this.filteredDecks = await DeckUtils.filterDecks(null, this.decks);
        }
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
                <ion-router-link onClick={() => this.signIn()}>Sign in</ion-router-link>
                to access your presentations.</p>
        </main>
    }

    private renderContent() {
        return <main class="ion-padding">
            <h1>Your presentations</h1>
            {this.renderDecksFilter()}
            {this.renderDecks()}
        </main>
    }

    private renderDecksFilter() {
        if (this.filteredDecks && this.filteredDecks.length > 0) {
            return <ion-searchbar debounce={500} animated={false} placeholder="Filter your presentations"
                                  onClick={($event) => $event.stopImmediatePropagation()}
                                  onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
                                  class="ion-no-padding ion-margin-top ion-margin-bottom"></ion-searchbar>;
        } else {
            return undefined;
        }
    }

    private renderDecks() {
        if (this.filteredDecks && this.filteredDecks.length > 0) {
            return (
                this.filteredDecks.map((deck: Deck) => {
                    return this.renderDeck(deck);
                })
            );
        } else {
            return this.renderEmptyDecks();
        }
    }

    private renderDeck(deck: Deck) {
        if (!deck || !deck.data) {
            return undefined;
        } else if (deck.data.meta && deck.data.meta.published) {
            return <a key={deck.id} href={this.presentationUrl + deck.data.meta.pathname} target="_blank">
                <app-feed-card deck={deck}></app-feed-card>
            </a>
        } else {
            return <app-feed-card key={deck.id} deck={deck}></app-feed-card>
        }
    }

    private renderEmptyDecks() {
        return <p>It's time to create your first presentation 😉</p>;
    }
}
