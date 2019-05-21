import {Component, Element, State} from '@stencil/core';

import {Subscription} from 'rxjs';
import {filter} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';
import {Deck} from '../../../models/deck';
import {User} from '../../../models/user';

import {Utils} from '../../../utils/core/utils';

import {AuthService} from '../../../services/api/auth/auth.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {DeckService} from '../../../services/api/deck/deck.service';
import {UserService} from '../../../services/api/user/user.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-menu',
    styleUrl: 'app-menu.scss',
    shadow: false
})
export class AppMenu {

    @Element() el: HTMLElement;

    private authService: AuthService;
    private authSubscription: Subscription;

    private navService: NavService;

    private userSubscription: Subscription;
    private userService: UserService;

    private deckService: DeckService;

    private deckSubscription: Subscription;
    private deckEditorService: DeckEditorService;

    @State()
    private authUser: AuthUser;

    private decks: Deck[] = null;

    @State()
    private filteredDecks: Deck[] = null;

    private skeletons: number[] = Array(3).fill(0);

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();

        this.deckService = DeckService.getInstance();
        this.userService = UserService.getInstance();

        this.deckEditorService = DeckEditorService.getInstance();
    }

    componentWillLoad() {
        this.authSubscription = this.authService.watch().subscribe((authUser: AuthUser) => {
            this.authUser = authUser;
        });

        this.userSubscription = this.userService.watch().pipe(
            filter((user: User) => user && !user.anonymous)).subscribe(async (user: User) => {
            if (user) {
                try {
                    this.decks = await this.deckService.getUserDecks(user.id);
                    await this.filterDecks(null);
                } catch (err) {
                    // TODO: print error?
                    this.decks = [];
                    await this.filterDecks(null);
                }
            } else {
                this.decks = [];
                await this.filterDecks(null);
            }
        });
    }

    componentDidLoad() {
        this.deckSubscription = this.deckEditorService.watch().subscribe(async (deck: Deck) => {
            await this.updateDeckList(deck);

            const filter: string = await this.getCurrentFilter();
            await this.filterDecks(filter);
        });
    }

    componentDidUnload() {
        if (this.authSubscription) {
            this.authSubscription.unsubscribe();
        }

        if (this.userSubscription) {
            this.userSubscription.unsubscribe();
        }

        if (this.deckSubscription) {
            this.deckSubscription.unsubscribe();
        }
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    private async signOut() {
        await this.authService.signOut();

        this.navService.navigate({
            url: '/',
            direction: NavDirection.ROOT
        });
    }

    private updateDeckList(deck: Deck): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!deck || !deck.id || !deck.name) {
                resolve();
                return;
            }

            if (!this.decks || this.decks.length <= 0) {
                this.decks = [];
            }

            const index: number = this.decks.findIndex((filteredDeck: Deck) => {
                return filteredDeck.id === deck.id;
            });

            if (index < 0) {
                this.decks = [...this.decks, deck];
            } else {
                this.decks[index].name = deck.name;
                this.decks = [...this.decks];
            }

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

    private filterDecks(value: string): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!value || value === undefined || value === '') {
                this.filteredDecks = this.decks ? [...this.decks] : null;

                resolve();
                return;
            }

            if (!this.decks || this.decks.length <= 0) {
                this.filteredDecks = this.decks ? [...this.decks] : null;

                resolve();
                return;
            }

            const matchingDecks: Deck[] = this.decks.filter((matchDeck: Deck) => {
                return matchDeck.name && matchDeck.name.toLowerCase().indexOf(value.toLowerCase()) > -1
            });

            this.filteredDecks = [...matchingDecks];

            resolve();
        });
    }

    private getCurrentFilter(): Promise<string> {
        return new Promise<string>(async (resolve) => {
            const searchBar: HTMLIonSearchbarElement = this.el.querySelector('ion-searchbar');

            if (!searchBar) {
                resolve(null);
                return;
            }

            const input: HTMLInputElement = await searchBar.getInputElement();

            if (!input) {
                resolve(null);
                return;
            }

            resolve(input.value);
        });
    }

    private async navigateEditor() {
        this.navService.navigate({
            url: '/editor',
            direction: NavDirection.ROOT
        });
    }

    render() {
        return <ion-list>
            {this.renderUser()}

            <ion-item-divider>
                <ion-label>Presentations</ion-label>
                <ion-button size="small" slot="end" shape="round" onClick={() => this.navigateEditor()} class="new ion-margin-end">
                    <ion-icon name="book" slot="start"></ion-icon>
                    <ion-label>New</ion-label>
                </ion-button>
            </ion-item-divider>

            {this.renderPresentations()}

            {this.renderSignOut()}

        </ion-list>;
    }

    private renderUser() {
        if (Utils.isLoggedIn(this.authUser)) {
            return <ion-item class="user">
                <app-user-info avatarColSize={3}></app-user-info>
            </ion-item>;
        } else {
            return <ion-item class="user"></ion-item>;
        }
    }

    private renderPresentations() {
        if (Utils.isLoggedIn(this.authUser)) {
            return [
                this.renderDecksFilter(),
                this.renderDecks()
            ];
        } else {
            return <ion-item button onClick={() => this.signIn()}>
                <ion-icon name="log-in" slot="start"></ion-icon>
                <ion-label>Sign in</ion-label>
            </ion-item>;
        }
    }

    private renderSignOut() {
        if (Utils.isLoggedIn(this.authUser)) {
            return <ion-item button class="signout" onClick={() => this.signOut()}>
                <ion-icon name="log-out" slot="start"></ion-icon>
                <ion-label>Sign out</ion-label>
            </ion-item>;
        } else {
            return undefined;
        }
    }

    private renderDecksFilter() {
        return <ion-searchbar debounce={500} animated placeholder="Filter your presentations"
                              onIonChange={(e: CustomEvent) => this.filterDecksOnChange(e)}
                              class="ion-no-padding ion-margin-top ion-margin-bottom"></ion-searchbar>;
    }

    private renderDecks() {
        if (this.filteredDecks && this.filteredDecks.length > 0) {
            return (
                this.filteredDecks.map((deck: Deck) => {
                    const url: string = `/editor/${deck.id}`;

                    return <ion-item href={url} routerDirection="root">
                        <ion-icon name="book" slot="start"></ion-icon>
                        <ion-label>{deck.name}</ion-label>
                    </ion-item>
                })
            );
        } else if (this.filteredDecks && this.filteredDecks.length === 0) {
            return (
                <ion-item>
                    <ion-label>No presentations 😔</ion-label>
                </ion-item>
            )
        } else {
            return this.renderSkeletons();
        }
    }

    private renderSkeletons() {
        if (this.skeletons && this.skeletons.length > 0) {
            return (
                this.skeletons.map((_value: number) => {
                    return <ion-item>
                        <ion-icon name="book" slot="start"></ion-icon>
                        <ion-skeleton-text animated></ion-skeleton-text>
                    </ion-item>
                })
            );
        } else {
            return undefined;
        }
    }

}
