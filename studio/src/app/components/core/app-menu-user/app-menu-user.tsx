import {Component, State} from '@stencil/core';

import {Subscription} from 'rxjs';
import {filter} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';
import {Deck} from '../../../models/deck';
import {User} from '../../../models/user';

import {Utils} from '../../../utils/core/utils';

import {AuthService} from '../../../services/auth/auth.service';
import {NavDirection, NavService} from '../../../services/nav/nav.service';
import {DeckService} from '../../../services/deck/deck.service';
import {UserService} from '../../../services/user/user.service';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';

@Component({
    tag: 'app-menu-user',
    styleUrl: 'app-menu-user.scss',
    shadow: false
})
export class AppMenuUser {

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

    @State()
    private decks: Deck[] = null;

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
                    const decks: Deck[] = await this.deckService.getUserDecks(user.id);
                    this.decks = [...decks];
                } catch (err) {
                    // TODO: print error?
                    this.decks = [];
                }
            } else {
                this.decks = [];
            }
        });
    }

    componentDidLoad() {
        this.deckSubscription = this.deckEditorService.watch().subscribe(async (deck: Deck) => {
            await this.updateDeckList(deck);
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
            url: '/signin',
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
                resolve();
                return;
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

    render() {
        return <ion-list>
            {this.renderUser()}

            <ion-item-divider>
                <ion-label>Presentations</ion-label>
                <ion-button size="small" slot="end" shape="round" margin-end href="/editor"
                            routerDirection="forward" class="new">
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
                <app-avatar slot="start" src={this.authUser.photo_url}></app-avatar>
                <ion-label>{this.authUser.name}</ion-label>
            </ion-item>;
        } else {
            return <ion-item class="user"></ion-item>;
        }
    }

    private renderPresentations() {
        if (Utils.isLoggedIn(this.authUser)) {
            return this.renderDecks();
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

    private renderDecks() {
        if (this.decks && this.decks.length > 0) {
            return (
                this.decks.map((deck: Deck) => {
                    const url: string = `/editor/${deck.id}`;

                    return <ion-item href={url} routerDirection="root">
                        <ion-icon name="book" slot="start"></ion-icon>
                        <ion-label>{deck.name}</ion-label>
                    </ion-item>
                })
            );
        } else if (this.decks && this.decks.length === 0) {
            return (
                <ion-item>
                    <ion-label>No presentations yet 😔</ion-label>
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
