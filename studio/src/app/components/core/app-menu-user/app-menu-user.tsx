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

    @State()
    private authUser: AuthUser;

    @State()
    private decks: Deck[] = [];

    private skeletons: number[] = Array(3).fill(0);

    constructor() {
        this.authService = AuthService.getInstance();
        this.navService = NavService.getInstance();

        this.deckService = DeckService.getInstance();
        this.userService = UserService.getInstance();
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

    componentDidUnload() {
        if (this.authSubscription) {
            this.authSubscription.unsubscribe();
        }

        if (this.userSubscription) {
            this.userSubscription.unsubscribe();
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

                    return <ion-item href={url} routerDirection="forward">
                        <ion-icon name="book" slot="start"></ion-icon>
                        <ion-label>{deck.name}</ion-label>
                    </ion-item>
                })
            );
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
