import {Component, h, State} from '@stencil/core';

import {Deck} from '../../../models/data/deck';

import {FeedService} from '../../../services/data/feed/feed.service';
import {Subscription} from 'rxjs';

@Component({
    tag: 'app-feed',
    styleUrl: 'app-feed.scss',
    shadow: false
})
export class AppFeed {

    private feedService: FeedService;

    @State()
    private decks: Deck[] = [];

    private subscription: Subscription;

    constructor() {
        this.feedService = FeedService.getInstance();
    }

    async componentWillLoad() {
        this.subscription = this.feedService.watchDecks().subscribe((decks: Deck[]) => {
            this.decks = decks;
        });
    }

    async componentDidLoad() {
        await this.feedService.find();
    }

    async componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    // TODO: To be replaced with Inifite Scroll when Ionic bug will be solved
    // https://github.com/ionic-team/ionic/issues/18632#issuecomment-506007810
    private async findNextDecks() {
        await this.feedService.find();
    }

    render() {
        return [
            this.renderDecks(),
            <ion-button onClick={() => this.findNextDecks()}>Next</ion-button>
        ]
    }

    private renderDecks() {
        if (this.decks && this.decks.length > 0) {
            return (
                this.decks.map((deck: Deck, i: number) => {
                    return <app-feed-card compact={i > 0} deck={deck}></app-feed-card>
                })
            );
        } else {
            return undefined;
        }
    }
}
