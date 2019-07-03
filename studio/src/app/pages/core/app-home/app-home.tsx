import {Component, h, State} from '@stencil/core';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {FeedService} from '../../../services/data/feed/feed.service';

@Component({
    tag: 'app-home',
    styleUrl: 'app-home.scss'
})
export class AppHome {

    @State()
    private mobile: boolean = false;

    private feedService: FeedService;

    constructor() {
        this.feedService = FeedService.getInstance();
    }

    componentWillLoad() {
        this.mobile = DeckDeckGoUtils.isMobile();
    }

    private async refreshFeed($event) {
        if (!$event || !$event.target) {
            return;
        }

        await this.feedService.refresh();

        $event.target.complete();
    }

    render() {
        return [
            <app-navigation presentation={true}></app-navigation>,
            <ion-content class="ion-padding">

                {this.renderRefresher()}

                <main>
                    <app-feed></app-feed>

                    <app-popular></app-popular>
                </main>

            </ion-content>
        ];
    }

    private renderRefresher() {
        if (this.mobile) {
            return <ion-refresher slot="fixed" onIonRefresh={($event: CustomEvent) => this.refreshFeed($event)}>
                <ion-refresher-content></ion-refresher-content>
            </ion-refresher>;
        } else {
            // Only on mobile devices otherwise conflict with click action to open presentations from cards
            return undefined;
        }
    }
}
