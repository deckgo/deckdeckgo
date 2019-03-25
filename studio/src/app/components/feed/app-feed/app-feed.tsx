import {Component} from '@stencil/core';

@Component({
    tag: 'app-feed',
    styleUrl: 'app-feed.scss',
    shadow: false
})
export class AppFeed {

    render() {
        return [
            <ion-card>

                <div class="deck-container">
                    <deckgo-deck embedded={true} pager={false}>
                        <deckgo-slide-title>
                            <h1 slot="title">DeckDeckGo</h1>
                            <div no-shadow slot="content">
                                <h3>The Progressive Web App alternative for simple presentations 🚀</h3>
                            </div>
                        </deckgo-slide-title>

                        <deckgo-slide-split>
                            <h1 slot="title">Progressive Web App</h1>
                            <h3 slot="start">Publish your presentation as a</h3>
                            <div slot="end"><strong>Progressive Web App</strong></div>
                        </deckgo-slide-split>

                        <deckgo-slide-youtube src="https://www.youtube.com/watch?v=3o3oGBTTRSs">
                            <h2 slot="title">Remote Control</h2>
                        </deckgo-slide-youtube>
                    </deckgo-deck>
                </div>

                <hr class="deck-divider" margin></hr>

                <app-feed-card-content firstCard={true}></app-feed-card-content>
            </ion-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
            <app-feed-card></app-feed-card>,
        ];
    }
}
