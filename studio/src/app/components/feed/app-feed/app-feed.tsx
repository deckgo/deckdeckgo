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

                <app-feed-card-content firstCard={true} caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card-content>
            </ion-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>,
            <app-feed-card caption="Card Title" description="Lorem ipsum dolor sit amet, consectetuer adipiscing elit. Aenean commodo ligula eget dolor. Aenean massa. Cum sociis natoque penatibus et magnis dis parturient montes, nascetur ridiculus mus. Donec quam felis, ultricies nec, pellentesque eu, pretium quis, sem. Nulla consequat massa quis enim. Donec pede justo, fringilla vel, aliquet nec, vulputate"></app-feed-card>
        ];
    }
}
