import {Component, Element, Event, EventEmitter, h, Prop, State} from '@stencil/core';

import {GifService} from '../../../../services/tenor/gif/gif.service';

@Component({
    tag: 'app-publish-done',
    styleUrl: 'app-publish-done.scss'
})
export class AppPublishDone {

    @Element() el: HTMLElement;

    private gifService: GifService;

    @Prop()
    publishedUrl: string;

    @State()
    private gif: TenorGif;

    private keywords: string[] = ['Hooray', 'You did it', 'Applause', 'Thumbs up'];

    @State()
    private keywordIndex: number = Math.floor(Math.random() * 4);

    @Event() private openShare: EventEmitter<void>;

    constructor() {
        this.gifService = GifService.getInstance();
    }

    async componentDidLoad() {
        await this.initRandomGifUrl();
    }

    private initRandomGifUrl(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const gifResponse: TenorSearchResponse = await this.gifService.getRandomGif(this.keywords[this.keywordIndex]);

            this.gif = gifResponse &&  gifResponse.results && gifResponse.results.length > 0 ? gifResponse.results[0] : null;

            resolve();
        });
    }

    render() {
        return <article>
                <div class="gif-container ion-margin">
                    {this.renderGif()}
                </div>

                <h1 class="ion-text-center">{this.keywords[this.keywordIndex]}! Your presentation has been published.</h1>

                <p>Time to <a onClick={() => this.openShare.emit()}><strong>share</strong></a> it with the world, your colleagues, friends and community.</p>

                <ion-button color="tertiary" shape="round" onClick={() => this.openShare.emit()} class="ion-margin">
                    <ion-icon name="share" slot="start"></ion-icon>
                    <ion-label>Share</ion-label>
                </ion-button>

                <ion-label class="published-url ion-padding ion-text-center">Your presentation is available at the following address: <a href={this.publishedUrl} target="_blank">{this.publishedUrl}</a></ion-label>
            </article>
    }

    private renderGif() {
        if (this.gif && this.gif.media && this.gif.media.length > 0 && this.gif.media[0].tinygif && this.gif.media[0].tinygif.url) {
            return <deckgo-lazy-img imgSrc={this.gif.media[0].tinygif.url}
                                    imgAlt={this.gif.title ? this.gif.title : this.gif.media[0].tinygif.url}></deckgo-lazy-img>
        } else {
            return undefined;
        }
    }

}
