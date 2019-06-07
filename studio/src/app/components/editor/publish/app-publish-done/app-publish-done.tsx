import {Component, h, State} from '@stencil/core';

import {GifService} from '../../../../services/api/gif/gif.service';

@Component({
    tag: 'app-publish-done',
    styleUrl: 'app-publish-done.scss'
})
export class AppPublishDone {

    private gifService: GifService;

    @State()
    private gif: TenorGif;

    constructor() {
        this.gifService = GifService.getInstance();
    }

    async componentWillLoad() {
        await this.initRandomGifUrl();
    }

    private initRandomGifUrl(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const gifResponse: TenorSearchResponse = await this.gifService.getRandomGif('hooray');

            this.gif = gifResponse &&  gifResponse.results && gifResponse.results.length > 0 ? gifResponse.results[0] : null;

            resolve();
        });
    }


    render() {
        return <main class="ion-padding">
            {this.renderGif()}
        </main>
    }

    private renderGif() {
        if (this.gif && this.gif.media && this.gif.media.length > 0 && this.gif.media[0].tinygif && this.gif.media[0].tinygif.url) {
            return <div class="gif-container">
                <deckgo-lazy-img imgSrc={this.gif.media[0].tinygif.url}
                                 imgAlt={this.gif.title ? this.gif.title : this.gif.media[0].tinygif.url}></deckgo-lazy-img>
            </div>
        } else {
            return undefined;
        }
    }

}
