import {Component, Element, Listen, State} from '@stencil/core';

import {EditorUtils} from '../../../utils/editor-utils';

import {GifService} from '../../../services/gif/gif.service';

@Component({
    tag: 'app-gif',
    styleUrl: 'app-gif.scss'
})
export class AppGif {

    @Element() el: HTMLElement;

    private gifService: GifService;

    @State()
    private gifsOdd: TenorGif[];

    @State()
    private gifsEven: TenorGif[];

    constructor() {
        this.gifService = GifService.getInstance();
    }

    async componentDidLoad() {
        history.pushState({modal: true}, null);

        const gifs: TenorGif[] = await this.gifService.getTrending();
        this.gifsOdd = gifs.filter((_a, i) => i % 2);
        this.gifsEven = gifs.filter((_a, i) => !(i % 2));
    }

    @Listen('window:popstate')
    async handleHardwareBackButton(_e: PopStateEvent) {
        await this.closeModal();
    }

    async closeModal() {
        await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss();
    }

    private addSlide(gif: TenorGif): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const url: string = gif.media[0].gif.url;

            const slide: any = await EditorUtils.createSlideGif(url);
            await (this.el.closest('ion-modal') as HTMLIonModalElement).dismiss(slide);

            resolve();
        });
    }

    render() {
        return [
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.closeModal()}>
                            <ion-icon name="close"></ion-icon>
                        </ion-button>
                    </ion-buttons>
                    <ion-title class="ion-text-uppercase">Pick a Gif</ion-title>
                </ion-toolbar>
            </ion-header>,
            <ion-content padding>
                <div class="gifs-container">
                    <div class="gifs-column">
                        {this.renderGifs(this.gifsOdd)}
                    </div>
                    <div class="gifs-column">
                        {this.renderGifs(this.gifsEven)}
                    </div>
                </div>
            </ion-content>,
            <ion-footer>
                <ion-toolbar>
                    <ion-searchbar debounce={500} placeholder="Search Tenor"></ion-searchbar>
                </ion-toolbar>
            </ion-footer>
        ];
    }

    private renderGifs(gifs: TenorGif[]) {
        if (gifs && gifs.length > 0) {
            return (
                gifs.map((gif: TenorGif) => {
                    if (gif.media && gif.media.length > 0
                        && gif.media[0].tinygif && gif.media[0].tinygif.url
                        && gif.media[0].gif && gif.media[0].gif.url) {
                        return <div custom-tappable onClick={() => this.addSlide(gif)}>
                            <img src={gif.media[0].tinygif.url} alt={gif.title ? gif.title : gif.url}></img>
                        </div>
                    } else {
                        return undefined;
                    }
                })
            );
        } else {
            return undefined;
        }
    }

}
