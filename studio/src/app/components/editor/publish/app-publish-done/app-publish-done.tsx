import {Component, Element, h, Prop, State} from '@stencil/core';

import {take} from 'rxjs/operators';

import 'web-social-share';

import {GifService} from '../../../../services/tenor/gif/gif.service';

import {Deck} from '../../../../models/deck';
import {AuthUser} from '../../../../models/auth-user';

import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';
import {AuthService} from '../../../../services/data/auth/auth.service';

@Component({
    tag: 'app-publish-done',
    styleUrl: 'app-publish-done.scss'
})
export class AppPublishDone {

    @Element() el: HTMLElement;

    private gifService: GifService;

    private deckEditorService: DeckEditorService;
    private authService: AuthService;

    @Prop()
    publishedUrl: string;

    @State()
    private gif: TenorGif;

    private keywords: string[] = ['Hooray', 'You did it', 'Applause', 'Thumbs up'];

    @State()
    private keywordIndex: number = Math.floor(Math.random() * 4);

    constructor() {
        this.gifService = GifService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
        this.authService = AuthService.getInstance();
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

    private openShare(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            // @ts-ignore
            if (navigator && navigator.share) {
                await this.shareMobile();
            } else {
                await this.shareDesktop();
            }

            resolve();
        });
    }

    private shareMobile() {
        return new Promise(async (resolve) => {
            const text: string = await this.getShareText();

            // @ts-ignore
            await navigator.share({
                text: text,
                url: this.publishedUrl,
            });

            resolve();
        });
    }

    private shareDesktop() {
        return new Promise(async (resolve) => {
            const webSocialShare = this.el.querySelector('web-social-share');

            if (!webSocialShare || !window) {
                return;
            }

            const shareOptions = {
                displayNames: true,
                config: [{
                    twitter: {
                        socialShareUrl: this.publishedUrl,
                        socialSharePopupWidth: 300,
                        socialSharePopupHeight: 400
                    }
                },{
                    linkedin: {
                        socialShareUrl: this.publishedUrl
                    }
                },{
                    email: {
                        socialShareBody: this.publishedUrl
                    }
                }, {
                    whatsapp: {
                        socialShareUrl: this.publishedUrl
                    }
                }, {
                    copy: {
                        socialShareUrl: this.publishedUrl
                    }
                }]
            };

            webSocialShare.share = shareOptions;

            webSocialShare.show = true;

            resolve();
        });
    }

    private getShareText(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.name && deck.name !== '') {
                    this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                        if (authUser && !authUser.anonymous && authUser.name && authUser.name !== '') {
                            resolve(`"${deck.name}" by ${authUser.name} created with DeckDeckGo`);
                        } else {
                            resolve(`"${deck.name}" created with DeckDeckGo`);
                        }
                    });
                } else {
                    resolve('A presentation created with DeckDeckGo');
                }
            });
        });
    }

    render() {
        return [
            <article>
                <div class="gif-container ion-margin">
                    {this.renderGif()}
                </div>

                <h1>{this.keywords[this.keywordIndex]}! Your presentation has been published.</h1>

                <p>Time to <a onClick={() => this.openShare()}><strong>share</strong></a> it with the world, your colleagues, friends and community.</p>

                <ion-button color="tertiary" shape="round" onClick={() => this.openShare()} class="ion-margin">
                    <ion-icon name="share" slot="start"></ion-icon>
                    <ion-label>Share</ion-label>
                </ion-button>

                <ion-label class="published-url ion-padding ion-text-center">Your presentation is available at the following address: <a href={this.publishedUrl} target="_blank">{this.publishedUrl}</a></ion-label>
            </article>,
            <web-social-share show={false}>
                <ion-icon name="logo-twitter" slot="twitter" style={{color: "#00aced", width: "1.4rem", height: "1.4rem"}}></ion-icon>
                <ion-icon name="logo-linkedin" slot="linkedin" style={{color: "#0077b5", width: "1.4rem", height: "1.4rem"}}></ion-icon>
                <ion-icon name="mail" slot="email" style={{color: "var(--ion-color-tertiary)", width: "1.4rem", height: "1.4rem"}}></ion-icon>
                <ion-icon name="logo-whatsapp" slot="whatsapp" style={{color: "#25D366", width: "1.4rem", height: "1.4rem"}}></ion-icon>
                <ion-icon name="copy" slot="copy" style={{width: "1.4rem", height: "1.4rem"}}></ion-icon>
            </web-social-share>
        ]
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
