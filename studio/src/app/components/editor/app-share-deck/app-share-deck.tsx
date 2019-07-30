import {Component, Element, h, Method} from '@stencil/core';

import {take} from 'rxjs/operators';

import 'web-social-share';

import {Deck} from '../../../models/data/deck';
import {AuthUser} from '../../../models/auth/auth.user';

import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {AuthService} from '../../../services/auth/auth.service';
import {EnvironmentConfigService} from '../../../services/core/environment/environment-config.service';

@Component({
    tag: 'app-share-deck',
    styleUrl: 'app-share-deck.scss',
    shadow: true
})
export class AppShareDeck {

    @Element() el: HTMLElement;

    private deckEditorService: DeckEditorService;
    private authService: AuthService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
        this.authService = AuthService.getInstance();
    }

    @Method()
    openShare(): Promise<void> {
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
            const publishedUrl: string = await this.getPublishedUrl();

            // @ts-ignore
            await navigator.share({
                text: text,
                url: publishedUrl,
            });

            resolve();
        });
    }

    private shareDesktop() {
        return new Promise(async (resolve) => {
            const webSocialShare = this.el.shadowRoot.querySelector('web-social-share');

            if (!webSocialShare || !window) {
                return;
            }

            const publishedUrl: string = await this.getPublishedUrl();

            const shareOptions = {
                displayNames: true,
                config: [{
                    twitter: {
                        socialShareUrl: publishedUrl,
                        socialSharePopupWidth: 300,
                        socialSharePopupHeight: 400
                    }
                },{
                    linkedin: {
                        socialShareUrl: publishedUrl
                    }
                },{
                    email: {
                        socialShareBody: publishedUrl
                    }
                }, {
                    whatsapp: {
                        socialShareUrl: publishedUrl
                    }
                }, {
                    copy: {
                        socialShareUrl: publishedUrl
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
                if (deck && deck.data && deck.data.name && deck.data.name !== '') {
                    this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
                        if (authUser && !authUser.anonymous && authUser.name && authUser.name !== '') {
                            resolve(`"${deck.data.name}" by ${authUser.name} created with DeckDeckGo`);
                        } else {
                            resolve(`"${deck.data.name}" created with DeckDeckGo`);
                        }
                    });
                } else {
                    resolve('A presentation created with DeckDeckGo');
                }
            });
        });
    }

    private getPublishedUrl(): Promise<string> {
        return new Promise<string>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.data && deck.data.meta && deck.data.meta.pathname && deck.data.meta.pathname !== '') {
                    const presentationUrl: string = EnvironmentConfigService.getInstance().get('presentationUrl');
                    resolve(presentationUrl + deck.data.meta.pathname);
                } else {
                    // Should not happens
                    resolve('https://deckdeckgo.com');
                }
            });
        });
    }

    render() {
        return <web-social-share show={false}>
            <ion-icon name="logo-twitter" slot="twitter" style={{color: "#00aced", "font-size": "1.6rem", "display": "block"}}></ion-icon>
            <ion-icon name="logo-linkedin" slot="linkedin" style={{color: "#0077b5", "font-size": "1.6rem", "display": "block"}}></ion-icon>
            <ion-icon name="mail" slot="email" style={{color: "var(--ion-color-tertiary)", "font-size": "1.6rem", "display": "block"}}></ion-icon>
            <ion-icon name="logo-whatsapp" slot="whatsapp" style={{color: "#25D366", "font-size": "1.6rem", "display": "block"}}></ion-icon>
            <ion-icon name="copy" slot="copy" style={{"font-size": "1.6rem", "display": "block"}}></ion-icon>
        </web-social-share>
    }

}
