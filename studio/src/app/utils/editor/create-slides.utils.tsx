import {h, JSX} from '@stencil/core';

import uuid from 'uuid/v4';

import {SlideAttributes, SlideTemplate} from '../../models/data/slide';

import {EnvironmentDeckDeckGoConfig} from '../../services/core/environment/environment-config';
import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

import {User, UserSocial} from '../../models/data/user';
import {Deck} from '../../models/data/deck';

import {QRCodeUtils} from './qrcode.utils';

import {DEFAULT_FIRST_SLIDE_PALETTE} from './random-palette';

export class CreateSlidesUtils {

    static createSlide(template: SlideTemplate, randomColor: boolean, deck?: Deck, user?: User): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            if (!document) {
                resolve(null);
                return;
            }

            if (template === SlideTemplate.TITLE) {
                resolve(await this.createSlideTitle(randomColor));
            } else if (template === SlideTemplate.CONTENT) {
                resolve(await this.createSlideContent());
            } else if (template === SlideTemplate.SPLIT) {
                resolve(await this.createSlideSplit());
            } else if (template === SlideTemplate.GIF) {
                resolve(await this.createSlideGif(EnvironmentConfigService.getInstance().get('gifExampleSrc')));
            } else if (template === SlideTemplate.AUTHOR) {
                resolve(await this.createSlideAuthor(user));
            } else if (template === SlideTemplate.YOUTUBE) {
                resolve(await this.createSlideYoutube());
            } else if (template === SlideTemplate.QRCODE) {
                resolve(await this.createSlideQRCode(deck));
            } else if (template === SlideTemplate.CHART) {
                resolve(await this.createSlideChart());
                resolve(await this.createSlideQRCode(deck));
            } else if (template === SlideTemplate.POLL) {
                resolve(await this.createSlidePoll());
            } else {
                resolve(null);
            }
        });
    }

    private static createSlideTitle(randomColor: boolean): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const content = <section slot="content"></section>;

            const style = await this.getRandomStyleColors(randomColor);

            const slide: JSX.IntrinsicElements = <deckgo-slide-title key={uuid()} {...style}>
                {title}
                {content}
            </deckgo-slide-title>;

            resolve(slide);
        });
    }

    private static getRandomStyleColors(randomColor: boolean): Promise<any | undefined> {
        return new Promise<any | undefined>((resolve) => {
            if (!randomColor) {
                resolve(undefined);
                return;
            }

            if (!DEFAULT_FIRST_SLIDE_PALETTE || DEFAULT_FIRST_SLIDE_PALETTE.length <= 0) {
                resolve(undefined);
                return;
            }

            const index: number = Math.floor(Math.random() * DEFAULT_FIRST_SLIDE_PALETTE.length);

            resolve({
                style: {
                    '--color': DEFAULT_FIRST_SLIDE_PALETTE[index].contrast.rgba,
                    '--background': DEFAULT_FIRST_SLIDE_PALETTE[index].color.rgba
                }
            });
        });
    }

    private static createSlideContent(): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const content = <section slot="content"></section>;

            const slide: JSX.IntrinsicElements = <deckgo-slide-content key={uuid()}>
                {title}
                {content}
            </deckgo-slide-content>;

            resolve(slide);
        });
    }

    public static createSlideSplit(attributes: SlideAttributes = undefined): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const start = <section slot="start"></section>;

            const end = <section slot="end"></section>;

            // @ts-ignore
            const slide: JSX.IntrinsicElements = <deckgo-slide-split key={uuid()} {...attributes}>
                {start}
                {end}
            </deckgo-slide-split>;

            resolve(slide);
        });
    }

    static createSlideGif(src: string): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h2 slot="header"></h2>;

            const content = <h3 slot="footer"></h3>;

            const slide: JSX.IntrinsicElements = <deckgo-slide-gif src={src} key={uuid()}>
                {title}
                {content}
            </deckgo-slide-gif>;

            resolve(slide);
        });
    }

    private static createSlideAuthor(user: User): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title">Author</h1>;

            const name: string = user && user.data && user.data.name && user.data.name !== undefined && user.data.name !== '' ? user.data.name : undefined;
            const bio: string = user && user.data && user.data.bio && user.data.bio !== undefined && user.data.bio !== '' ? user.data.bio : undefined;

            const author = <section slot="author">
                {name !== undefined ? <div>{name}{bio ? <div><br/></div> : undefined}</div> : undefined}
                {bio !== undefined ? <div><small>{bio}</small></div> : undefined}
            </section>;

            const imgSrc: string = user && user.data && user.data.photo_url ? user.data.photo_url : undefined;
            const imgAlt: string = user && user.data && user.data.name ? user.data.name : 'Author';

            const links = await this.createSocialLinks(user);

            const slide: JSX.IntrinsicElements = <deckgo-slide-author key={uuid()} img-src={imgSrc} img-alt={imgAlt}>
                {title}
                {author}
                {links}
            </deckgo-slide-author>;

            resolve(slide);
        });
    }

    private static createSocialLinks(user: User): Promise<JSX.IntrinsicElements[]> {
        return new Promise<JSX.IntrinsicElements[]>((resolve) => {
            const links = [];

            if (user && user.data && user.data.social) {
                const userSocial: UserSocial = user.data.social;

                const config: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

                if (userSocial.twitter && userSocial.twitter !== '' && userSocial.twitter !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} twitter={user.data.social.twitter}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/ionicons/twitter.svg`} aria-label="Twitter"></deckgo-lazy-img>
                        Twitter
                    </deckgo-social>);
                }

                if (userSocial.linkedin && userSocial.linkedin !== '' && userSocial.linkedin !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} linkedin={user.data.social.linkedin}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/ionicons/linkedin.svg`} aria-label="LinkedIn"></deckgo-lazy-img>
                        LinkedIn
                    </deckgo-social>);
                }

                if (userSocial.dev && userSocial.dev !== '' && userSocial.dev !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} dev={user.data.social.dev}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/dev.svg`} aria-label="Dev"></deckgo-lazy-img>
                        Dev
                    </deckgo-social>);
                }

                if (userSocial.medium && userSocial.medium !== '' && userSocial.medium !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} medium={user.data.social.medium}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/medium.svg`} aria-label="Medium"></deckgo-lazy-img>
                        Medium
                    </deckgo-social>);
                }

                if (userSocial.github && userSocial.github !== '' && userSocial.github !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} github={user.data.social.github}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/ionicons/github.svg`} aria-label="GitHub"></deckgo-lazy-img>
                        GitHub
                    </deckgo-social>);
                }

                if (userSocial.custom && userSocial.custom !== '' && userSocial.custom !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} fullUrl={user.data.social.custom}>
                        <deckgo-lazy-img svg-src={`${config.globalAssetsUrl}/icons/ionicons/globe.svg`} aria-label="Web"></deckgo-lazy-img>
                        Web
                    </deckgo-social>);
                }
            }

            resolve(links);
        });
    }

    static createSlideYoutube(src: string = undefined): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const slide: JSX.IntrinsicElements = <deckgo-slide-youtube key={uuid()} src={src}>
                {title}
            </deckgo-slide-youtube>;

            resolve(slide);
        });
    }

    private static createSlideQRCode(deck: Deck): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const content: string = QRCodeUtils.getPresentationUrl(deck);

            const slide: JSX.IntrinsicElements = <deckgo-slide-qrcode key={uuid()} content={content} img-src={`${EnvironmentConfigService.getInstance().get('deckdeckgo').globalAssetsUrl}/img/deckdeckgo-logo.svg`}>
                {title}
            </deckgo-slide-qrcode>;

            resolve(slide);
        });
    }

    static createSlideChart(attributes: SlideAttributes = undefined): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            // @ts-ignore
            const slide: JSX.IntrinsicElements = <deckgo-slide-chart key={uuid()} {...attributes}>
                {title}
            </deckgo-slide-chart>;

            resolve(slide);
        });
    }

    static createSlidePoll(question: string = undefined, answers: string[] = undefined): Promise<JSX.IntrinsicElements> {
        return new Promise<JSX.IntrinsicElements>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const questionSlot = <h1 slot="question">{question}</h1>;

            const answerSlots = [];
            answers.forEach((answer: string, i: number) => {
                answerSlots.push(<h2 slot={`answer-${i + 1}`}>{answer}</h2>);
            });

            const deckDeckGoConfig: EnvironmentDeckDeckGoConfig = EnvironmentConfigService.getInstance().get('deckdeckgo');

            const slide: JSX.IntrinsicElements = <deckgo-slide-poll key={uuid()} pollLink={deckDeckGoConfig.pollUrl} pollServer={deckDeckGoConfig.pollServerUrl}>
                {questionSlot}
                {...answerSlots}

                <div slot="how_to">Go to <a href="https://poll.deckdeckgo.com">poll.deckdeckgo.com</a> and use the code {'{0}'}</div>
                <div slot="awaiting_votes">Awaiting votes</div>
            </deckgo-slide-poll>;

            resolve(slide);
        });
    }

}
