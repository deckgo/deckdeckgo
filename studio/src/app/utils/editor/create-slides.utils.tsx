import {h} from '@stencil/core';

import {SlideTemplate} from '../../models/data/slide';

import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';
import {User, UserSocial} from '../../models/data/user';

export enum SlotType {
    SECTION = 'section',
    H1 = 'h1',
    H2 = 'h2',
    H3 = 'h3',
    CODE = 'deckgo-highlight-code'
}

export class CreateSlidesUtils {

    static createSlide(template: SlideTemplate, user?: User): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve(null);
                return;
            }

            if (template === SlideTemplate.TITLE) {
                resolve(await this.createSlideTitle());
            } else if (template === SlideTemplate.CONTENT) {
                resolve(await this.createSlideContent());
            } else if (template === SlideTemplate.SPLIT) {
                resolve(await this.createSlideSplit());
            } else if (template === SlideTemplate.GIF) {
                resolve(await this.createSlideGif(EnvironmentConfigService.getInstance().get('gifExampleSrc')));
            } else if (template === SlideTemplate.AUTHOR) {
                resolve(await this.createSlideAuthor(user));
            } else {
                resolve(null);
            }
        });
    }

    private static createSlideTitle(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const content = <section slot="content"></section>;

            const slide: any = <deckgo-slide-title>
                {title}
                {content}
            </deckgo-slide-title>;

            resolve(slide);
        });
    }

    private static createSlideContent(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title"></h1>;

            const content = <section slot="content"></section>;

            const slide: any = <deckgo-slide-content>
                {title}
                {content}
            </deckgo-slide-content>;

            resolve(slide);
        });
    }

    private static createSlideSplit(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const start = <section slot="start"></section>;

            const end = <section slot="end"></section>;

            const slide: any = <deckgo-slide-split>
                {start}
                {end}
            </deckgo-slide-split>;

            resolve(slide);
        });
    }

    static createSlideGif(src: string): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h2 slot="header"></h2>;

            const content = <h3 slot="footer"></h3>;

            const slide: any = <deckgo-slide-gif src={src}>
                {title}
                {content}
            </deckgo-slide-gif>;

            resolve(slide);
        });
    }

    private static createSlideAuthor(user: User): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="title">Author</h1>;

            const author = <section slot="author"></section>;

            const imgSrc: string = user && user.data && user.data.photo_url ? user.data.photo_url : undefined;
            const name: string = user && user.data && user.data.name ? user.data.name : 'Author';

            const links = await this.createSocialLinks(user);

            const slide: any = <deckgo-slide-author img-src={imgSrc} img-alt={name}>
                {title}
                {author}
                {links}
            </deckgo-slide-author>;

            resolve(slide);
        });
    }

    private static createSocialLinks(user: User): Promise<any[]> {
        return new Promise<any[]>((resolve) => {
            const links = [];

            if (user && user.data && user.data.social) {
                const userSocial: UserSocial = user.data.social;

                if (userSocial.twitter && userSocial.twitter !== '' && userSocial.twitter !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} twitter={user.data.social.twitter}>
                        <ion-icon src="/assets/icons/ionicons/twitter.svg" area-label="Twitter"></ion-icon>
                        Twitter
                    </deckgo-social>);
                }

                if (userSocial.linkedin && userSocial.linkedin !== '' && userSocial.linkedin !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} linkedin={user.data.social.linkedin}>
                        <ion-icon src="/assets/icons/ionicons/linkedin.svg" area-label="LinkedIn"></ion-icon>
                        LinkedIn
                    </deckgo-social>);
                }

                if (userSocial.dev && userSocial.dev !== '' && userSocial.dev !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} dev={user.data.social.dev}>
                        <ion-icon src="/assets/icons/dev.svg" area-label="Dev"></ion-icon>
                        Dev
                    </deckgo-social>);
                }

                if (userSocial.medium && userSocial.medium !== '' && userSocial.medium !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} medium={user.data.social.medium}>
                        <ion-icon src="/assets/icons/medium.svg" area-label="Medium"></ion-icon>
                        Medium
                    </deckgo-social>);
                }

                if (userSocial.github && userSocial.github !== '' && userSocial.github !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} github={user.data.social.github}>
                        <ion-icon src="/assets/icons/ionicons/github.svg" area-label="GitHub"></ion-icon>
                        GitHub
                    </deckgo-social>);
                }

                if (userSocial.custom && userSocial.custom !== '' && userSocial.custom !== undefined) {
                    links.push(<deckgo-social slot={`social-link`} fullUrl={user.data.social.custom}>
                        <ion-icon src="/assets/icons/ionicons/globe.svg" area-label="Web"></ion-icon>
                        Web
                    </deckgo-social>);
                }
            }

            resolve(links);
        });
    }

}
