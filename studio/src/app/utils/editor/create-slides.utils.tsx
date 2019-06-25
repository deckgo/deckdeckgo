import {h} from '@stencil/core';

import {ApiSlideTemplate} from '../../models/api/api.slide';

import {EnvironmentConfigService} from '../../services/core/environment/environment-config.service';

export enum SlotType {
    SECTION = 'section',
    H1 = 'h1',
    H2 = 'h2',
    H3 = 'h3',
    CODE = 'deckgo-highlight-code'
}

export class CreateSlidesUtils {

    static createSlide(template: ApiSlideTemplate): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve(null);
                return;
            }

            if (template === ApiSlideTemplate.TITLE) {
                resolve(await this.createSlideTitle());
            } else if (template === ApiSlideTemplate.CONTENT) {
                resolve(await this.createSlideContent());
            } else if (template === ApiSlideTemplate.SPLIT) {
                resolve(await this.createSlideSplit());
            } else if (template === ApiSlideTemplate.GIF) {
                resolve(await this.createSlideGif(EnvironmentConfigService.getInstance().get('gifExampleSrc')));
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

}
