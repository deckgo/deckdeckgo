import {SlideTemplate} from '../models/slide-template';

export enum SlotType {
    P = 'p',
    H1 = 'h1',
    H2 = 'h2',
    H3 = 'h3'
}

export class EditorUtils {

    private static DEFAULT_TITLE: string = 'Click to add title';
    private static DEFAULT_CONTENT: string = 'Click to add content';
    private static DEFAULT_CAPTION: string = 'Click to add a caption';

    static createSlide(template: SlideTemplate): Promise<any> {
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
                resolve(await this.createSlideGif());
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

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

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

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

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

            const start = <p slot="start" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const end = <p slot="end" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </p>;

            const slide: any = <deckgo-slide-split>
                {start}
                {end}
            </deckgo-slide-split>;

            resolve(slide);
        });
    }

    private static createSlideGif(): Promise<any> {
        return new Promise<any>((resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const title = <h1 slot="header" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CAPTION}
            </h1>;

            const content = <h2 slot="footer" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CAPTION}
            </h2>;

            const slide: any = <deckgo-slide-gif src="./assets/img/example.gif">
                {title}
                {content}
            </deckgo-slide-gif>;

            resolve(slide);
        });
    }

}
