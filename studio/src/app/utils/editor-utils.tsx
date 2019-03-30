import {SlideTemplate} from '../models/slide-template';

export enum SlotType {
    DIV = 'div',
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
                resolve(await this.createSlideGif('./assets/img/example.gif'));
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

            const content = <div slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </div>;

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

            const content = <div slot="content" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </div>;

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

            const start = <div slot="start" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </div>;

            const end = <div slot="end" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CONTENT}
            </div>;

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

            const title = <h2 slot="header" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CAPTION}
            </h2>;

            const content = <h3 slot="footer" class="deckgo-untouched" contenteditable>
                {this.DEFAULT_CAPTION}
            </h3>;

            const slide: any = <deckgo-slide-gif src={src}>
                {title}
                {content}
            </deckgo-slide-gif>;

            resolve(slide);
        });
    }

}
