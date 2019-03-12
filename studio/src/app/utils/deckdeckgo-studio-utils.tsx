import {DeckdeckgoSlideTemplate} from './deckdeckgo-slide-template';

export class DeckdeckgoStudioUtils {

    static DEFAULT_TITLE: string = 'Click to add title';
    static DEFAULT_CONTENT: string = 'Click to add content';

    static createSlide(template: DeckdeckgoSlideTemplate): Promise<any> {
        return new Promise<any>(async (resolve) => {
            if (!document) {
                resolve(null);
                return;
            }

            if (template === DeckdeckgoSlideTemplate.TITLE) {
                resolve(await this.createSlideTitle());
            } else if (template === DeckdeckgoSlideTemplate.CONTENT) {
                resolve(await this.createSlideContent());
            } else if (template === DeckdeckgoSlideTemplate.SPLIT) {
                resolve(await this.createSlideSplit());
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

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable
                              onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                              onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable
                               onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                               onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
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

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable
                              onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                              onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
                {this.DEFAULT_TITLE}
            </h1>;

            const content = <p slot="content" class="deckgo-untouched" contenteditable
                               onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                               onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
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

            const title = <h1 slot="title" class="deckgo-untouched" contenteditable
                              onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                              onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
                {this.DEFAULT_TITLE}
            </h1>;

            const start = <p slot="start" class="deckgo-untouched" contenteditable
                             onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                             onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
                {this.DEFAULT_CONTENT}
            </p>;

            const end = <p slot="end" class="deckgo-untouched" contenteditable
                           onMouseDown={($event: MouseEvent) => this.touchOnMouseDown($event)}
                           onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
                {this.DEFAULT_CONTENT}
            </p>;

            const slide: any = <deckgo-slide-split>
                {title}
                {start}
                {end}
            </deckgo-slide-split>;

            resolve(slide);
        });
    }

    private static touchOnMouseDown($event: MouseEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event) {
                resolve();
                return;
            }

            if ($event.target && $event.target instanceof HTMLElement) {
                const element: HTMLElement = $event.target as HTMLElement;

                if (element.classList && element.classList.contains('deckgo-untouched')) {
                    if (element.firstChild) {
                        element.removeChild(element.firstChild);
                    }

                    element.classList.remove('deckgo-untouched');

                    element.focus();
                }

                await this.dispatchEventTouch('elementTouched', element);
            }

            resolve();
        });
    }

    private static unTouchOnBlur($event: FocusEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event || !document) {
                resolve();
                return;
            }

            if ($event.target && $event.target instanceof HTMLElement) {
                const element: HTMLElement = $event.target as HTMLElement;

                if (element.classList && !element.classList.contains('deckgo-untouched') && !element.firstChild) {
                    element.appendChild(document.createTextNode(element.nodeName && element.nodeName.toLowerCase() === 'h1' ? DeckdeckgoStudioUtils.DEFAULT_TITLE : DeckdeckgoStudioUtils.DEFAULT_CONTENT));
                    element.classList.add('deckgo-untouched');
                }
            }

            resolve();
        });
    }

    private static dispatchEventTouch(eventName: string, element?: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const slot: HTMLElement = element ? await this.findSlottedElement(element) : null;

            const $event: CustomEvent = new CustomEvent(eventName, {
                bubbles: true,
                detail: slot
            });

            element.dispatchEvent($event);

            resolve();
        });
    }

    private static findSlottedElement(element: HTMLElement): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!element || !element.parentElement) {
                resolve(element);
                return;
            }

            if (element.getAttribute('slot')) {
                resolve(element);
                return;
            }

            const result: HTMLElement = await this.findSlottedElement(element.parentElement);

            resolve(result);
        });
    }

}
