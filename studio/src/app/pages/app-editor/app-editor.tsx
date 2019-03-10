import {Component, State, Element} from '@stencil/core';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @State()
    private slides: any[] = [];

    private DEFAULT_TITLE: string = 'Click to add title';
    private DEFAULT_CONTENT: string = 'Click to add content';

    componentDidLoad() {
        this.addSlide();
    }

    private addSlide() {
        if (!document) {
            return;
        }

        const title = <h1 slot="title" class="untouched" contenteditable
                          onFocus={($event: FocusEvent) => this.touchOnFocus($event)}
                          onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
            {this.DEFAULT_TITLE}
        </h1>;

        const content = <p slot="content" class="untouched" contenteditable
                           onFocus={($event: FocusEvent) => this.touchOnFocus($event)}
                           onBlur={($event: FocusEvent) => this.unTouchOnBlur($event)}>
            {this.DEFAULT_CONTENT}
        </p>;

        const slide: any = <deckgo-slide-title>
            {title}
            {content}
        </deckgo-slide-title>;

        this.slides = [...this.slides, slide];
    }

    private async animatePrevNextSlide(next: boolean) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if (next) {
            await (deck as any).slideNext(false, false);
        } else {
            await (deck as any).slidePrev(false, false);
        }
    }

    private touchOnFocus($event: FocusEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!$event) {
                resolve();
                return;
            }

            if ($event.target && $event.target instanceof HTMLElement) {
                const element: HTMLElement = $event.target as HTMLElement;

                if (element.classList && element.classList.contains('untouched')) {
                    if (element.firstChild) {
                        element.removeChild(element.firstChild);
                    }

                    element.classList.remove('untouched');
                }
            }

            resolve();
        });
    }

    private unTouchOnBlur($event: FocusEvent): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!$event || !document) {
                resolve();
                return;
            }

            if ($event.target && $event.target instanceof HTMLElement) {
                const element: HTMLElement = $event.target as HTMLElement;

                if (element.classList && !element.classList.contains('untouched') && !element.firstChild) {
                    element.appendChild(document.createTextNode(element.nodeName && element.nodeName.toLowerCase() === 'h1' ? this.DEFAULT_TITLE : this.DEFAULT_CONTENT));
                    element.classList.add('untouched');
                }
            }

            resolve();
        });
    }

    // TODO: SlideTo

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content padding>
                <main>
                    <deckgo-deck embedded={true}>
                        {this.renderSlides()}
                    </deckgo-deck>
                </main>
            </ion-content>,
            <ion-footer>
                <ion-toolbar>
                    <ion-buttons slot="start">
                        <ion-button onClick={() => this.animatePrevNextSlide(false)} color="primary">
                            <ion-icon slot="icon-only" name="arrow-back"></ion-icon>
                        </ion-button>

                        <ion-button onClick={() => this.animatePrevNextSlide(true)} color="primary">
                            <ion-icon slot="icon-only" name="arrow-forward"></ion-icon>
                        </ion-button>

                        <ion-button onClick={() => this.addSlide()} color="primary">
                            <ion-icon slot="icon-only" name="bookmark"></ion-icon>
                        </ion-button>
                    </ion-buttons>

                    <ion-buttons slot="end">
                        <ion-button onClick={() => this.addSlide()} color="primary" shape="round" size="small">
                            <ion-label>Add slide</ion-label>
                        </ion-button>
                    </ion-buttons>
                </ion-toolbar>
            </ion-footer>,
            <deckgo-inline-editor></deckgo-inline-editor>
        ];
    }

    private renderSlides() {
        return (
            this.slides.map((slide: any) => {
                return slide
            })
        );
    }
}
