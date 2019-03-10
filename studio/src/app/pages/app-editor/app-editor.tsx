import {Component, State, Element, Prop} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @Prop({connect: 'ion-modal-controller'}) modalController: HTMLIonModalControllerElement;

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

    private async slideTo(index: number, speed?: number | undefined) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        await (deck as any).slideTo(index, speed);
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

    private async openSlidePicker() {
        const slidesTitle: string[] = await this.getSlidesTitle();

        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-slide-picker',
            componentProps: {
                slides: slidesTitle
            }
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data >= 0) {
                await this.slideTo(detail.data);
            }
        });

        await modal.present();
    }

    private getSlidesTitle(): Promise<string[]> {
        return new Promise<string[]>((resolve) => {
            const results: string[] = [];

            const slides: NodeListOf<HTMLElement> = this.el.querySelectorAll('deckgo-deck > *');

            if (slides) {
                for (const slide of Array.from(slides)) {
                    if (slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                        const title = slide.querySelector('[slot="title"]');

                        if (title) {
                            results.push(title.innerHTML);
                        }
                    }
                }
            }

            resolve(results);
        });
    }

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

                        <ion-button onClick={() => this.openSlidePicker()} color="primary">
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
