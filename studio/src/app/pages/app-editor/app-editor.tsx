import {Component, Element, Prop, State} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {DeckdeckgoStudioUtils} from '../../utils/deckdeckgo-studio-utils';
import {DeckdeckgoSlideTemplate} from '../../utils/deckdeckgo-slide-template';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @Prop({connect: 'ion-modal-controller'}) modalController: HTMLIonModalControllerElement;
    @Prop({connect: 'ion-popover-controller'}) popoverController: HTMLIonPopoverControllerElement;

    @State()
    private slides: any[] = [];

    async componentDidLoad() {
        await this.initSlide();
    }

    private initSlide(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const slide: any = await DeckdeckgoStudioUtils.createSlide(DeckdeckgoSlideTemplate.TITLE);

            await this.concatSlide(slide);

            resolve();
        });
    }

    private concatSlide(slide: any): Promise<void> {
        return new Promise<void>((resolve) => {
            this.slides = [...this.slides, slide];

            resolve();
        });
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

    private async slideToLastSlide() {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if (deck.hasChildNodes()) {
            await this.slideTo(deck.children.length);
        }
    }

    private async openSlideNavigate() {
        const slidesTitle: string[] = await this.getSlidesTitle();

        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-slide-navigate',
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

    private async openSlideAdd($event: UIEvent) {
        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-slide-add',
            event: $event,
            mode: 'ios',
            cssClass: 'app-slide-add'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data && detail.data.slide) {
                await this.concatSlide(detail.data.slide);

                if (detail.data.swipe) {
                    await this.slideToLastSlide();
                }
            }
        });

        await popover.present();
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

                        <ion-button onClick={() => this.openSlideNavigate()} color="primary">
                            <ion-icon slot="icon-only" ios="ios-list" md="ios-list"></ion-icon>
                        </ion-button>
                    </ion-buttons>

                    <ion-buttons slot="end">
                        <ion-button onClick={(e: UIEvent) => this.openSlideAdd(e)} color="primary" shape="round"
                                    size="small">
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
