import {Component, Element, Listen, Prop, State} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {DeckdeckgoStudioCreateSlide} from '../../utils/deckdeckgo-studio-create-slide';
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

    private slideIndex: number = 0;

    async componentWillLoad() {
        await this.initSlide();
    }

    async componentDidLoad() {
        await this.initSlideSize();
    }

    private initSlideSize(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                return;
            }

            await (deck as any).initSlideSize();

            resolve();
        });
    }

    private initSlide(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const slide: any = await DeckdeckgoStudioCreateSlide.createSlide(DeckdeckgoSlideTemplate.TITLE);

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
            if (detail.data.slide) {
                await this.concatSlide(detail.data.slide);

                await this.slideToLastSlide();
            }
        });

        await popover.present();
    }

    private hideToolbar(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            const deckIndex: number = await (deck as any).getActiveIndex();

            if (deckIndex === this.slideIndex) {
                resolve();
                return;
            }

            await toolbar.hideToolbar();
            this.slideIndex = deckIndex;

            resolve();
        });
    }

    private deckTouched($event: MouseEvent | TouchEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            await toolbar.touch($event);

            resolve();
        });
    }

    @Listen('blockSlide')
    async onBlockSlide($event: CustomEvent) {
        await this.blockSlide($event.detail);
    }

    private blockSlide(blockState: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            await (deck as any).blockSlide(blockState);

            resolve();
        });
    }

    @Listen('deleteSlide')
    async onDeleteSlide() {
        await this.deleteSlide();
    }

    private deleteSlide(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            await (deck as any).deleteActiveSlide();

            resolve();
        });
    }

    private toggleFullScreen(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            await (deck as any).toggleFullScreen();

            resolve();
        });
    }

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content padding>
                <main>
                    <deckgo-deck embedded={true}
                                 onMouseDown={(e: MouseEvent) => this.deckTouched(e)}
                                 onTouchStart={(e: TouchEvent) => this.deckTouched(e)}
                                 onSlideNextDidChange={() => this.hideToolbar()}
                                 onSlidePrevDidChange={() => this.hideToolbar()}
                                 onSlideToChange={() => this.hideToolbar()}>
                        {this.slides}
                    </deckgo-deck>
                    <app-editor-toolbar></app-editor-toolbar>
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

                        <ion-button onClick={() => this.toggleFullScreen()} color="primary">
                            <ion-icon slot="icon-only" name="expand"></ion-icon>
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
            <deckgo-inline-editor>
                <ion-icon ios="ios-link" md="ios-link" slot="link"></ion-icon>
            </deckgo-inline-editor>
        ];
    }
}
