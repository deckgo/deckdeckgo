import {Component, Element, Listen, Prop, State} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {take} from 'rxjs/operators';

import {SlideTemplate} from '../../models/slide-template';
import {EditorUtils} from '../../utils/editor-utils';

import {User} from '../../models/user';

import {EditorHelper} from '../../helpers/editor/editor.helper';
import {AuthService} from '../../services/auth/auth.service';
import {GuestService} from '../../services/guest/guest.service';
import {NavDirection, NavService} from '../../services/nav/nav.service';

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

    @State()
    private displaying: boolean = false;

    private editorHelper: EditorHelper = new EditorHelper();

    private authService: AuthService;
    private guestService: GuestService;
    private navService: NavService;

    constructor() {
        this.authService = AuthService.getInstance();
        this.guestService = GuestService.getInstance();
        this.navService = NavService.getInstance();
    }

    async componentWillLoad() {
        this.editorHelper.init(this.el);

        this.authService.watch().pipe(take(1)).subscribe(async (user: User) => {
            if(!user) {
                await this.authService.signInAnonymous();
            }

            await this.initSlide();
        });
    }

    async componentDidLoad() {
        await this.initSlideSize();

        await this.updateInlineEditorListener();
    }

    componentDidUnload() {
        this.editorHelper.destroy();
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

    private updateInlineEditorListener(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                return;
            }

            const inlineEditor: HTMLElement = this.el.querySelector('deckgo-inline-editor');

            if (!inlineEditor) {
                return;
            }

            (inlineEditor as any).attachTo = deck;

            resolve();
        });
    }

    private initSlide(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!document) {
                resolve();
                return;
            }

            const slide: any = await EditorUtils.createSlide(SlideTemplate.TITLE);

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

    @Listen('document:mouseInactivity')
    inactivity($event: CustomEvent) {
        this.displaying = !$event.detail;
    }

    @Listen('keyup')
    async onKeyup(e: KeyboardEvent) {
        if (e && e.key === 'Tab' && document && document.activeElement && document.activeElement instanceof HTMLElement) {
            await this.touchToolbar(document.activeElement);
        }
    }

    @Listen('document:keydown')
    async onKeydown(e: KeyboardEvent) {
        if (e && e.key === 'Escape') {
            await this.selectDeck();
        }
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

    @Listen('actionOpenSlideAdd')
    async onActionOpenSlideAdd($event: CustomEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const couldAddSlide: boolean = await this.guestService.couldAddSlide(this.slides);

        if (!couldAddSlide) {
            await this.signIn();
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-slide-type',
            event: $event.detail,
            mode: 'ios',
            cssClass: 'app-slide-type'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data.slide) {
                await this.concatSlide(detail.data.slide);

                await this.slideToLastSlide();
            }
        });

        await popover.present();
    }

    @Listen('actionPublish')
    async onActionPublish() {
        // No slides, no publish
        if (!this.slides || this.slides.length <= 0) {
            return;
        }

        const couldAddSlide: boolean = await this.guestService.couldPublish(this.slides);

        if (!couldAddSlide) {
            await this.signIn();
            return;
        }

        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-publish'
        });

        modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
            // TODO Publish or publish from the modal and do nothing here?
        });

        await modal.present();
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
            if (!$event) {
                resolve();
                return;
            }

            if (!$event.target || !($event.target instanceof HTMLElement)) {
                resolve();
                return;
            }

            const element: HTMLElement = $event.target as HTMLElement;

            await this.touchToolbar(element);

            resolve();
        });
    }

    private selectDeck(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

            if (toolbar) {
                await toolbar.blurSelectedElement();
                await toolbar.unSelect();
            }

            await this.blockSlide(false);

            resolve();
        });
    }

    private touchToolbar(element: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const toolbar: HTMLAppEditorToolbarElement = this.el.querySelector('app-editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            await toolbar.touch(element);

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
            await (deck as any).toggleKeyboardAssist(!blockState);

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

    private async signIn() {
        this.navService.navigate({
            url: '/login/editor',
            direction: NavDirection.FORWARD
        });
    }

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content padding>
                <main class={this.displaying ? 'idle' : undefined}>
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
            <ion-footer class={this.displaying ? 'idle' : undefined}>
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
                        <app-add-slide-action></app-add-slide-action>
                    </ion-buttons>
                </ion-toolbar>
            </ion-footer>,
            <deckgo-inline-editor>
                <ion-icon ios="ios-link" md="ios-link" slot="link"></ion-icon>
            </deckgo-inline-editor>
        ];
    }
}
