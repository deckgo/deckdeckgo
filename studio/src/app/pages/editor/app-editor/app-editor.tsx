import {Component, Element, Listen, Prop, State} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {filter, take} from 'rxjs/operators';


import {CreateSlidesUtils} from '../../../utils/editor/create-slides.utils';

import {AuthUser} from '../../../models/auth-user';
import {Slide, SlideTemplate} from '../../../models/slide';
import {Deck} from '../../../models/deck';

import {ParseStyleUtils} from '../../../utils/editor/parse-style.utils';

import {DeckEventsHandler} from '../../../handlers/editor/deck-events/deck-events.handler';

import {AuthService} from '../../../services/auth/auth.service';
import {GuestService} from '../../../services/guest/guest.service';
import {NavDirection, NavService} from '../../../services/nav/nav.service';

import {EditorHelper} from '../../../helpers/editor/editor.helper';
import {DeckAction} from '../../../popovers/editor/app-deck-actions/deck-action';
import {DeckEditorService} from '../../../services/deck/deck-editor.service';

interface FirstSlideContent {
    title: string;
    content: string;
}

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @Prop({connect: 'ion-modal-controller'}) modalController: HTMLIonModalControllerElement;
    @Prop({connect: 'ion-popover-controller'}) popoverController: HTMLIonPopoverControllerElement;

    @Prop()
    deckId: string;

    @State()
    private slides: any[] = [];

    private slideIndex: number = 0;

    @State()
    private displaying: boolean = false;

    private deckEventsHandler: DeckEventsHandler = new DeckEventsHandler();

    private authService: AuthService;
    private guestService: GuestService;
    private navService: NavService;

    private deckEditorService: DeckEditorService;
    private deckStyle: any;

    constructor() {
        this.authService = AuthService.getInstance();
        this.guestService = GuestService.getInstance();
        this.navService = NavService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
    }

    async componentWillLoad() {
        this.deckEventsHandler.init(this.el);

        // If no user create an anonymous one
        this.authService.watch().pipe(take(1)).subscribe(async (authUser: AuthUser) => {
            if (!authUser) {
                await this.authService.signInAnonymous();
            }
        });

        // As soon as we have got a user, an anonymous where the creation started above or an already used anonymous or a logged one, we init
        this.authService.watch().pipe(
            filter((authUser: AuthUser) => authUser !== null && authUser !== undefined),
            take(1)).subscribe(async (_authUser: AuthUser) => {

            if (!this.deckId) {
                await this.initSlide();
            } else {
                await this.fetchSlides();
            }
        });
    }

    async componentDidLoad() {
        await this.initSlideSize();

        await this.updateInlineEditorListener();
    }

    componentDidUnload() {
        this.deckEventsHandler.destroy();
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

            const slide: any = await CreateSlidesUtils.createSlide(SlideTemplate.TITLE);

            await this.concatSlide(slide);

            resolve();
        });
    }

    private fetchSlides(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!document || !this.deckId) {
                resolve();
                return;
            }

            const helper: EditorHelper = new EditorHelper();
            const slides: Slide[] = await helper.loadDeckAndRetrieveSlides(this.deckId);

            if (slides && slides.length > 0) {
                this.slides = [...slides];
            }

            await this.initDeckStyle();

            resolve();
        });
    }

    private initDeckStyle(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                if (deck && deck.attributes && deck.attributes.style) {
                    this.deckStyle = await ParseStyleUtils.convertStyle(deck.attributes.style);
                } else {
                    this.deckStyle = undefined;
                }

                resolve();
            });
        });
    }

    private concatSlide(extraSlide: any): Promise<void> {
        return new Promise<void>((resolve) => {
            this.slides = [...this.slides, extraSlide];

            resolve();
        });
    }

    @Listen('document:mouseInactivity')
    async inactivity($event: CustomEvent) {
        this.displaying = !$event.detail;

        // Wait a bit for the display/repaint of the footer
        setTimeout(async () => {
            await this.initSlideSize();
        }, 100);
    }

    @Listen('keyup')
    async onKeyup($event: KeyboardEvent) {
        if ($event && $event.key === 'Tab' && document && document.activeElement && document.activeElement instanceof HTMLElement) {
            await this.touchToolbar(document.activeElement);
        }
    }

    @Listen('document:keydown')
    async onKeydown($event: KeyboardEvent) {
        if ($event && $event.key === 'Escape') {
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

    @Listen('slideDidLoad')
    async slideToLastSlideOnSlideLoad($event) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if ($event && $event.target && $event.target instanceof HTMLElement) {
            const newSlide: HTMLElement = $event.target;

            if (!newSlide.getAttribute('slide_id') && deck.hasChildNodes()) {
                await this.slideTo(deck.children && deck.children.length > 0 ? deck.children.length - 1 : 0);
            }
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
                        const title: HTMLElement = slide.querySelector('[slot="title"]');

                        if (title) {
                            results.push(title.innerHTML);
                        } else {
                            const start: HTMLElement = slide.querySelector('[slot="start"]');

                            if (start) {
                                results.push(start.textContent);
                            } else {
                                const end: HTMLElement = slide.querySelector('[slot="end"]');

                                if (end) {
                                    results.push(end.textContent);
                                } else {
                                    results.push('');
                                }
                            }
                        }
                    }
                }
            }

            resolve(results);
        });
    }

    private getFirstSlideContent(): Promise<FirstSlideContent> {
        return new Promise<FirstSlideContent>(async (resolve) => {
            let title: string = '';
            let content: string = '';

            const slide: HTMLElement = this.el.querySelector('deckgo-deck > *:first-child');

            if (slide && slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                const titleElement: HTMLElement = slide.querySelector('[slot="title"]');
                const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

                if (titleElement) {
                    title = titleElement.textContent;
                }

                if (contentElement) {
                    content = contentElement.textContent;
                }
            }

            resolve({
                title: title,
                content: content
            });
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
            if (detail && detail.data) {
                if (detail.data.template === SlideTemplate.GIF) {
                    await this.openGifPicker();
                }

                if (detail.data.slide) {
                    await this.addSlide(detail.data.slide);
                }
            }
        });

        await popover.present();
    }

    private async addSlide(slide: any) {
        await this.concatSlide(slide);
    }

    private async openGifPicker() {
        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-gif'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data) {
                await this.addSlide(detail.data);
            }
        });

        await modal.present();
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

        const firstSlide: FirstSlideContent = await this.getFirstSlideContent();

        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-publish',
            componentProps: {
                caption: firstSlide.title,
                description: firstSlide.content
            }
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
            url: '/signin/editor',
            direction: NavDirection.FORWARD
        });
    }

    async openDeckActions($event: UIEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-deck-actions',
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                if (detail.data.action === DeckAction.FULLSCREEN) {
                    await this.toggleFullScreen();
                } else if (detail.data.action === DeckAction.JUMP_TO) {
                    await this.openSlideNavigate();
                } else if (detail.data.action === DeckAction.SETTINGS) {
                    await this.openDeckSettings();
                }
            }
        });

        await popover.present();
    }

    async openDeckSettings() {
        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-deck-settings'
        });

        modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
            // Nothing special
        });

        await modal.present();
    }

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content padding>
                <main class={this.displaying ? 'idle' : undefined}>
                    <deckgo-deck embedded={true} style={this.deckStyle}
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
                        <ion-tab-button onClick={() => this.animatePrevNextSlide(false)} color="primary">
                            <ion-icon name="arrow-back"></ion-icon>
                            <ion-label>Previous</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.animatePrevNextSlide(true)} color="primary">
                            <ion-icon name="arrow-forward"></ion-icon>
                            <ion-label>Next</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.openSlideNavigate()} color="primary" class="wider-devices">
                            <ion-icon src="/assets/icons/chapters.svg"></ion-icon>
                            <ion-label>Jump to</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.toggleFullScreen()} color="primary" class="wider-devices">
                            <ion-icon name="expand"></ion-icon>
                            <ion-label>Fullscreen</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.openDeckSettings()} color="primary" class="wider-devices">
                            <ion-icon name="settings"></ion-icon>
                            <ion-label>Settings</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={(e: UIEvent) => this.openDeckActions(e)} color="primary" class="small-devices">
                            <ion-icon md="md-more" ios="md-more"></ion-icon>
                            <ion-label>More</ion-label>
                        </ion-tab-button>
                    </ion-buttons>

                    <ion-buttons slot="end">
                        <app-add-slide-action></app-add-slide-action>
                    </ion-buttons>
                </ion-toolbar>
            </ion-footer>,
            <deckgo-inline-editor></deckgo-inline-editor>
        ];
    }
}
