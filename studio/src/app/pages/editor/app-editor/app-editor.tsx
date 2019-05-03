import {Component, Element, Listen, Prop, State} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {AuthUser} from '../../../models/auth-user';
import {Slide, SlideTemplate} from '../../../models/slide';
import {Deck} from '../../../models/deck';

import {CreateSlidesUtils, SlotType} from '../../../utils/editor/create-slides.utils';
import {ParseStyleUtils} from '../../../utils/editor/parse-style.utils';

import {DeckEventsHandler} from '../../../handlers/editor/events/deck/deck-events.handler';
import {RemoteEventsHandler} from '../../../handlers/editor/events/remote/remote-events.handler';

import {EditorHelper} from '../../../helpers/editor/editor.helper';

import {AuthService} from '../../../services/api/auth/auth.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {EditorAction} from '../../../popovers/editor/app-editor-actions/editor-action';
import {BusyService} from '../../../services/editor/busy/busy.service';
import {EditorEventsHandler} from '../../../handlers/editor/events/editor/editor-events.handler';

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
    private presenting: boolean = false;

    private deckEventsHandler: DeckEventsHandler = new DeckEventsHandler();
    private removeEventsHandler: RemoteEventsHandler = new RemoteEventsHandler();
    private editorEventsHandler: EditorEventsHandler = new EditorEventsHandler();

    private authService: AuthService;
    private anonymousService: AnonymousService;
    private navService: NavService;

    private deckEditorService: DeckEditorService;
    private deckStyle: any;

    private busySubscription: Subscription;
    private busyService: BusyService;

    @State()
    private slidesFetched: boolean = false;

    constructor() {
        this.authService = AuthService.getInstance();
        this.anonymousService = AnonymousService.getInstance();
        this.navService = NavService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
        this.busyService = BusyService.getInstance();
    }

    async componentWillLoad() {
        await this.deckEventsHandler.init(this.el);
        await this.editorEventsHandler.init(this.el);

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

            this.slidesFetched = true;
        });

        this.busySubscription = this.busyService.watchSlideEditable().subscribe(async (slide: HTMLElement) => {
            await this.contentEditable(slide);
        });
    }

    async componentDidLoad() {
        await this.initSlideSize();

        await this.updateInlineEditorListener();

        await this.removeEventsHandler.init(this.el);
    }

    async componentDidUnload() {
        this.deckEventsHandler.destroy();
        this.editorEventsHandler.destroy();
        await this.removeEventsHandler.destroy();

        if (this.busySubscription) {
            this.busySubscription.unsubscribe();
        }
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
        this.presenting = !$event.detail;

        // Wait a bit for the display/repaint of the footer
        setTimeout(async () => {
            await this.initSlideSize();
        }, 100);
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

    private async openRemoteControl() {
        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-remote'
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

    private getFirstSlideContent(): Promise<string> {
        return new Promise<string>(async (resolve) => {
            let content: string = '';

            const slide: HTMLElement = this.el.querySelector('deckgo-deck > *:first-child');

            if (slide && slide.tagName && slide.tagName.toLowerCase().indexOf('deckgo-slide') > -1) {
                const contentElement: HTMLElement = slide.querySelector('[slot="content"]');

                if (contentElement) {
                    content = contentElement.textContent;
                }
            }

            resolve(content);
        });
    }

    @Listen('actionOpenSlideAdd')
    async onActionOpenSlideAdd($event: CustomEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

        if (!couldAddSlide) {
            await this.signIn();
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-slide-type',
            event: $event.detail,
            mode: 'md',
            cssClass: 'popover-menu'
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

        const couldAddSlide: boolean = await this.anonymousService.couldPublish(this.slides);

        if (!couldAddSlide) {
            await this.signIn();
            return;
        }

        const content: string = await this.getFirstSlideContent();

        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-publish',
            componentProps: {
                description: content
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

            if ($event instanceof TouchEvent) {
                $event.preventDefault();
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
            component: 'app-editor-actions',
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                if (detail.data.action === EditorAction.FULLSCREEN) {
                    await this.toggleFullScreen();
                } else if (detail.data.action === EditorAction.JUMP_TO) {
                    await this.openSlideNavigate();
                } else if (detail.data.action === EditorAction.REMOTE) {
                    await this.openRemoteControl();
                }
            }
        });

        await popover.present();
    }

    private contentEditable(slide: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!slide || slide.childElementCount <= 0) {
                resolve();
                return;
            }

            const elements: HTMLElement[] = Array.prototype.slice.call(slide.childNodes);
            elements.forEach((e: HTMLElement) => {
                e.setAttribute(e.nodeName && e.nodeName.toLowerCase() === SlotType.CODE ? 'editable' : 'contentEditable', '');
            });

            resolve();
        });
    }

    render() {
        return [
            <app-navigation publish={true}></app-navigation>,
            <ion-content class="ion-padding">
                <main class={this.slidesFetched ? (this.presenting ? 'ready idle' : 'ready') : undefined}>

                    {this.renderLoading()}

                    <deckgo-deck embedded={true} style={this.deckStyle}
                                 onMouseDown={(e: MouseEvent) => this.deckTouched(e)}
                                 onTouchStart={(e: TouchEvent) => this.deckTouched(e)}
                                 onSlideNextDidChange={() => this.hideToolbar()}
                                 onSlidePrevDidChange={() => this.hideToolbar()}
                                 onSlideToChange={() => this.hideToolbar()}>
                        {this.slides}
                    </deckgo-deck>
                    <app-editor-toolbar></app-editor-toolbar>
                    <deckgo-remote autoConnect={false}></deckgo-remote>
                </main>
            </ion-content>,
            <ion-footer class={this.presenting ? 'idle' : undefined}>
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

                        <ion-tab-button onClick={() => this.openRemoteControl()} color="primary" class="wider-devices">
                            <ion-icon name="phone-portrait"></ion-icon>
                            <ion-label>Remote</ion-label>
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
            <deckgo-inline-editor containers="h1,h2,h3,section"></deckgo-inline-editor>
        ];
    }

    private renderLoading() {
        if (this.slidesFetched) {
            return undefined;
        } else {
            return <ion-spinner color="primary"></ion-spinner>;
        }
    }
}
