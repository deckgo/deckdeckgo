import {Component, Element, Listen, Prop, State, h} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {AuthUser} from '../../../models/auth-user';
import {Slide, SlideTemplate} from '../../../models/slide';
import {Deck} from '../../../models/deck';

import {CreateSlidesUtils, SlotType} from '../../../utils/editor/create-slides.utils';
import {ParseStyleUtils} from '../../../utils/editor/parse-style.utils';
import {ParseBackgroundUtils} from '../../../utils/editor/parse-background.utils';
import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {DeckEventsHandler} from '../../../handlers/editor/events/deck/deck-events.handler';
import {RemoteEventsHandler} from '../../../handlers/editor/events/remote/remote-events.handler';
import {EditorEventsHandler} from '../../../handlers/editor/events/editor/editor-events.handler';

import {EditorHelper} from '../../../helpers/editor/editor.helper';

import {AuthService} from '../../../services/api/auth/auth.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
import {EditorAction} from '../../../popovers/editor/app-editor-actions/editor-action';
import {BusyService} from '../../../services/editor/busy/busy.service';

@Component({
    tag: 'app-editor',
    styleUrl: 'app-editor.scss'
})
export class AppEditor {

    @Element() el: HTMLElement;

    @Prop()
    deckId: string;

    @State()
    private slides: any[] = [];

    @State()
    private background: any;

    @State()
    private style: any;

    private slideIndex: number = 0;

    @State()
    private presenting: boolean = false;

    private deckEventsHandler: DeckEventsHandler = new DeckEventsHandler();
    private remoteEventsHandler: RemoteEventsHandler = new RemoteEventsHandler();
    private editorEventsHandler: EditorEventsHandler = new EditorEventsHandler();

    private authService: AuthService;
    private anonymousService: AnonymousService;
    private navService: NavService;

    private deckEditorService: DeckEditorService;

    private busySubscription: Subscription;
    private busyService: BusyService;

    @State()
    private slidesFetched: boolean = false;

    @State()
    private hideFooterActions: boolean = true;

    @State()
    private hideNavigation: boolean = false;

    constructor() {
        this.authService = AuthService.getInstance();
        this.anonymousService = AnonymousService.getInstance();
        this.navService = NavService.getInstance();
        this.deckEditorService = DeckEditorService.getInstance();
        this.busyService = BusyService.getInstance();
    }

    @Listen('ionRouteDidChange', { target: 'window' })
    async onRouteDidChange($event: CustomEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        // ionViewDidEnter and ionViewDidLeave, kind of
        if ($event.detail.to && $event.detail.to.indexOf('editor') > -1) {
            await this.init();
        } else if ($event.detail.from && $event.detail.from.indexOf('editor') > -1) {
            await this.destroy();
        }
    }

    async init() {
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
            // Hide actions footer till deck is editable
            this.hideFooterActions = false;

            await this.contentEditable(slide);
        });
    }

    async destroy() {
        this.deckEventsHandler.destroy();
        this.editorEventsHandler.destroy();
        await this.remoteEventsHandler.destroy();

        if (this.busySubscription) {
            this.busySubscription.unsubscribe();
        }
    }

    async componentDidLoad() {
        await this.updateInlineEditorListener();

        await this.remoteEventsHandler.init(this.el);
    }

    async componentDidUnload() {
        await this.remoteEventsHandler.destroy();
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
                    this.style = await ParseStyleUtils.convertStyle(deck.attributes.style);
                } else {
                    this.style = undefined;
                }

                this.background = await ParseBackgroundUtils.convertBackground(deck.background);

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

    async inactivity($event: CustomEvent) {
        this.presenting = !$event.detail;

        await this.editorEventsHandler.selectDeck();

        // Wait a bit for the display/repaint of the footer
        setTimeout(async () => {
            await this.deckEventsHandler.initSlideSize();
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
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-slide-navigate'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data >= 0) {
                await this.slideTo(detail.data);
            }
        });

        await modal.present();
    }

    private async openRemoteControl() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-remote'
        });

        await modal.present();
    }

    async onActionOpenSlideAdd($event: CustomEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

        if (!couldAddSlide) {
            await this.signIn();
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
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
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-gif'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            await this.addSlideGif(detail.data);
        });

        await modal.present();
    }

    private addSlideGif(gif: TenorGif): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!gif || !gif.media || gif.media.length <= 0 || !gif.media[0].gif || !gif.media[0].gif.url) {
                resolve();
                return;
            }

            const url: string = gif.media[0].gif.url;
            const slide: any = await CreateSlidesUtils.createSlideGif(url);

            await this.addSlide(slide);

            resolve();
        });
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

        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-publish',
            cssClass: 'fullscreen'
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

            if ($event instanceof MouseEvent && DeckDeckGoUtils.isMobile()) {
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
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
    }

    async openDeckActions($event: UIEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
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

    private stickyToolbarActivated($event: CustomEvent) {
        this.hideFooterActions = $event ? $event.detail : false;
        this.hideNavigation = $event ? DeckDeckGoUtils.isIOS() && $event.detail : false;
    }

    render() {
        return [
            <app-navigation publish={true} class={this.hideNavigation ? 'hidden' : undefined}></app-navigation>,
            <ion-content class="ion-padding">
                <app-help class={!this.slidesFetched ? 'hidden' : undefined}></app-help>
                <main class={this.slidesFetched ? (this.presenting ? 'ready idle' : 'ready') : undefined}>

                    {this.renderLoading()}

                    <deckgo-deck embedded={true} style={this.style}
                                 onMouseDown={(e: MouseEvent) => this.deckTouched(e)}
                                 onTouchStart={(e: TouchEvent) => this.deckTouched(e)}
                                 onSlideNextDidChange={() => this.hideToolbar()}
                                 onSlidePrevDidChange={() => this.hideToolbar()}
                                 onSlideToChange={() => this.hideToolbar()}
                                 onMouseInactivity={($event: CustomEvent) => this.inactivity($event)}>
                        {this.slides}
                        {this.background}
                    </deckgo-deck>
                    <deckgo-remote autoConnect={false}></deckgo-remote>
                </main>
                <app-editor-toolbar></app-editor-toolbar>
            </ion-content>,
            <ion-footer class={this.presenting ? 'idle' : undefined}>
                <ion-toolbar>
                    <ion-buttons slot="start" class={this.hideFooterActions ? 'hidden' : undefined}>
                        <app-add-slide-action
                            onActionOpenSlideAdd={($event: CustomEvent) => this.onActionOpenSlideAdd($event)}></app-add-slide-action>

                        <ion-tab-button onClick={() => this.animatePrevNextSlide(false)} color="primary" mode="md">
                            <ion-icon name="arrow-back"></ion-icon>
                            <ion-label>Previous</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.animatePrevNextSlide(true)} color="primary" mode="md">
                            <ion-icon name="arrow-forward"></ion-icon>
                            <ion-label>Next</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.openSlideNavigate()} color="primary" class="wider-devices" mode="md">
                            <ion-icon src="/assets/icons/chapters.svg"></ion-icon>
                            <ion-label>Jump to</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.toggleFullScreen()} color="primary" class="wider-devices" mode="md">
                            <ion-icon name="expand"></ion-icon>
                            <ion-label>Fullscreen</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={() => this.openRemoteControl()} color="primary" class="wider-devices" mode="md">
                            <ion-icon name="phone-portrait"></ion-icon>
                            <ion-label>Remote</ion-label>
                        </ion-tab-button>

                        <ion-tab-button onClick={(e: UIEvent) => this.openDeckActions(e)} color="primary" class="small-devices" mode="md">
                            <ion-icon name="more" md="md-more" ios="md-more"></ion-icon>
                            <ion-label>More</ion-label>
                        </ion-tab-button>
                    </ion-buttons>
                </ion-toolbar>
            </ion-footer>,
            <deckgo-inline-editor containers="h1,h2,h3,section" sticky-mobile="true"
                                  onStickyToolbarActivated={($event: CustomEvent) => this.stickyToolbarActivated($event)}
                                  img-anchor="deckgo-lazy-img" img-property-width="--deckgo-lazy-img-width"
                                  img-property-css-float="--deckgo-lazy-img-float">
            </deckgo-inline-editor>
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
