import {Component, Element, Listen, Prop, State, h} from '@stencil/core';

import {Subscription} from 'rxjs';
import {filter, take} from 'rxjs/operators';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {AuthUser} from '../../../models/auth/auth.user';
import {SlideTemplate} from '../../../models/data/slide';
import {Deck} from '../../../models/data/deck';

import {CreateSlidesUtils, SlotType} from '../../../utils/editor/create-slides.utils';
import {ParseStyleUtils} from '../../../utils/editor/parse-style.utils';
import {ParseBackgroundUtils} from '../../../utils/editor/parse-background.utils';
import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {DeckEventsHandler} from '../../../handlers/editor/events/deck/deck-events.handler';
import {RemoteEventsHandler} from '../../../handlers/editor/events/remote/remote-events.handler';
import {EditorEventsHandler} from '../../../handlers/editor/events/editor/editor-events.handler';

import {EditorHelper} from '../../../helpers/editor/editor.helper';

import {AuthService} from '../../../services/auth/auth.service';
import {AnonymousService} from '../../../services/editor/anonymous/anonymous.service';
import {NavDirection, NavService} from '../../../services/core/nav/nav.service';
import {DeckEditorService} from '../../../services/editor/deck/deck-editor.service';
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

    @State()
    private slidesEditable: boolean = false;

    @State()
    private fullscreen: boolean = false;

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
        if ($event.detail.to && $event.detail.to.indexOf('/editor') === 0) {
            await this.init();
        } else if ($event.detail.from && $event.detail.from.indexOf('/editor') === 0) {
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

            this.slidesEditable = true;

            await this.contentEditable(slide);

            await this.showHelp();
        });

        this.fullscreen = DeckDeckGoUtils.isFullscreen();
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

        this.initWindowResize();
    }

    async componentDidUnload() {
        await this.remoteEventsHandler.destroy();

        this.removeWindowResize();
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
            const slides: any[] = await helper.loadDeckAndRetrieveSlides(this.deckId);

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
                if (deck && deck.data && deck.data.attributes && deck.data.attributes.style) {
                    this.style = await ParseStyleUtils.convertStyle(deck.data.attributes.style);
                } else {
                    this.style = undefined;
                }

                this.background = await ParseBackgroundUtils.convertBackground(deck.data.background);

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

    private async animatePrevNextSlide($event: CustomEvent<boolean>) {
        if (!$event) {
            return;
        }

        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if ($event.detail) {
            await (deck as any).slideNext(false, false);
        } else {
            await (deck as any).slidePrev(false, false);
        }
    }

    private async slideTo($event: CustomEvent<number>) {
        if (!$event) {
            return;
        }

        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        await (deck as any).slideTo($event.detail);
    }

    private async addSlide($event: CustomEvent<any>) {
        if (!$event) {
            return;
        }

        await this.concatSlide($event.detail);
    }

    @Listen('actionPublish')
    async onActionPublish() {
        // No slides, no publish
        if (!this.slides || this.slides.length <= 0) {
            return;
        }

        const couldPublish: boolean = await this.anonymousService.couldPublish(this.slides);

        if (!couldPublish) {
            await this.signIn();
            return;
        }

        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-publish',
            cssClass: 'fullscreen'
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

            // Click on the pager
            if (!($event.target as HTMLElement).nodeName || ($event.target as HTMLElement).nodeName.toLowerCase() === 'deckgo-deck') {
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

    private initWindowResize() {
        if (window) {
            window.addEventListener('resize', DeckDeckGoUtils.debounce(async () => {
                this.fullscreen = DeckDeckGoUtils.isFullscreen();
            }, 300));
        }
    }

    private removeWindowResize() {
        if (window) {
            window.removeEventListener('resize', DeckDeckGoUtils.debounce(async () => {
                this.fullscreen = DeckDeckGoUtils.isFullscreen();
            }, 300));
        }
    }

    private async signIn() {
        this.navService.navigate({
            url: '/signin' + (window && window.location ? window.location.pathname : ''),
            direction: NavDirection.FORWARD
        });
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

    private showHelp(): Promise<void> {
        return new Promise<void>(async (resolve) => {
           const actions: HTMLElement = this.el.querySelector('app-editor-actions');

           if (actions) {
               await (actions as any).displayHelp();
           }

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
                <main class={this.slidesEditable ? (this.presenting ? 'ready idle' : 'ready') : undefined}>

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
                <app-editor-toolbar onSignIn={() => this.signIn()}></app-editor-toolbar>
            </ion-content>,
            <ion-footer class={this.presenting ? 'idle' : undefined}>
                <app-editor-actions hideFooterActions={this.hideFooterActions} fullscreen={this.fullscreen} slides={this.slides}
                                    onSignIn={() => this.signIn()}
                                    onAddSlide={($event: CustomEvent<any>) => this.addSlide($event)}
                                    onAnimatePrevNextSlide={($event: CustomEvent<boolean>) => this.animatePrevNextSlide($event)}
                                    onSlideTo={($event: CustomEvent<number>) => this.slideTo($event)}
                                    onToggleFullScreen={() => this.toggleFullScreen()}></app-editor-actions>
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
