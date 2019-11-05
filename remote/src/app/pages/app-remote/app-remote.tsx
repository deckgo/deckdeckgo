import {Component, Element, Listen, Prop, State, h, JSX} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';

import {isMobile} from '@deckdeckgo/utils';

// Types
import {
    DeckdeckgoEvent,
    DeckdeckgoEventEmitter,
    DeckdeckgoEventType,
    DeckdeckgoEventDeck,
    DeckdeckgoEventSlideTo,
    DeckdeckgoSlideAction,
    DeckdeckgoEventSlideAction,
    DeckdeckgoEventSlide,
    DeckdeckgoEventNextPrevSlide,
    DeckdeckgoEventDeckReveal
} from '@deckdeckgo/types';

// Utils
import {IonControllerUtils} from '../../services/utils/ion-controller-utils';
import {ParseSlidesUtils} from '../../utils/parse-slides.utils';

// Services
import {CommunicationService, ConnectionState} from '../../services/communication/communication.service';
import {AccelerometerService} from '../../services/accelerometer/accelerometer.service';
import {NotesService} from '../../services/notes/notes.service';
import {ParseAttributesUtils} from '../../utils/parse-attributes.utils';

@Component({
    tag: 'app-remote',
    styleUrl: 'app-remote.scss'
})
export class AppRemote {

    @Element() el: HTMLElement;

    @Prop()
    room: string;

    private subscriptionState: Subscription;
    private subscriptionEvent: Subscription;

    @State() private connectionState: ConnectionState = ConnectionState.DISCONNECTED;

    @State() private deckWidth: number;
    @State() private deckHeight: number;
    @State() private drawHeightOffset: number;
    @State() private drawWidthOffset: number;

    @State() private slides: JSX.IntrinsicElements[] = [];
    @State() private slideIndex: number = 0;

    @State() private deckAttributes: any;

    @State() private deckReveal: boolean = true;
    @State() private deckRevealOnMobile: boolean = false;

    @State() drawing: boolean = false;

    @State() action: DeckdeckgoSlideAction;

    @State() extraPlayAction: boolean = false;

    private acceleratorSubscription: Subscription;
    private acceleratorInitSubscription: Subscription;

    private communicationService: CommunicationService;
    private accelerometerService: AccelerometerService;
    private notesService: NotesService;

    constructor() {
        this.communicationService = CommunicationService.getInstance();
        this.accelerometerService = AccelerometerService.getInstance();
        this.notesService = NotesService.getInstance();
    }

    async componentDidLoad() {
        this.subscriptionState = this.communicationService.watchState().subscribe(async (state: ConnectionState) => {
            this.connectionState = state;

            if (state === ConnectionState.CONNECTED) {
                this.communicationService.emit({
                    type: DeckdeckgoEventType.SLIDES_REQUEST,
                    emitter: DeckdeckgoEventEmitter.APP
                });
            }
        });

        this.subscriptionEvent = this.communicationService.watchEvent().subscribe(async ($event: DeckdeckgoEvent) => {
            if ($event.emitter === DeckdeckgoEventEmitter.DECK) {
                if ($event.type === DeckdeckgoEventType.SLIDES_ANSWER) {
                    await this.initDeckAndSlides(($event as DeckdeckgoEventDeck));
                    await this.slidePickerTo(0);
                } else if ($event.type === DeckdeckgoEventType.DECK_UPDATE) {
                    await this.initDeckAndSlides(($event as DeckdeckgoEventDeck));
                    await this.slideToLastSlide();
                    await this.setNotes();
                } else if ($event.type === DeckdeckgoEventType.SLIDE_UPDATE) {
                    await this.updateSlide(($event as DeckdeckgoEventSlide));
                    await this.setNotes();
                } else if ($event.type === DeckdeckgoEventType.NEXT_SLIDE) {
                    await this.animateNextSlide(($event as DeckdeckgoEventNextPrevSlide).slideAnimation);
                } else if ($event.type === DeckdeckgoEventType.PREV_SLIDE) {
                    await this.animatePrevSlide(($event as DeckdeckgoEventNextPrevSlide).slideAnimation);
                } else if ($event.type === DeckdeckgoEventType.SLIDE_TO) {
                    const index: number = ($event as DeckdeckgoEventSlideTo).index;
                    const speed: number = ($event as DeckdeckgoEventSlideTo).speed;

                    await this.slideTo(index, speed);
                } else if ($event.type === DeckdeckgoEventType.DELETE_SLIDE) {
                    await this.deleteSlide();
                } else if ($event.type === DeckdeckgoEventType.SLIDE_ACTION) {
                    this.action = ($event as DeckdeckgoEventSlideAction).action;
                } else if ($event.type === DeckdeckgoEventType.DECK_REVEAL_UPDATE) {
                    this.deckReveal = ($event as DeckdeckgoEventDeckReveal).reveal;
                }
            }
        });

        this.acceleratorSubscription = this.accelerometerService.watch().subscribe(async (prev: boolean) => {
            await this.prevNextSlide(prev, false);

            setTimeout(async () => {
                await this.startAccelerometer();
            }, this.accelerometerService.delay);
        });

        this.acceleratorInitSubscription = this.accelerometerService.watchInitialized().subscribe(async (initialized: boolean) => {
            if (initialized) {
                const deck: HTMLElement = this.el.querySelector('deckgo-deck');

                if (deck) {
                    await this.startAccelerometer();
                }
            }
        });

        if (window) {
            window.addEventListener('resize', async () => {
                await this.deckSize();
            });
        }

        await this.autoConnect();
    }

    private initDeckAndSlides($event: DeckdeckgoEventDeck): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if ($event && $event.deck) {
                const slidesElements: JSX.IntrinsicElements[] = await ParseSlidesUtils.parseSlides($event.deck);
                this.slides = [...slidesElements];

                this.deckAttributes = await ParseAttributesUtils.parseAttributes($event.deck.attributes);

                this.deckRevealOnMobile = !$event.mobile && isMobile() ? $event.deck.reveal : $event.deck.revealOnMobile;
                this.deckReveal = $event.mobile && isMobile() ? $event.deck.reveal : $event.deck.revealOnMobile;

            } else {
                this.slides = undefined;
            }

            resolve();
        });
    }

    private updateSlide($event: DeckdeckgoEventSlide): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if ($event && $event.slide && this.slides && this.slides.length >= $event.index) {

                const slideElement: JSX.IntrinsicElements = await ParseSlidesUtils.parseSlide($event.slide, $event.index);
                this.slides[$event.index] = slideElement;

                this.slides = [...this.slides];
            }

            resolve();
        });
    }

    async componentDidUnload() {
        await this.disconnect();

        if (this.subscriptionState) {
            this.subscriptionState.unsubscribe();
        }

        if (this.subscriptionEvent) {
            this.subscriptionEvent.unsubscribe();
        }

        if (this.acceleratorSubscription) {
            this.acceleratorSubscription.unsubscribe();
        }

        if (this.acceleratorInitSubscription) {
            this.acceleratorInitSubscription.unsubscribe();
        }
    }

    private deckSize(): Promise<void> {
        return new Promise<void>((resolve) => {
            const container: HTMLElement = this.el.querySelector('div.deck');

            if (!container) {
                return;
            }

            this.deckWidth = container.offsetWidth;
            this.deckHeight = container.offsetHeight;

            const header: HTMLElement = this.el.querySelector('ion-header');

            if (!header) {
                return;
            }

            this.drawHeightOffset = header.offsetHeight + parseInt(window.getComputedStyle(container).marginTop);
            this.drawWidthOffset = parseInt(window.getComputedStyle(container).marginLeft);

            resolve();
        });
    }

    private async prevNextSlide(prev: boolean, slideAnimation: boolean) {
        if (prev) {
            await this.emitPrevSlide(slideAnimation);
            await this.animatePrevSlide(slideAnimation);
        } else {
            await this.emitNextSlide(slideAnimation);
            await this.animateNextSlide(slideAnimation);
        }
    }

    private async slideDidChange(prev: boolean) {
        if (prev) {
            await this.emitPrevSlide(false);
        } else {
            await this.emitNextSlide(false);
        }

        await this.afterSwipe();
    }

    private async emitNextSlide(slideAnimation: boolean): Promise<void> {
        await this.emitPrevNextSlide(DeckdeckgoEventType.NEXT_SLIDE, slideAnimation);
    }

    private async emitPrevSlide(slideAnimation: boolean) {
        await this.emitPrevNextSlide(DeckdeckgoEventType.PREV_SLIDE, slideAnimation);
    }

    private async emitPrevNextSlide(type: DeckdeckgoEventType, slideAnimation: boolean) {
        this.emitSlidePrevNext(type, slideAnimation);
    }

    private async afterSwipe() {
        await this.setActiveIndex();
        await this.setNotes();

        this.action = null;
    }

    private setActiveIndex(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const deck: HTMLElement = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            this.slideIndex = await (deck as any).getActiveIndex();

            this.setExtraPlayAction();

            resolve();
        });
    }

    private setExtraPlayAction() {
        const element: HTMLElement = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.slideIndex + 1) + ')');

        this.extraPlayAction = element && element.nodeName && (
            element.nodeName.toLowerCase() === 'deckgo-slide-youtube' || element.nodeName.toLowerCase() === 'deckgo-slide-video'
        );
    }

    private async setNotes() {
        let next: HTMLElement = null;

        if (this.slides && this.slides.length > this.slideIndex) {
            next = this.el.querySelector('.deckgo-slide-container:nth-child(' + (this.slideIndex + 1) + ')');
        }

        this.notesService.next(next);
    }

    private emitSlidePrevNext(type: DeckdeckgoEventType, slideAnimation: boolean) {
        this.communicationService.emit({
            type: type,
            emitter: DeckdeckgoEventEmitter.APP,
            slideAnimation: slideAnimation
        });
    }

    private async animateNextSlide(slideAnimation: boolean) {
        await this.animatePrevNextSlide(true, slideAnimation);

        await this.afterSwipe();
    }

    private async animatePrevSlide(slideAnimation: boolean) {
        await this.animatePrevNextSlide(false, slideAnimation);

        await this.afterSwipe();
    }

    private async animatePrevNextSlide(next: boolean, slideAnimation: boolean) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        if (next) {
            await (deck as any).slideNext(slideAnimation, false);
        } else {
            await (deck as any).slidePrev(slideAnimation, false);
        }
    }

    private async slideTo(index: number, speed?: number | undefined) {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        const deckLength: number = await (deck as any).getLength();

        if (index >= deckLength) {
            return;
        }

        await (deck as any).slideTo(index, speed);

        await this.afterSwipe();
    }

    private async slideToLastSlide() {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        deck.addEventListener('slidesDidLoad', async (_$event: CustomEvent) => {
            const deckLength: number = await (deck as any).getLength();

            if (deckLength > 0) {
                await this.slideTo(deckLength - 1);
            }

            await this.setActiveIndex();
        }, {once: true});
    }

    private async deleteSlide() {
        const deck: HTMLElement = this.el.querySelector('deckgo-deck');

        if (!deck) {
            return;
        }

        await (deck as any).deleteActiveSlide();

        if (this.slides && this.slides.length > this.slideIndex && this.slideIndex >= 0) {
            this.slides.splice(this.slideIndex, 1);
            this.slideIndex = this.slideIndex > 0 ? this.slideIndex - 1 : 0;

            this.setExtraPlayAction();
        }
    }

    private moveDraw(event: CustomEvent<number>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const draw: HTMLAppDrawElement = this.el.querySelector('app-draw');

            if (!draw) {
                resolve();
                return;
            }

            await draw.moveDraw(event.detail, '300ms');

            resolve();
        });
    }

    private scrollDraw(event: CustomEvent<number>): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const draw: HTMLAppDrawElement = this.el.querySelector('app-draw');

            if (!draw) {
                resolve();
                return;
            }

            await draw.moveDraw(event.detail, '0ms');

            resolve();
        });
    }

    private async emitAction($event: UIEvent) {
        $event.stopPropagation();

        this.action = this.action === DeckdeckgoSlideAction.PLAY ? DeckdeckgoSlideAction.PAUSE : DeckdeckgoSlideAction.PLAY;

        this.communicationService.emit({
            type: DeckdeckgoEventType.SLIDE_ACTION,
            emitter: DeckdeckgoEventEmitter.APP,
            action: this.action
        });

        await this.actionPlayPause();
    }

    private actionPlayPause() {
        return new Promise(async (resolve) => {
            const deck = this.el.querySelector('deckgo-deck');

            if (!deck) {
                resolve();
                return;
            }

            const index = await (deck as any).getActiveIndex();

            const slideElement: any = this.el.querySelector('.deckgo-slide-container:nth-child(' + (index + 1) + ')');

            if (!slideElement) {
                resolve();
                return;
            }

            if (this.action === DeckdeckgoSlideAction.PAUSE) {
                await slideElement.pause();
            } else {
                await slideElement.play();
            }

            resolve();
        });
    }

    private async openConnectModal() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-remote-connect'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                await this.connect();
            } else {
                await this.disconnect();
            }
        });

        await modal.present();
    }

    private async openSlidePicker() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-remote-slide-picker',
            componentProps: {
                slides: this.slides
            }
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data >= 0) {
                await this.slidePickerTo(detail.data);
            }
        });

        await modal.present();
    }

    async slidePickerTo(newSlideIndex: number) {
        await this.slideTo(newSlideIndex);

        this.communicationService.emit({
            type: DeckdeckgoEventType.SLIDE_TO,
            emitter: DeckdeckgoEventEmitter.APP,
            index: newSlideIndex
        });
    }

    private async connect() {
        await this.disconnect();
        await this.communicationService.connect();
        await this.communicationService.join();
    }

    private async disconnect() {
        await this.communicationService.disconnect();
        await this.accelerometerService.stop();
    }

    private async openDisconnectConfirm() {
        const alert: HTMLIonAlertElement = await IonControllerUtils.createAlert({
            header: 'Disconnect',
            message: 'The remote control must be disconnected from the presentation?',
            cssClass: 'custom-info',
            buttons: [
                {
                    text: 'No',
                    role: 'cancel',
                    handler: () => {
                        // Nothing
                    }
                }, {
                    text: 'Yes',
                    handler: async () => {
                        await this.disconnect();
                    }
                }
            ]
        });

        await alert.present();
    }

    @Listen('drawing')
    isDrawing(event: CustomEvent) {
        this.drawing = event.detail;
    }

    private async initDeck() {
        await this.deckSize();
        await this.startAccelerometer();
        await this.setNotes()
    }

    private async startAccelerometer() {
        try {
            await this.accelerometerService.start();
        } catch (err) {
            // Well then no accelerometer support
        }
    }

    private autoConnect(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.room || this.room === undefined || this.room.length <= 0) {
                resolve();
                return;
            }

            this.communicationService.room = decodeURI(this.room);

            await this.communicationService.connect();
            await this.communicationService.join();

            resolve();
        });
    }

    render() {
        return [
            <app-header>
                {this.renderHeaderButtons()}
            </app-header>,
            <ion-content>
                {this.renderContent()}

                {this.renderActions()}
            </ion-content>
        ];
    }

    private renderHeaderButtons() {
        if (this.connectionState !== ConnectionState.CONNECTED) {
            return undefined;
        }

        return <ion-buttons slot="end">
            <app-stopwatch-time></app-stopwatch-time>

            <ion-button onClick={() => this.openSlidePicker()}>
                <ion-icon src="/assets/icons/chapters.svg"></ion-icon>
            </ion-button>

            <ion-button onClick={() => this.openDisconnectConfirm()}>
                <ion-icon name="log-out"></ion-icon>
            </ion-button>
        </ion-buttons>;
    }

    private renderContent() {
        if (this.connectionState === ConnectionState.CONNECTED) {
            return [<main>
                {this.renderDeck()}
                <div class="deck-navigation-buttons">
                    <div class="deck-navigation-button-prev">
                        <ion-button color="secondary" onClick={() => this.prevNextSlide(true, true)}>
                            <ion-label>Previous</ion-label>
                        </ion-button>
                    </div>
                    <div class="deck-navigation-button-next">
                        <ion-button color="primary" onClick={() => this.prevNextSlide(false, true)}>
                            <ion-label>Next</ion-label>
                        </ion-button>
                    </div>

                    {this.renderExtraActions()}
                </div>
                <app-notes></app-notes>
            </main>
            ];
        } else if (this.connectionState !== ConnectionState.DISCONNECTED) {
            let text: string = 'Not connected';

            if (this.connectionState === ConnectionState.CONNECTING) {
                text = 'Connecting...'
            } else if (this.connectionState === ConnectionState.CONNECTED_WITH_SIGNALING_SERVER) {
                text = 'Connected with the signaling server, waiting for the presentation...'
            } else if (this.connectionState === ConnectionState.NOT_CONNECTED) {
                text = 'Can\' connect, shit happens 😉 Try to reload your presentation...'
            }

            return [
                <h1 class="ion-padding">{text}</h1>,
                <ion-spinner name="dots" color="primary"></ion-spinner>
            ];
        } else {
            return [
                <h1 class="ion-padding">The DeckDeckGo remote control</h1>,
                <a onClick={() => this.openConnectModal()} class="link-to-modal">
                    <p class="ion-padding-start ion-padding-end">Not connected yet, <strong>click here</strong> to find
                        your presentation or use the link button below <ion-icon name="link"></ion-icon></p>
                </a>
            ];
        }
    }

    private renderDeck() {
        return <div class="deck">
            <deckgo-deck embedded={true} {...this.deckAttributes}
                         keyboard={false}
                         revealOnMobile={this.deckRevealOnMobile} reveal={this.deckReveal}
                         onSlidesDidLoad={() => this.initDeck()}
                         onSlideNextDidChange={() => this.slideDidChange(false)}
                         onSlidePrevDidChange={() => this.slideDidChange(true)}
                         onSlideWillChange={(event: CustomEvent<number>) => this.moveDraw(event)}
                         onSlideDrag={(event: CustomEvent<number>) => this.scrollDraw(event)}>
                {this.slides}
            </deckgo-deck>
            <app-draw width={this.deckWidth}
                      height={this.deckHeight}
                      slides={this.slides.length}
                      heightOffset={this.drawHeightOffset}
                      widthOffset={this.drawWidthOffset}></app-draw>
        </div>;
    }

    private renderActions() {
        if (this.connectionState === ConnectionState.CONNECTED) {
            return undefined;
        } else {
            return (
                <ion-fab vertical="bottom" horizontal="end" slot="fixed">
                    <ion-fab-button onClick={() => this.openConnectModal()}>
                        <ion-icon name="link"></ion-icon>
                    </ion-fab-button>
                </ion-fab>
            );
        }
    }

    private renderExtraActions() {
        if (this.extraPlayAction) {
            const icon: string = this.action === DeckdeckgoSlideAction.PLAY ? 'pause' : 'play';

            return (
                <div class="deck-action-button">
                    <button onClick={(e: UIEvent) => this.emitAction(e)} area-label={icon}>
                        <ion-icon name={icon} class={`deck-action-button-icon-${icon}`}></ion-icon>
                    </button>
                </div>
            )
        } else {
            return undefined;
        }
    }
}
