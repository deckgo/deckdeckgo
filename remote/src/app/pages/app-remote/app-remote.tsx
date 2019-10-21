import {Component, Element, Listen, Prop, State, h} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';

// Types
import {
    DeckdeckgoEvent,
    DeckdeckgoEventEmitter,
    DeckdeckgoEventType,
    DeckdeckgoEventSlides,
    DeckdeckgoEventSlideTo,
    DeckdeckgoSlideAction, DeckdeckgoSlideDefinition, DeckdeckgoEventSlideAction
} from '@deckdeckgo/types';

// Utils
import {IonControllerUtils} from '../../services/utils/ion-controller-utils';

// Services
import {CommunicationService, ConnectionState} from '../../services/communication/communication.service';
import {AccelerometerService} from '../../services/accelerometer/accelerometer.service';

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

    @State() private contentWidth: number;
    @State() private contentHeight: number;
    @State() private headerHeight: number;

    @State() private slides: DeckdeckgoSlideDefinition[] = [];
    @State() private slideIndex: number = 0;

    @State() drawing: boolean = false;

    @State() action: DeckdeckgoSlideAction;

    private acceleratorSubscription: Subscription;
    private acceleratorInitSubscription: Subscription;

    private communicationService: CommunicationService;
    private accelerometerService: AccelerometerService;

    constructor() {
        this.communicationService = CommunicationService.getInstance();
        this.accelerometerService = AccelerometerService.getInstance();
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
                    await this.initSlides(($event as DeckdeckgoEventSlides));
                    await this.slidePickerTo(0);
                } else if ($event.type === DeckdeckgoEventType.SLIDES_UPDATE) {
                    await this.initSlides(($event as DeckdeckgoEventSlides));
                    await this.slideToLastSlide();
                } else if ($event.type === DeckdeckgoEventType.NEXT_SLIDE) {
                    await this.animateNextSlide();
                } else if ($event.type === DeckdeckgoEventType.PREV_SLIDE) {
                    await this.animatePrevSlide();
                } else if ($event.type === DeckdeckgoEventType.SLIDE_TO) {
                    const index: number = ($event as DeckdeckgoEventSlideTo).index;
                    const speed: number = ($event as DeckdeckgoEventSlideTo).speed;

                    await this.slideTo(index, speed);
                } else if ($event.type === DeckdeckgoEventType.DELETE_SLIDE) {
                    await this.deleteSlide();
                } else if ($event.type === DeckdeckgoEventType.SLIDE_ACTION) {
                    this.action = ($event as DeckdeckgoEventSlideAction).action;
                }
            }
        });

        this.acceleratorSubscription = this.accelerometerService.watch().subscribe(async (prev: boolean) => {
            if (prev) {
                await this.prevSlide(false);
                await this.animatePrevSlide();
            } else {
                await this.nextSlide(false);
                await this.animateNextSlide();
            }

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
                await this.contentSize();
            });
        }

        await this.contentSize();

        await this.autoConnect();
    }

    private initSlides(event: DeckdeckgoEventSlides): Promise<void> {
        return new Promise<void>((resolve) => {
            if (event.slides) {
                this.slides = event.slides;
            } else {
                // If the slides definition is not provided, we generate a pseudo list of slides for the deck length
                const length: number = event.length;

                for (let i: number = 0; i < length; i++) {
                    this.slides.push({
                        name: 'deckgo-slide-title'
                    });
                }
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

    private contentSize(): Promise<void> {
        return new Promise<void>((resolve) => {
            const content: HTMLElement = this.el.querySelector('ion-content');

            if (!content) {
                return;
            }

            this.contentWidth = content.offsetWidth;
            this.contentHeight = content.offsetHeight;

            const header: HTMLElement = this.el.querySelector('ion-header');

            if (!header) {
                return;
            }

            this.headerHeight = header.offsetHeight;

            resolve();
        });
    }

    private async nextSlide(slideAnimation: boolean): Promise<void> {
        await this.prevNextSlide(DeckdeckgoEventType.NEXT_SLIDE, slideAnimation);
    }

    private async prevSlide(slideAnimation: boolean) {
        await this.prevNextSlide(DeckdeckgoEventType.PREV_SLIDE, slideAnimation);
    }

    private async prevNextSlide(type: DeckdeckgoEventType, slideAnimation: boolean) {
        this.emitSlidePrevNext(type, slideAnimation);

        await this.afterSwipe();
    }

    private async afterSwipe() {
        await this.setActiveIndex();

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

            resolve();
        });
    }

    private emitSlidePrevNext(type: DeckdeckgoEventType, slideAnimation: boolean) {
        this.communicationService.emit({
            type: type,
            emitter: DeckdeckgoEventEmitter.APP,
            slideAnimation: slideAnimation
        });
    }

    private async animateNextSlide() {
        await this.animatePrevNextSlide(true);

        await this.afterSwipe();
    }

    private async animatePrevSlide() {
        await this.animatePrevNextSlide(false);

        await this.afterSwipe();
    }

    private async arrowNextSlide(e: UIEvent) {
        e.stopPropagation();

        await this.nextSlide(true);
    }

    private async arrowPrevSlide(e: UIEvent) {
        e.stopPropagation();

        await this.prevSlide(true);
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

    private scrollNotes(e: UIEvent, scrollTop: number): Promise<void> {
        return new Promise<void>(async (resolve) => {
            e.stopPropagation();

            const notes: HTMLElement = this.el.querySelector('p.notes');

            if (!notes) {
                resolve();
                return;
            }

            notes.scrollTop = notes.scrollTop + scrollTop;

            resolve();
        });
    }

    private emitAction(e: UIEvent) {
        e.stopPropagation();

        this.action = this.action === DeckdeckgoSlideAction.PLAY ? DeckdeckgoSlideAction.PAUSE : DeckdeckgoSlideAction.PLAY;

        this.communicationService.emit({
            type: DeckdeckgoEventType.SLIDE_ACTION,
            emitter: DeckdeckgoEventEmitter.APP,
            action: this.action
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

    private async openSettingsModal() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-remote-settings'
        });

        modal.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
            await this.startAccelerometer();
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

    @Listen('drawing')
    isDrawing(event: CustomEvent) {
        this.drawing = event.detail;
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
            <ion-header>
                <ion-toolbar color="primary">
                    <ion-title class="ion-text-uppercase">DeckDeckGo</ion-title>

                    <ion-buttons slot="start">
                        <ion-menu-toggle>
                            <ion-button>
                                <ion-icon slot="icon-only" name="menu"></ion-icon>
                            </ion-button>
                        </ion-menu-toggle>
                    </ion-buttons>

                    <ion-buttons slot="end">
                        <ion-button onClick={() => this.openSettingsModal()}>
                            <ion-icon name="settings"></ion-icon>
                        </ion-button>         
                    </ion-buttons>
                </ion-toolbar>    
            </ion-header>,
            <ion-content>
                {this.renderContent()}

                {this.renderActions()}
            </ion-content>
        ];
    }

    private renderContent() {
        if (this.connectionState === ConnectionState.CONNECTED) {
            return ([
                <deckgo-deck embedded={true}
                             onSlidesDidLoad={() => this.startAccelerometer()}
                             onSlideNextDidChange={() => this.nextSlide(false)}
                             onSlidePrevDidChange={() => this.prevSlide(false)}
                             onSlideWillChange={(event: CustomEvent<number>) => this.moveDraw(event)}
                             onSlideDrag={(event: CustomEvent<number>) => this.scrollDraw(event)}>
                    {this.renderSlides()}
                </deckgo-deck>,
                <app-draw width={this.contentWidth}
                          height={this.contentHeight}
                          slides={this.slides.length}
                          heightOffset={this.headerHeight}></app-draw>
            ]);
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
                <dark-mode-switch></dark-mode-switch>,
                <h1 class="ion-padding">The DeckDeckGo remote control</h1>,
                <a onClick={() => this.openConnectModal()} class="link-to-modal">
                    <p class="ion-padding-start ion-padding-end">Not connected yet, <strong>click here</strong> to find
                        your presentation or use the link button below <ion-icon name="link"></ion-icon></p>
                </a>
            ];
        }
    }

    private renderSlides() {
        return (
            this.slides.map((slideDefinition: DeckdeckgoSlideDefinition, i: number) => {
                return <deckgo-slide-title>
                    <div slot="content" class="ion-padding">
                        <div class="floating-slide-title">
                            <ion-chip color="primary">
                                <ion-label>Slide {i + 1} of {this.slides.length}</ion-label>
                            </ion-chip>
                        </div>
                        <div class="floating-slide-timer">
                            <app-stopwatch-time></app-stopwatch-time>
                        </div>
                        {this.renderSlideContent(slideDefinition)}
                    </div>
                </deckgo-slide-title>
            })
        );
    }

    private renderSlideHint() {
        if (this.drawing) {
            return <ion-icon name="brush"></ion-icon>;
        } else {
            return <ion-icon name="swap"></ion-icon>;
        }
    }

    private renderSlideContent(slideDefinition: DeckdeckgoSlideDefinition) {
        return [
            this.renderNotes(slideDefinition),
            this.renderSlideHint()
        ]
    }

    private renderNotes(slideDefinition: DeckdeckgoSlideDefinition) {
        if (slideDefinition.notes && slideDefinition.notes.length > 0) {
            // Just in case, remove html tags from the notes
            return <p class="ion-padding notes">{slideDefinition.notes.replace(/<(?:[^>=]|='[^']*'|="[^"]*"|=[^'"][^\s>]*)*>/gmi, '')}</p>;
        } else {
            return undefined;
        }
    }

    private renderActions() {
        if (this.connectionState === ConnectionState.CONNECTED) {
            return (<ion-fab vertical="bottom" horizontal="end" slot="fixed">
                    <ion-fab-button>
                        <ion-icon name="apps"></ion-icon>
                    </ion-fab-button>
                    <ion-fab-list side="start">
                        <ion-fab-button color="medium" onClick={() => this.openSlidePicker()}>
                            <ion-icon src="/assets/icons/chapters.svg"></ion-icon>
                        </ion-fab-button>
                        <ion-fab-button color="medium" onClick={(e: UIEvent) => this.arrowNextSlide(e)}>
                            <ion-icon name="arrow-forward"></ion-icon>
                        </ion-fab-button>
                        {this.renderNotesActions()}
                        <ion-fab-button color="medium" onClick={(e: UIEvent) => this.arrowPrevSlide(e)}>
                            <ion-icon name="arrow-back"></ion-icon>
                        </ion-fab-button>
                        <app-accelerometer></app-accelerometer>
                    </ion-fab-list>
                    <ion-fab-list side="top">
                        {this.renderExtraActions()}
                        <ion-fab-button color="medium" onClick={() => this.disconnect()}>
                            <ion-icon name="log-out"></ion-icon>
                        </ion-fab-button>
                    </ion-fab-list>
                </ion-fab>
            );
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

    private renderNotesActions() {
        if (this.slides && this.slides.length > 0 && this.slides[this.slideIndex].notes) {
            return [
                <ion-fab-button color="medium" onClick={(e: UIEvent) => this.scrollNotes(e, 50)}>
                    <ion-icon name="arrow-down"></ion-icon>
                </ion-fab-button>,
                <ion-fab-button color="medium" onClick={(e: UIEvent) => this.scrollNotes(e, -50)}>
                    <ion-icon name="arrow-up"></ion-icon>
                </ion-fab-button>
            ]
        } else {
            return null;
        }
    }

    private renderExtraActions() {
        if (this.slides &&
            (this.slides[this.slideIndex].name === 'deckgo-slide-youtube'.toUpperCase() ||
             this.slides[this.slideIndex].name === 'deckgo-slide-big-img'.toUpperCase())) {

            const icon: string = this.action === DeckdeckgoSlideAction.PLAY ? 'pause' : 'play';

            return (
                <ion-fab-button color="medium" onClick={(e: UIEvent) => this.emitAction(e)}>
                    <ion-icon name={icon}></ion-icon>
                </ion-fab-button>
            )
        } else {
            return null;
        }
    }
}
