import {Component, Element, Event, EventEmitter, h, Listen, Prop} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {get, set} from 'idb-keyval';

import {SlideTemplate} from '../../../../models/data/slide';

import {MoreAction} from '../../../../utils/editor/more-action';

import {IonControllerUtils} from '../../../../utils/core/ion-controller-utils';
import {AnonymousService} from '../../../../services/editor/anonymous/anonymous.service';
import {CreateSlidesUtils} from '../../../../utils/editor/create-slides.utils';

@Component({
    tag: 'app-editor-actions',
    styleUrl: 'app-editor-actions.scss',
    shadow: false
})
export class AppEditorActions {

    @Element() el: HTMLElement;

    @Prop()
    hideFooterActions: boolean = true;

    @Prop()
    fullscreen: boolean = false;

    @Prop()
    slides: any[] = [];

    private anonymousService: AnonymousService;

    @Event() signIn: EventEmitter<void>;

    @Event() addSlide: EventEmitter<any>;

    @Event() animatePrevNextSlide: EventEmitter<boolean>;

    @Event() slideTo: EventEmitter<number>;

    @Event() toggleFullScreen: EventEmitter<void>;

    @Event() private actionPublish: EventEmitter<void>;

    @Event() private openShare: EventEmitter<void>;

    constructor() {
        this.anonymousService = AnonymousService.getInstance();
    }

    async onActionOpenSlideAdd($event: CustomEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const couldAddSlide: boolean = await this.anonymousService.couldAddSlide(this.slides);

        if (!couldAddSlide) {
            await this.signIn.emit();
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-create-slide',
            event: $event.detail,
            mode: 'md',
            cssClass: 'popover-menu'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                if (detail.data.template === SlideTemplate.GIF) {
                    await this.openGifPicker();
                } else if (detail.data.template === SlideTemplate.YOUTUBE) {
                    await this.openYoutube();
                }

                if (detail.data.slide) {
                    await this.addSlide.emit(detail.data.slide);
                }
            }
        });

        await popover.present();
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

    private async openYoutube() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-youtube'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            await this.addSlideYoutube(detail.data);
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

            await this.addSlide.emit(slide);

            resolve();
        });
    }

    private addSlideYoutube(youtubeUrl: string): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!youtubeUrl || youtubeUrl === undefined || youtubeUrl === '') {
                resolve();
                return;
            }

            const slide: any = await CreateSlidesUtils.createSlideYoutube(youtubeUrl);

            await this.addSlide.emit(slide);

            resolve();
        });
    }

    @Listen('pagerClick', {target: 'document'})
    async onPagerClick() {
        await this.openSlideNavigate();
    }

    private async openSlideNavigate() {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: 'app-slide-navigate'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data >= 0) {
                await this.slideTo.emit(detail.data);
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

    async openDeckActions($event: UIEvent) {
        if (!$event || !$event.detail) {
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-more-actions',
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data) {
                if (detail.data.action === MoreAction.FULLSCREEN) {
                    await this.toggleFullScreenMode();
                } else if (detail.data.action === MoreAction.JUMP_TO) {
                    await this.openSlideNavigate();
                } else if (detail.data.action === MoreAction.REMOTE) {
                    await this.openRemoteControl();
                } else if (detail.data.action === MoreAction.SHARE) {
                    await this.openShare.emit();
                } else if (detail.data.action === MoreAction.PUBLISH) {
                    this.actionPublish.emit();
                }
            }
        });

        await popover.present();
    }

    private toggleFullScreenMode(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await this.toggleFullScreen.emit();

            await this.openFullscreenInfo();

            resolve();
        });
    }

    private async openFullscreenInfo() {
        const infoDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_fullscreen_info');

        if (infoDisplayedOnce) {
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-fullscreen-info',
            mode: 'ios',
            cssClass: 'info'
        });

        popover.onDidDismiss().then(async (_detail: OverlayEventDetail) => {
            await set('deckdeckgo_display_fullscreen_info', true);
        });

        await popover.present();
    }

    render() {
        return <ion-toolbar>
            <ion-buttons slot="start" class={this.hideFooterActions ? 'hidden' : undefined}>
                <app-add-slide-action
                    onActionOpenSlideAdd={($event: CustomEvent) => this.onActionOpenSlideAdd($event)}>
                </app-add-slide-action>

                <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(false)} color="primary" mode="md">
                    <ion-icon name="arrow-back"></ion-icon>
                    <ion-label>Previous</ion-label>
                </ion-tab-button>

                <ion-tab-button onClick={() => this.animatePrevNextSlide.emit(true)} color="primary" mode="md">
                    <ion-icon name="arrow-forward"></ion-icon>
                    <ion-label>Next</ion-label>
                </ion-tab-button>

                <ion-tab-button onClick={() => this.openSlideNavigate()} color="primary" class="wider-devices"
                                mode="md">
                    <ion-icon src="/assets/icons/ionicons/list.svg"></ion-icon>
                    <ion-label>Slides</ion-label>
                </ion-tab-button>

                <ion-tab-button onClick={() => this.toggleFullScreenMode()} color="primary" class="wider-devices" mode="md">
                    {this.renderFullscreen()}
                </ion-tab-button>

                <ion-tab-button onClick={() => this.openRemoteControl()} color="primary" class="wider-devices"
                                mode="md">
                    <ion-icon name="phone-portrait"></ion-icon>
                    <ion-label>Remote</ion-label>
                </ion-tab-button>

                <app-share-action class="wider-devices"></app-share-action>

                <ion-tab-button onClick={(e: UIEvent) => this.openDeckActions(e)} color="primary" class="small-devices"
                                mode="md">
                    <ion-icon name="more" md="md-more" ios="md-more"></ion-icon>
                    <ion-label>More</ion-label>
                </ion-tab-button>
            </ion-buttons>

            <ion-buttons slot="end" class={this.hideFooterActions ? 'hidden' : undefined}>
                <app-help-action></app-help-action>
            </ion-buttons>
        </ion-toolbar>
    }

    private renderFullscreen() {
        if (this.fullscreen) {
            return [
                <ion-icon name="contract"></ion-icon>,
                <ion-label>Exit fullscreen</ion-label>
            ];
        } else {
            return [
                <ion-icon name="expand"></ion-icon>,
                <ion-label>Fullscreen</ion-label>
            ];
        }
    }
}
