import {Component, Element, EventEmitter, h, Prop, State, Event} from '@stencil/core';

import {alertController} from '@ionic/core';

import {take} from 'rxjs/operators';

import {Deck} from '../../../../models/data/deck';

import {QRCodeUtils} from '../../../../utils/editor/qrcode.utils';
import {EditAction} from '../../../../utils/editor/edit-action';

import {DeckEditorService} from '../../../../services/editor/deck/deck-editor.service';

@Component({
    tag: 'app-edit-slide-qrcode'
})
export class AppEditSlideQRCode {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    qrCode: boolean;

    @Prop()
    slideDidChange: EventEmitter<HTMLElement>;

    @State()
    private customQRCode: boolean = false;

    @State()
    private customContent: string = undefined;

    @Event()
    private action: EventEmitter<EditAction>;

    private deckEditorService: DeckEditorService;

    constructor() {
        this.deckEditorService = DeckEditorService.getInstance();
    }

    async componentWillLoad() {
        this.customQRCode = this.qrCode && this.selectedElement && this.selectedElement.hasAttribute('custom-qrcode');
        this.customContent = this.qrCode && this.customQRCode && this.selectedElement ? this.selectedElement.getAttribute('content') : undefined;
    }

    private async onRadioCustomLink($event: CustomEvent) {
        if ($event && $event.detail && $event.detail.value !== this.customQRCode) {
            this.customQRCode = $event.detail.value;

            await this.setDefaultQRCodeLink();
        }
    }

    private handleInput($event: CustomEvent<KeyboardEvent>) {
        this.customContent = ($event.target as InputTargetEvent).value;
    }

    private onInputCustomUrlChange(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            const valid: boolean = await this.validateCustomUrl();

            if (!valid) {
                resolve();
                return;
            }

            this.selectedElement.setAttribute('content', this.customContent);
            this.selectedElement.setAttribute('custom-qrcode', `${true}`);

            this.slideDidChange.emit(this.selectedElement);

            resolve();
        });
    }

    private validateCustomUrl(): Promise<boolean> {
        return new Promise<boolean>((resolve) => {
            try {
                if (!this.customContent) {
                    resolve(false);
                    return;
                }

                new URL(this.customContent);

                // https://stackoverflow.com/a/14582229/5404186
                const pattern: RegExp = new RegExp('^(https?:\\/\\/)?' + // protocol
                    '((([a-z\\d]([a-z\\d-]*[a-z\\d])*)\\.?)+[a-z]{2,}|' + // domain name
                    '((\\d{1,3}\\.){3}\\d{1,3}))' + // OR ip (v4) address
                    '(\\:\\d+)?(\\/[-a-z\\d%_.~+]*)*' + // port and path
                    '(\\?[;&a-z\\d%_.~+=-]*)?' + // query string
                    '(\\#[-a-z\\d_]*)?$', 'i'); // fragment locator

                resolve(pattern.test(this.customContent));
            } catch (err) {
                resolve(false);
            }
        });
    }

    private setDefaultQRCodeLink(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (this.customQRCode) {
                resolve();
                return;
            }

            this.deckEditorService.watch().pipe(take(1)).subscribe(async (deck: Deck) => {
                this.selectedElement.setAttribute('content', QRCodeUtils.getPresentationUrl(deck));
                this.selectedElement.removeAttribute('custom-qrcode');

                this.customContent = undefined;

                this.slideDidChange.emit(this.selectedElement);

                resolve();
            });
        });
    }

    private async presentQRCodeInfo() {
        const alert: HTMLIonAlertElement = await alertController.create({
            message: 'The QR codes you add to your presentations are by default linked with the homepage.<br/><br/>As soon as you share them, their content will automatically be updated with their online urls.<br/><br/>Alternatively, you could also provide a custom url for their content.',
            buttons: ['Ok']
        });

        return await alert.present();
    }

    render() {
        return [
            <ion-radio-group onIonChange={($event) => this.onRadioCustomLink($event)}>
                <ion-item-divider class="ion-padding-top">
                    <ion-label>Target</ion-label>
                    <button slot="end" class="info" onClick={() => this.presentQRCodeInfo()}>
                        <ion-icon name="help"></ion-icon>
                    </button>
                </ion-item-divider>

                <ion-item>
                    <ion-label>Your presentation</ion-label>
                    <ion-radio slot="start" value={false} mode="md" checked={!this.customQRCode}></ion-radio>
                </ion-item>
                <ion-item>
                    <ion-label>A custom url</ion-label>
                    <ion-radio slot="start" value={true} mode="md" checked={this.customQRCode}></ion-radio>
                </ion-item>
            </ion-radio-group>,

            <ion-item class="with-padding">
                <ion-input value={this.customContent} placeholder="https://..." debounce={500}
                           disabled={!this.customQRCode}
                           onIonInput={(e: CustomEvent<KeyboardEvent>) => this.handleInput(e)}
                           onIonChange={() => this.onInputCustomUrlChange()}></ion-input>
            </ion-item>,

            <ion-item class="action-button ion-margin-top">
                <ion-button shape="round" onClick={() => this.action.emit(EditAction.OPEN_CUSTOM_LOGO)}
                            color="tertiary">
                    <ion-label>Your logo</ion-label>
                </ion-button>
            </ion-item>,

            <ion-item class="action-button ion-margin-bottom">
                <ion-button shape="round" onClick={() => this.action.emit(EditAction.DELETE_LOGO)}
                            fill="outline" class="delete">
                    <ion-label>Delete logo</ion-label>
                </ion-button>
            </ion-item>
        ];
    }
}
