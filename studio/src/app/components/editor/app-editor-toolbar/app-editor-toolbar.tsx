import {Component, Element, Method, State, Event, EventEmitter, Prop, Listen} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';

import {Utils} from '../../../utils/core/utils';
import {SlotType} from '../../../utils/editor/create-slides.utils';
import {ToggleSlotUtils} from '../../../utils/editor/toggle-slot.utils';
import {PhotoHelper} from '../../../helpers/editor/photo.helper';

import {ImageAction} from '../../../popovers/editor/app-image/image-action';

import {BusyService} from '../../../services/editor/busy/busy.service';

@Component({
    tag: 'app-editor-toolbar',
    styleUrl: 'app-editor-toolbar.scss',
    shadow: false
})
export class AppEditorToolbar {

    @Prop({connect: 'ion-modal-controller'}) modalController: HTMLIonModalControllerElement;
    @Prop({connect: 'ion-popover-controller'}) popoverController: HTMLIonPopoverControllerElement;

    @Element() el: HTMLElement;

    @State()
    private displayed: boolean = false;

    @State()
    private color: string;

    @State()
    private background: string;

    private selectedElement: HTMLElement;

    @State()
    private deckOrSlide: boolean = false;

    @State()
    private code: boolean = false;

    private applyToAllDeck: boolean = false;

    @Event() private blockSlide: EventEmitter<boolean>;

    @Event() private slideDelete: EventEmitter<HTMLElement>;

    @Event() private slideDidChange: EventEmitter<HTMLElement>;
    @Event() private deckDidChange: EventEmitter<HTMLElement>;
    @Event() private codeDidChange: EventEmitter<HTMLElement>;

    private subscription: Subscription;
    private busyService: BusyService;

    @State()
    private deckBusy: boolean = false;

    constructor() {
        this.busyService = BusyService.getInstance();
    }

    async componentWillLoad() {
        this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
            this.deckBusy = busy;
        });
    }

    async componentDidLoad() {
        await this.colorPickerListener(true);
        await this.backgroundPickerListener(true);

        this.initWindowResize();
    }

    async componentDidUnload() {
        await this.colorPickerListener(false);
        await this.backgroundPickerListener(false);

        if (this.subscription) {
            this.subscription.unsubscribe();
        }
    }

    private initWindowResize() {
        if (window) {
            window.addEventListener('resize', Utils.debounce(async () => {
                await this.moveToolbar();
            }, 100));
        }
    }

    @Listen('document:mouseInactivity')
    async inactivity($event: CustomEvent) {
        this.displayed = $event.detail;

        await this.blurSelectedElement();
    }

    @Method()
    touch(element: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            await this.unSelect();
            await this.select(element);

            resolve(null);
        });
    }

    @Method()
    blurSelectedElement(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (this.selectedElement) {
                this.selectedElement.blur();
            }

            resolve();
        });
    }

    private select(element: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const selected: HTMLElement = await this.findSelectedElement(element);

            if (!selected) {
                this.blockSlide.emit(false);
                resolve();
                return;
            }

            await this.initSelectedElement(selected);

            selected.focus();

            await this.displayToolbar(selected);

            this.blockSlide.emit(!this.deckOrSlide);

            resolve();
        });
    }

    @Method()
    unSelect(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (this.selectedElement) {
                this.selectedElement.removeEventListener('paste', this.cleanOnPaste, true);

                this.selectedElement.blur();

                await this.hideToolbar();
            }

            resolve();
        });
    }

    private findSelectedElement(element: HTMLElement): Promise<HTMLElement> {
        return new Promise<HTMLElement>(async (resolve) => {
            if (!element || !element.parentElement) {
                resolve(element);
                return;
            }

            if (this.isElementSlideOrDeck(element)) {
                resolve(element);
                return;
            }

            if (element.getAttribute('slot') && element.getAttribute('slot') !== 'code') {
                resolve(element);
                return;
            }

            const result: HTMLElement = await this.findSelectedElement(element.parentElement);

            resolve(result);
        });
    }

    private isElementSlideOrDeck(element: HTMLElement): boolean {
        return element && element.nodeName && (element.nodeName.toLowerCase().indexOf('deckgo-deck') > -1 || element.nodeName.toLowerCase().indexOf('deckgo-slide') > -1);
    }

    private isElementCode(element: HTMLElement): boolean {
        return element && element.nodeName && element.nodeName.toLowerCase() === 'deckgo-highlight-code';
    }

    private cleanOnPaste = async ($event) => {
        return new Promise<void>(async (resolve) => {
            if (!$event || !document) {
                resolve();
                return;
            }

            const parseText: string = $event.clipboardData.getData('text/plain');

            if (!parseText || parseText.length <= 0) {
                resolve();
                return;
            }

            // In the future, Safari isn't yet ready / without preventDefault
            // // @ts-ignore
            // navigator.permissions.query({name: 'clipboard-write'}).then(async result => {
            //     if (result.state === 'granted' || result.state === 'prompt') {
            //         // @ts-ignore
            //         await navigator.clipboard.writeText(parseText);
            //     }
            // });

            $event.preventDefault();

            const parseTexts: string[] = parseText.replace(/(?:\r\n|\r|\n)/g, '<br/>').split('<br/>');

            if (!parseTexts || parseTexts.length <= 0) {
                resolve();
                return;
            }

            const isCodeElement: boolean = $event.target && $event.target.nodeName && $event.target.nodeName.toLowerCase() === 'code';

            const selected: Selection = await this.getSelection();
            if (selected && selected.rangeCount) {
                const range = selected.getRangeAt(0);
                range.deleteContents();

                parseTexts.forEach((text: string, index: number) => {
                    const newTextNode: Text = document.createTextNode(text);
                    range.collapse(false);
                    range.insertNode(newTextNode);

                    if (index < parseTexts.length - 1) {
                        range.collapse(false);

                        if (isCodeElement) {
                            const text: Text = document.createTextNode('\n');
                            range.insertNode(text);
                        } else {
                            const br: HTMLBRElement = document.createElement('br');
                            range.insertNode(br);
                        }

                    }
                });

                selected.empty();
            }

            resolve();
        });
    };

    private getSelection(): Promise<Selection> {
        return new Promise<Selection>((resolve) => {
            let selectedSelection: Selection = null;

            if (window && window.getSelection) {
                selectedSelection = window.getSelection();
            } else if (document && document.getSelection) {
                selectedSelection = document.getSelection();
            } else if (document && (document as any).selection) {
                selectedSelection = (document as any).selection.createRange().text;
            }

            resolve(selectedSelection);
        });
    }

    private displayToolbar(element: HTMLElement): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!element) {
                resolve();
                return;
            }

            await this.moveToolbar();

            this.displayed = true;

            const style: CSSStyleDeclaration = window.getComputedStyle(element);
            this.color = style.color;
            this.background = style.backgroundColor;

            const colorPicker: HTMLElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            await this.setToolbarPosition(element, colorPicker, 38);

            const backgroundPicker: HTMLElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            await this.setToolbarPosition(element, backgroundPicker, 78);

            resolve();
        });
    }

    private moveToolbar(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            const toolbar: HTMLElement = this.el.querySelector('div.editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            await this.setToolbarPosition(this.selectedElement, toolbar, 0);

            resolve();
        });
    }

    private setToolbarPosition(src: HTMLElement, applyTo: HTMLElement, offsetWidth: number): Promise<void> {
        return new Promise<void>((resolve) => {
            // Selected element
            const width: number = src.clientWidth > 200 ? src.clientWidth : 200;

            const rect: ClientRect = src.getBoundingClientRect();
            const left: number = rect && rect.left > 0 ? rect.left : 0;
            const top: number = rect && rect.top > 0 ? rect.top : 0;

            // The <main/> element in order to find the offset
            const mainPaneRect: ClientRect = applyTo.parentElement && applyTo.parentElement.parentElement ? applyTo.parentElement.parentElement.getBoundingClientRect() : null;
            const extraLeft: number = mainPaneRect && mainPaneRect.left > 0 ? mainPaneRect.left : 0;
            const extraTop: number = mainPaneRect && mainPaneRect.top > 0 ? mainPaneRect.top : 0;

            // Set top position
            applyTo.style.top = '' + (top - extraTop > 0 ? top - extraTop : 0) + 'px';

            const windowWidth: number = window.innerWidth | screen.width;

            const leftStandardPosition: number = left + offsetWidth - extraLeft;
            const leftPosition: number = leftStandardPosition + width > windowWidth ? windowWidth - width : leftStandardPosition;

            // Set left position
            applyTo.style.left = '' + leftPosition + 'px';

            // If not slide or deck selected, move a bit the toolbar
            if (!this.deckOrSlide) {
                applyTo.style.transform = 'translate(0, -2.4rem)';
            } else {
                applyTo.style.transform = 'translate(0,0)';
            }

            // Set a width in order to align right the delete button
            applyTo.style.width = '' + width + 'px';

            resolve();
        });
    }

    @Method()
    hideToolbar(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            this.displayed = false;
            await this.initSelectedElement(null);

            resolve();
        });
    }

    private deleteElement(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (this.deckBusy && this.deckOrSlide) {
                resolve();
                return;
            }

            this.busyService.deckBusy(true);

            if (this.selectedElement.nodeName && this.selectedElement.nodeName.toLowerCase().indexOf('deckgo-slide') > -1) {
                this.slideDelete.emit(this.selectedElement);
            } else {
                const parent: HTMLElement = this.selectedElement.parentElement;
                this.selectedElement.parentElement.removeChild(this.selectedElement);
                this.slideDidChange.emit(parent);
            }

            await this.hideToolbar();

            resolve();
        });
    }

    // Color

    private colorPickerListener(bind: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            const colorPicker: HTMLInputElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            if (bind) {
                colorPicker.addEventListener('change', this.selectColor, false);
            } else {
                colorPicker.removeEventListener('change', this.selectColor, true);
            }


            resolve();
        });
    }

    private openColorPicker = (): Promise<void> => {
        return new Promise<void>((resolve) => {
            const colorPicker: HTMLInputElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.click();

            resolve();
        });
    };

    private selectColor = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.color = $event.target.value;

        if (this.deckOrSlide) {
            const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

            element.style.setProperty('--color', $event.target.value);
        } else {
            this.selectedElement.style.color = $event.target.value;
        }

        await this.emitChange();
    };

    // Background

    private backgroundPickerListener(bind: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            const backgroundPicker: HTMLInputElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            if (bind) {
                backgroundPicker.addEventListener('change', this.selectBackground, false);
            } else {
                backgroundPicker.removeEventListener('change', this.selectBackground, true);
            }


            resolve();
        });
    }

    private openBackgroundPicker = (): Promise<void> => {
        return new Promise<void>((resolve) => {
            const backgroundPicker: HTMLInputElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            backgroundPicker.click();

            resolve();
        });
    };

    private selectBackground = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.background = $event.target.value;

        if (this.deckOrSlide) {
            const element: HTMLElement = this.applyToAllDeck ? this.selectedElement.parentElement : this.selectedElement;

            element.style.setProperty('--background', $event.target.value);
        } else if (this.selectedElement.parentElement && this.selectedElement.parentElement.nodeName && this.selectedElement.parentElement.nodeName.toLowerCase() === 'deckgo-slide-split') {
            const element: HTMLElement = this.selectedElement.parentElement;

            if (this.selectedElement.getAttribute('slot') === 'start') {
                element.style.setProperty('--slide-split-background-start', $event.target.value);
            } else if (this.selectedElement.getAttribute('slot') === 'end') {
                element.style.setProperty('--slide-split-background-end', $event.target.value);
            } else {
                this.selectedElement.style.background = $event.target.value;
            }
        } else {
            this.selectedElement.style.background = $event.target.value;
        }

        await this.emitChange();
    };

    private async openSlotType() {
        if (this.deckOrSlide) {
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-slot-type',
            componentProps: {
                selectedElement: this.selectedElement
            },
            mode: 'md',
            cssClass: 'popover-menu'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data && detail.data.type) {
                await this.toggleSlotType(detail.data.type);
            }
        });

        await popover.present();
    }

    private async openCode() {
        if (!this.code) {
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-code',
            componentProps: {
                selectedElement: this.selectedElement,
                codeDidChange: this.codeDidChange
            },
            mode: 'md',
            cssClass: 'popover-menu'
        });

        await popover.present();
    }

    private toggleSlotType(type: SlotType): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement || !this.selectedElement.parentElement) {
                resolve();
                return;
            }

            const element: HTMLElement = await ToggleSlotUtils.toggleSlotType(this.selectedElement, type);

            this.selectedElement.parentElement.replaceChild(element, this.selectedElement);

            await this.initSelectedElement(element);

            await this.emitChange();

            await this.hideToolbar();

            resolve();
        });
    }

    private emitChange(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!this.selectedElement || !this.selectedElement.parentElement) {
                resolve();
                return;
            }

            if (this.applyToAllDeck) {
                const deckElement: HTMLElement = this.deckOrSlide ? this.selectedElement.parentElement : this.selectedElement.parentElement.parentElement;
                this.deckDidChange.emit(deckElement);
            } else {
                // If not deck or slide, then parent is the container slide
                this.slideDidChange.emit(this.deckOrSlide ? this.selectedElement : this.selectedElement.parentElement);
            }

            resolve();
        });
    }

    private initSelectedElement(element: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            this.selectedElement = element;
            this.deckOrSlide = this.isElementSlideOrDeck(element);
            this.code = this.isElementCode(element);

            if (element) {
                element.addEventListener('paste', this.cleanOnPaste, false);
            }

            resolve();
        });
    }

    private async openForDeckOrSlide($event: UIEvent, myFunction: Function) {
        if (!this.deckOrSlide) {
            await myFunction();
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-deck-or-slide',
            event: $event,
            mode: 'ios'
        });

        popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data) {
                this.applyToAllDeck = detail.data.deck;
                await myFunction();
            }
        });

        await popover.present();
    }

    private async openBackground() {
        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-image',
            componentProps: {
                deckOrSlide: this.deckOrSlide
            },
            mode: 'md',
            cssClass: 'popover-menu'
        });

        popover.onWillDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data) {
                if (detail.data.hasOwnProperty('applyToAllDeck')) {
                    this.applyToAllDeck = detail.data.applyToAllDeck;
                }

                if (detail.data.action === ImageAction.OPEN_PHOTOS) {
                    await this.openPhotos();
                } else if (detail.data.action === ImageAction.DELETE_PHOTO) {
                    await this.deleteBackgroundPhoto();
                }
            }
        });

        await popover.present();
    }

    private async openPhotos() {
        const modal: HTMLIonModalElement = await this.modalController.create({
            component: 'app-photo'
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data && this.selectedElement) {
                const helper: PhotoHelper = new PhotoHelper(this.slideDidChange, this.deckDidChange);
                await helper.appendPhoto(this.selectedElement, (detail.data as UnsplashPhoto), this.deckOrSlide, this.applyToAllDeck);
            }
        });

        await modal.present();
    }

    private deleteBackgroundPhoto(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.deckOrSlide) {
                resolve();
                return;
            }

            const helper: PhotoHelper = new PhotoHelper(this.slideDidChange, this.deckDidChange);
            await helper.deleteBackground(this.selectedElement, this.applyToAllDeck);
        });
    }

    render() {
        return [
            <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
                {this.renderSlotType()}
                {this.renderPhotos()}
                {this.renderActions()}
                {this.renderCodeOptions()}
                {this.renderDelete()}
            </div>,
            <input type="color" name="color-picker" value={this.color}></input>,
            <input type="color" name="background-picker" value={this.background}></input>
        ];
    }

    private renderDelete() {
        return <a onClick={() => this.deleteElement()} title="Delete"
                  class={this.deckBusy && this.deckOrSlide ? "delete disabled" : "delete"}>
            <ion-icon ios="md-trash" md="md-trash"></ion-icon>
        </a>
    }

    private renderActions() {
        const styleColor = {
            'border-bottom': '2px solid ' + this.color
        };

        const styleBackground = {
            'border-bottom': '2px solid ' + this.background
        };

        return [
            <a onClick={(e: UIEvent) => this.openForDeckOrSlide(e, this.openBackgroundPicker)}
               title="Background">
                <ion-label style={styleBackground}>Bg</ion-label>
            </a>,
            <a onClick={(e: UIEvent) => this.openForDeckOrSlide(e, this.openColorPicker)} title="Color">
                <ion-label style={styleColor}>A</ion-label>
            </a>
        ]
    }

    private renderSlotType() {
        if (this.deckOrSlide) {
            return undefined;
        } else {
            return <a onClick={() => this.openSlotType()} title="Toggle element type">
                <ion-label>T</ion-label>
            </a>
        }
    }

    private renderCodeOptions() {
        if (!this.code) {
            return undefined;
        } else {
            return <a onClick={() => this.openCode()} title="Code attributes">
                <ion-icon name="code"></ion-icon>
            </a>
        }
    }

    private renderPhotos() {
        return <a onClick={() => this.openBackground()} title="Background">
            <ion-icon name="images"></ion-icon>
        </a>
    }

}
