import {Component, Element, Method, State, Event, EventEmitter, Listen, h} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subject, Subscription} from 'rxjs';
import {debounceTime} from 'rxjs/operators';

import {DeckDeckGoUtils} from '@deckdeckgo/utils';

import {SlotType} from '../../../utils/editor/create-slides.utils';
import {ToggleSlotUtils} from '../../../utils/editor/toggle-slot.utils';
import {IonControllerUtils} from '../../../utils/core/ion-controller-utils';

import {ImageHelper} from '../../../helpers/editor/image.helper';

import {ImageAction} from '../../../popovers/editor/app-image/image-action';

import {BusyService} from '../../../services/editor/busy/busy.service';
import {get, set} from 'idb-keyval';

@Component({
    tag: 'app-editor-toolbar',
    styleUrl: 'app-editor-toolbar.scss',
    shadow: false
})
export class AppEditorToolbar {

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

    private elementResizeObserver: ResizeObserverConstructor;

    private moveToolbarSubscription: Subscription;
    private moveToolbarSubject: Subject<void> = new Subject();

    constructor() {
        this.busyService = BusyService.getInstance();
    }

    async componentWillLoad() {
        this.subscription = this.busyService.watchDeckBusy().subscribe((busy: boolean) => {
            this.deckBusy = busy;
        });

        this.moveToolbarSubscription = this.moveToolbarSubject.pipe(debounceTime(250)).subscribe(async () => {
            await this.moveToolbar();
        });
    }

    async componentDidLoad() {
        this.initWindowResize();
    }

    async componentDidUnload() {
        if (this.subscription) {
            this.subscription.unsubscribe();
        }

        if (this.moveToolbarSubscription) {
            this.moveToolbarSubscription.unsubscribe();
        }

        this.removeWindowResize();
    }

    private initWindowResize() {
        if (window) {
            window.addEventListener('resize', () => this.moveToolbarSubject.next(), {passive: true});
        }
    }

    private removeWindowResize() {
        if (window) {
            window.removeEventListener('resize', () => this.moveToolbarSubject.next(), true);
        }
    }

    @Listen('mouseInactivity', {target: 'document'})
    async inactivity($event: CustomEvent) {
        if (!DeckDeckGoUtils.isFullscreen()) {
            return;
        }

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
                await this.detachMoveToolbarOnElement();

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

                await this.emitChange();
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
            const extraTop: number = mainPaneRect ? mainPaneRect.top : 0;

            // Set top position
            const topPosition: string = `${(top - extraTop > 0 ? top - extraTop : 0)}px`;
            applyTo.style.top = this.deckOrSlide ? `calc(${topPosition} + var(--editor-toolbar-padding, 0px))` : `${topPosition}`;

            const windowWidth: number = window.innerWidth | screen.width;

            const leftStandardPosition: number = left + offsetWidth - extraLeft;

            // Set left position
            const leftPosition: string = `${leftStandardPosition + width > windowWidth ? windowWidth - width : leftStandardPosition}px`;
            applyTo.style.left = this.deckOrSlide ? `calc(${leftPosition} + var(--editor-toolbar-padding, 0px))` : `${leftPosition}`;

            // If not slide or deck selected, move a bit the toolbar
            if (!this.deckOrSlide) {
                applyTo.style.transform = 'translate(0, -2.4rem)';
            } else {
                applyTo.style.transform = 'translate(0,0)';
            }

            // Set a width in order to align right the delete button
            applyTo.style.width = this.deckOrSlide ? `calc(${width}px - var(--editor-toolbar-padding, 0px) - var(--editor-toolbar-padding, 0px))` : `${width}px`;

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

    private async openSlotType() {
        if (this.deckOrSlide) {
            return;
        }

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
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

        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
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

    @Listen('colorDidChange', {target: 'document'})
    async onColorDidChange(_element: HTMLElement) {
        await this.emitChange();
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
        return new Promise<void>(async (resolve) => {
            this.selectedElement = element;
            this.deckOrSlide = this.isElementSlideOrDeck(element);
            this.code = this.isElementCode(element);

            if (element) {
                element.addEventListener('paste', this.cleanOnPaste, false);

                await this.attachMoveToolbarOnElement();
            }

            resolve();
        });
    }

    private attachMoveToolbarOnElement(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (window && 'ResizeObserver' in window) {
                await this.detachMoveToolbarOnElement();

                this.elementResizeObserver = new ResizeObserver(async (_entries) => {
                    await this.moveToolbar();
                });
                this.elementResizeObserver.observe(this.selectedElement);
            } else {
                // Fallback, better  than nothing. It won't place the toolbar if the size on enter or delete  but at least if the content change like if list is toggled
                this.selectedElement.addEventListener('focusout', () => this.moveToolbarSubject.next(), {passive: true});
            }

            resolve();
        });
    }


    private detachMoveToolbarOnElement(): Promise<void> {
        return new Promise<void>((resolve) => {
            if (window && 'ResizeObserver' in window) {
                if (this.elementResizeObserver && this.selectedElement) {
                    this.elementResizeObserver.unobserve(this.selectedElement);
                    this.elementResizeObserver.disconnect;
                }
            } else {
                this.selectedElement.removeEventListener('focusout', () => this.moveToolbarSubject.next(), true);
            }

            resolve();
        });
    }

    private async openColor() {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-color',
            componentProps: {
                deckOrSlide: this.deckOrSlide,
                color: this.color,
                background: this.background,
                selectedElement: this.selectedElement
            },
            mode: 'md',
            cssClass: 'popover-menu'
        });

        await popover.present();
    }

    private async openBackground() {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
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
                    await this.openImagesModal('app-photo');
                } else if (detail.data.action === ImageAction.DELETE_BACKGROUND) {
                    await this.deleteBackground();
                } else if (detail.data.action === ImageAction.ADD_IMAGE && detail.data.image) {
                    await this.appendImage(detail.data.image);
                } else if (detail.data.action === ImageAction.OPEN_GIFS) {
                    await this.openImagesModal('app-gif');
                } else if (detail.data.action === ImageAction.OPEN_CUSTOM) {
                    await this.openCustomImagesModal();
                }
            }
        });

        await popover.present();
    }

    private async openImagesModal(componentTag: string) {
        const modal: HTMLIonModalElement = await IonControllerUtils.createModal({
            component: componentTag
        });

        modal.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail && detail.data && this.selectedElement) {
                await this.appendImage(detail.data);
            }
        });

        await modal.present();
    }

    private appendImage(image: UnsplashPhoto | TenorGif): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            if (!image) {
                resolve();
                return;
            }

            const helper: ImageHelper = new ImageHelper(this.slideDidChange, this.deckDidChange);
            await helper.appendImage(this.selectedElement, image, this.deckOrSlide, this.applyToAllDeck);

            resolve();
        });
    }

    private async openCustomImagesModal() {
        const infoDisplayedOnce: boolean = await get<boolean>('deckdeckgo_display_custom_images');

        if (!infoDisplayedOnce) {
            await this.openCustomImagesPublicInfo();
        } else {
            await this.openImagesModal('app-custom-images');
        }
    }

    private async openCustomImagesPublicInfo() {
        const popover: HTMLIonPopoverElement = await IonControllerUtils.createPopover({
            component: 'app-info-images',
            mode: 'ios'
        });

        popover.onDidDismiss().then(async () => {
            await set('deckdeckgo_display_custom_images', true);
        });

        await popover.present();
    }

    private deleteBackground(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.deckOrSlide) {
                resolve();
                return;
            }

            const helper: ImageHelper = new ImageHelper(this.slideDidChange, this.deckDidChange);
            await helper.deleteBackground(this.selectedElement, this.applyToAllDeck);
        });
    }

    render() {
        return [
            <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
                {this.renderSlotType()}
                {this.renderPhotos()}
                {this.renderColor()}
                {this.renderCodeOptions()}
                {this.renderDelete()}
            </div>
        ];
    }

    private renderDelete() {
        return <a onClick={() => this.deleteElement()} title="Delete"
                  class={this.deckBusy && this.deckOrSlide ? "delete disabled" : "delete"}>
            <ion-icon name="trash"></ion-icon>
        </a>
    }

    private renderColor() {
        return <a onClick={() => this.openColor()} title="Color">
            <ion-icon name="color-fill"></ion-icon>
        </a>
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
