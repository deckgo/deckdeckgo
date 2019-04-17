import {Component, Element, Method, State, Event, EventEmitter, Prop, Listen} from '@stencil/core';
import {OverlayEventDetail} from '@ionic/core';

import {Subscription} from 'rxjs';

import {SlotType} from '../../../utils/editor/create-slides.utils';
import {Utils} from '../../../utils/core/utils';

import {DeckBusyService} from '../../../services/api/deck/deck-busy.service';

@Component({
    tag: 'app-editor-toolbar',
    styleUrl: 'app-editor-toolbar.scss',
    shadow: false
})
export class AppEditorToolbar {

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

    private applyToAllDeck: boolean = false;

    @Event() private blockSlide: EventEmitter<boolean>;

    @Event() private slideDelete: EventEmitter<HTMLElement>;

    @Event() private slideDidChange: EventEmitter<HTMLElement>;
    @Event() private deckDidChange: EventEmitter<HTMLElement>;

    private subscription: Subscription;
    private deckBusyService: DeckBusyService;

    @State()
    private deckBusy: boolean = false;

    private originalPlaceHolder: Node;

    constructor() {
        this.deckBusyService = DeckBusyService.getInstance();
    }

    async componentWillLoad() {
        this.subscription = this.deckBusyService.watch().subscribe((busy: boolean) => {
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
                if (this.selectedElement) {
                    await this.select(this.selectedElement);
                }
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

            if (selected.classList && selected.classList.contains('deckgo-untouched')) {
                if (selected.firstChild) {
                    this.originalPlaceHolder = selected.removeChild(selected.firstChild);
                }

                selected.classList.remove('deckgo-untouched');
            }

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
                if (this.originalPlaceHolder && this.selectedElement.classList && !this.selectedElement.classList.contains('deckgo-untouched') && !this.selectedElement.firstChild) {
                    this.selectedElement.appendChild(this.originalPlaceHolder);
                    this.selectedElement.classList.add('deckgo-untouched');
                }

                this.selectedElement.removeEventListener('paste', this.cleanOnPaste, true);

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

            if (element.getAttribute('slot')) {
                resolve(element);
                return;
            }

            const result: HTMLElement = await this.findSelectedElement(element.parentElement);

            resolve(result);
        });
    }

    private isElementSlideOrDeck(element: HTMLElement): boolean {
        return element && element.nodeName && (element.nodeName.toLowerCase().indexOf('deckgo-deck') > -1 || element.nodeName.toLowerCase().indexOf('deckgo-slide') > -1)
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

            const selected: Selection = await this.getSelection();
            if (selected && selected.rangeCount) {
                const range = selected.getRangeAt(0);
                range.deleteContents();

                parseTexts.forEach((text: string, index: number) => {
                    const newTextNode: Text = document.createTextNode(text);
                    range.insertNode(newTextNode);

                    if (index < parseTexts.length - 1) {
                        const br: HTMLBRElement = document.createElement('br');
                        range.insertNode(br);
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

            const toolbar: HTMLElement = this.el.querySelector('div.editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            await this.setElementPosition(element, toolbar, 0);

            this.displayed = true;

            const style: CSSStyleDeclaration = window.getComputedStyle(element);
            this.color = style.color;
            this.background = style.backgroundColor;

            const colorPicker: HTMLElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            await this.setElementPosition(element, colorPicker, 38);

            const backgroundPicker: HTMLElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            await this.setElementPosition(element, backgroundPicker, 78);

            resolve();
        });
    }

    private setElementPosition(src: HTMLElement, applyTo: HTMLElement, offsetWidth: number): Promise<void> {
        return new Promise<void>((resolve) => {
            const top: number = src.offsetTop > 0 ? src.offsetTop : 0;
            const left: number = src.offsetLeft > 0 ? src.offsetLeft : 0;

            if (window.innerWidth < 1024 || screen.width < 1024) {
                applyTo.style.top = '' + (top > 50 ? top - 42 : 0) + 'px';
            } else {
                applyTo.style.top = '' + top + 'px';
            }

            if (this.deckOrSlide) {
                applyTo.style.left = '' + (0 + offsetWidth) + 'px';
                applyTo.style.transform = 'translate(0,0)';
            } else {
                applyTo.style.left = '' + (left + offsetWidth) + 'px';
                applyTo.style.transform = left > 50 && top > 50 ? 'translate(0, -2.7rem)' : 'translate(0,0)';
            }

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

            this.deckBusyService.busy(true);

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

    private async openSlotType($event: UIEvent) {
        if (this.deckOrSlide) {
            return;
        }

        const popover: HTMLIonPopoverElement = await this.popoverController.create({
            component: 'app-slot-type',
            componentProps: {
                selectedElement: this.selectedElement
            },
            event: $event,
            mode: 'ios'
        });

        popover.onDidDismiss().then(async (detail: OverlayEventDetail) => {
            if (detail.data && detail.data.type) {
                await this.toggleSlotType(detail.data.type);
            }
        });

        await popover.present();
    }

    private toggleSlotType(type: SlotType): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement || !this.selectedElement.parentElement) {
                resolve();
                return;
            }

            const element: HTMLElement = document.createElement(type.toString());

            if (this.selectedElement.attributes && this.selectedElement.attributes.length) {
                for (let i: number = 0; i < this.selectedElement.attributes.length; i++) {
                    element.setAttribute(this.selectedElement.attributes[i].name, this.selectedElement.attributes[i].value);
                }
            }

            if (this.selectedElement.childNodes && this.selectedElement.childNodes.length > 0) {
                const elements: HTMLElement[] = Array.prototype.slice.call(this.selectedElement.childNodes);
                elements.forEach((e: HTMLElement) => {
                    element.appendChild(e);
                });
            }

            this.selectedElement.parentElement.replaceChild(element, this.selectedElement);

            await this.initSelectedElement(element);

            await this.emitChange();

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

    render() {
        return [
            <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
                {this.renderActions()}
                {this.renderSlotType()}
            </div>,
            <input type="color" name="color-picker" value={this.color}></input>,
            <input type="color" name="background-picker" value={this.background}></input>
        ];
    }

    private renderActions() {
        const styleColor = {
            'border-bottom': '2px solid ' + this.color
        };

        const styleBackground = {
            'border-bottom': '2px solid ' + this.background
        };

        return [<a onClick={() => this.deleteElement()}
                   class={this.deckBusy && this.deckOrSlide ? "disabled" : undefined}>
            <ion-icon name="trash"></ion-icon>
        </a>,
            <a onClick={(e: UIEvent) => this.openForDeckOrSlide(e, this.openColorPicker)}>
                <ion-label style={styleColor}>A</ion-label>
            </a>,
            <a onClick={(e: UIEvent) => this.openForDeckOrSlide(e, this.openBackgroundPicker)}>
                <ion-label style={styleBackground}>Bg</ion-label>
            </a>
        ]
    }

    private renderSlotType() {
        if (this.deckOrSlide) {
            return undefined;
        } else {
            return <a onClick={(e: UIEvent) => this.openSlotType(e)}>
                <ion-label>T</ion-label>
            </a>
        }
    }

}
