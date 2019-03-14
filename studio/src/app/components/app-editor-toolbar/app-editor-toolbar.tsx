import {Component, Element, Method, State, Event, EventEmitter} from '@stencil/core';

import {DeckdeckgoStudioCreateSlide} from '../../utils/deckdeckgo-studio-create-slide';

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

    @Event() private blockSlide: EventEmitter<boolean>;

    async componentDidLoad() {
        await this.colorPickerListener(true);
        await this.backgroundPickerListener(true);

        this.initWindowResize();
    }

    async componentDidUnload() {
        await this.colorPickerListener(false);
        await this.backgroundPickerListener(false);
    }

    private initWindowResize() {
        if (window) {
            window.addEventListener('resize', this.debounce(async () => {
                if (this.selectedElement) {
                    await this.select(this.selectedElement);
                }
            }, 100));
        }
    }

    private debounce(func: Function, timeout?: number) {
        let timer: number;
        return (event) => {
            if (timer) {
                clearTimeout(timer);
            }

            timer = setTimeout(func, timeout > 0 ? timeout : 300, event);
        };
    }

    @Method()
    touch($event: MouseEvent | TouchEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event) {
                resolve();
                return;
            }

            if (!$event.target || !($event.target instanceof HTMLElement)) {
                resolve();
                return;
            }

            const element: HTMLElement = $event.target as HTMLElement;

            await this.unSelect();
            await this.select(element);

            resolve(null);
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

            if (selected.classList && selected.classList.contains('deckgo-untouched')) {
                if (selected.firstChild) {
                    selected.removeChild(selected.firstChild);
                }

                selected.classList.remove('deckgo-untouched');
            }

            selected.focus();

            await this.displayToolbar(selected);

            this.blockSlide.emit(!this.isElementSlideOrDeck(selected));

            resolve();
        });
    }

    private unSelect(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (this.selectedElement) {
                if (this.selectedElement.classList && !this.selectedElement.classList.contains('deckgo-untouched') && !this.selectedElement.firstChild) {
                    this.selectedElement.appendChild(document.createTextNode(this.selectedElement.nodeName && this.selectedElement.nodeName.toLowerCase() === 'h1' ? DeckdeckgoStudioCreateSlide.DEFAULT_TITLE : DeckdeckgoStudioCreateSlide.DEFAULT_CONTENT));
                    this.selectedElement.classList.add('deckgo-untouched');
                }

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
            this.selectedElement = element;

            const colorPicker = this.el.querySelector('input');

            if (!colorPicker) {
                resolve();
                return;
            }

            await this.setElementPosition(element, colorPicker, 38);

            const style: CSSStyleDeclaration = window.getComputedStyle(element);
            this.color = style.color;
            this.background = style.backgroundColor;

            resolve();
        });
    }

    private setElementPosition(src: HTMLElement, applyTo: HTMLElement, offsetWidth: number): Promise<void> {
        return new Promise<void>((resolve) => {
            const deckOrSlide: boolean = this.isElementSlideOrDeck(src);

            const top: number = src.offsetTop > 0 ? src.offsetTop : 0;
            const left: number = src.offsetLeft > 0 ? src.offsetLeft : 0;

            if (window.innerWidth < 1024 || screen.width < 1024) {
                applyTo.style.top = '' + (top > 50 ? top - 42 : 0) + 'px';
            } else {
                applyTo.style.top = '' + top + 'px';
            }

            if (deckOrSlide) {
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
        return new Promise<void>((resolve) => {
            this.displayed = false;
            this.selectedElement = null;

            this.displayed = false;

            resolve();
        });
    }

    private deleteElement(): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!this.selectedElement) {
                resolve();
                return;
            }

            this.selectedElement.parentElement.removeChild(this.selectedElement);

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

    private openColorPicker(): Promise<void> {
        return new Promise<void>((resolve) => {
            const colorPicker: HTMLInputElement = this.el.querySelector('input[name=\'color-picker\']');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.click();

            resolve();
        });
    }

    private selectColor = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.color = $event.target.value;

        if (this.isElementSlideOrDeck(this.selectedElement)) {
            this.selectedElement.style.setProperty('--color', $event.target.value);
        } else {
            this.selectedElement.style.color = $event.target.value;
        }
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

    private openBackgroundPicker(): Promise<void> {
        return new Promise<void>((resolve) => {
            const backgroundPicker: HTMLInputElement = this.el.querySelector('input[name=\'background-picker\']');

            if (!backgroundPicker) {
                resolve();
                return;
            }

            backgroundPicker.click();

            resolve();
        });
    }

    private selectBackground = async ($event) => {
        if (!this.selectedElement) {
            return;
        }

        this.background = $event.target.value;

        if (this.isElementSlideOrDeck(this.selectedElement)) {
            this.selectedElement.style.setProperty('--background', $event.target.value);
        } else {
            this.selectedElement.style.background = $event.target.value;
        }
    };

    render() {
        return [
            <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
                {this.renderActions()}
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

        return [<a onClick={() => this.deleteElement()}>
            <ion-icon name="trash"></ion-icon>
        </a>,
            <a onClick={() => this.openColorPicker()}>
                <ion-label style={styleColor}>A</ion-label>
            </a>,
            <a onClick={() => this.openBackgroundPicker()}>
                <ion-label style={styleBackground}>Bg</ion-label>
            </a>
        ]
    }

}
