import {Component, Element, Method, State} from '@stencil/core';

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

    private selectedElement: HTMLElement;

    async componentDidLoad() {
        await this.colorPickerListener(true);

        this.initWindowResize();
    }

    async componentDidUnload() {
        await this.colorPickerListener(false);
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
            const slot: HTMLElement = await this.findSlottedElement(element);

            if (slot.classList && slot.classList.contains('deckgo-untouched')) {
                if (slot.firstChild) {
                    slot.removeChild(slot.firstChild);
                }

                slot.classList.remove('deckgo-untouched');

                slot.focus();
            }

            await this.displayToolbar(slot);

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

    private findSlottedElement(element: HTMLElement): Promise<HTMLElement> {
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

            const result: HTMLElement = await this.findSlottedElement(element.parentElement);

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

            await this.setElementPosition(element, toolbar);

            this.displayed = true;
            this.selectedElement = element;

            const colorPicker = this.el.querySelector('input');

            if (!colorPicker) {
                resolve();
                return;
            }

            await this.setElementPosition(element, colorPicker);

            const style: CSSStyleDeclaration = window.getComputedStyle(element);
            this.color = style.color;

            resolve();
        });
    }

    private setElementPosition(src: HTMLElement, applyTo: HTMLElement): Promise<void> {
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
                applyTo.style.left = '0px';
                applyTo.style.transform = 'translate(0,0)';
            } else {
                applyTo.style.left = '0' + left + 'px';
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

    private colorPickerListener(bind: boolean): Promise<void> {
        return new Promise<void>((resolve) => {
            const colorPicker = this.el.querySelector('input');

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
            const colorPicker = this.el.querySelector('input');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.click();

            resolve();
        });
    }

    private selectColor = async ($event) => {
        this.color = $event.target.value;
        this.selectedElement.style.color = $event.target.value;
    };

    render() {
        return [
            <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
                {this.renderActions()}
            </div>,
            <input type="color" name="color-picker" value={this.color}></input>
        ];
    }

    private renderActions() {
        const style = {
            'border-bottom': '2px solid ' + this.color
        };

        return [<a onClick={() => this.deleteElement()} class="trash">
            <ion-icon name="trash"></ion-icon>
        </a>,
            <a onClick={() => this.openColorPicker()}>
                <ion-label style={style}>A</ion-label>
            </a>
        ]
    }

}
