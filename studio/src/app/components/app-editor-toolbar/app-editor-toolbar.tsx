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
    private activated: boolean = false;

    @State()
    private color: string;

    private selectedElement: HTMLElement;

    async componentDidLoad() {
        await this.colorPickerListener(true);
    }

    async componentDidUnload() {
        await this.colorPickerListener(false);
    }

    @Method()
    touch($event: MouseEvent | TouchEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!$event) {
                resolve();
                return;
            }

            await this.unSelect();
            await this.select($event);

            resolve(null);
        });
    }

    private select($event: MouseEvent | TouchEvent): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if ($event.target && $event.target instanceof HTMLElement) {
                const element: HTMLElement = $event.target as HTMLElement;

                const slot: HTMLElement = await this.findSlottedElement(element);

                if (slot.classList && slot.classList.contains('deckgo-untouched')) {
                    if (slot.firstChild) {
                        slot.removeChild(slot.firstChild);
                    }

                    slot.classList.remove('deckgo-untouched');

                    slot.focus();
                }

                await this.displayToolbar(slot);
            }

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

            if (element.getAttribute('slot')) {
                resolve(element);
                return;
            }

            const result: HTMLElement = await this.findSlottedElement(element.parentElement);

            resolve(result);
        });
    }

    private displayToolbar(element: HTMLElement): Promise<void> {
        return new Promise<void>((resolve) => {
            if (!element) {
                resolve();
                return;
            }

            const toolbar: HTMLElement = this.el.querySelector('div.editor-toolbar');

            if (!toolbar) {
                resolve();
                return;
            }

            const style: CSSStyleDeclaration = window.getComputedStyle(element);
            const marginTop: number = parseInt(style.marginTop, 0);

            toolbar.style.top = '' + (element.offsetTop + marginTop) + 'px';
            toolbar.style.left = '' + element.offsetLeft + 'px';

            this.displayed = true;
            this.selectedElement = element;

            const colorPicker = this.el.querySelector('input');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.style.top = '' + (element.offsetTop + marginTop) + 'px';
            colorPicker.style.left = '' + (element.offsetLeft + 68) + 'px';

            this.color = style.color;

            resolve();
        });
    }

    @Method()
    hideToolbar(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.displayed = false;
            this.selectedElement = null;

            this.displayed = false;
            this.activated = false;

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
                <a onClick={() => this.activated = !this.activated}
                   class={this.activated ? "activated open-close" : "open-close"}>
                    <ion-icon ios="ios-add" md="ios-add"></ion-icon>
                </a>
                {this.renderActions()}
            </div>,
            <input type="color" name="color-picker" value={this.color}></input>
        ];
    }

    private renderActions() {
        if (this.activated) {

            const style = {
                'border-bottom': '2px solid ' + this.color
            };

            return [<a onClick={() => this.deleteElement()} class={this.activated ? "activated trash" : "trash"}>
                <ion-icon name="trash"></ion-icon>
            </a>,
                <a onClick={() => this.openColorPicker()} class={this.activated ? "activated" : undefined}>
                    <ion-label style={style}>A</ion-label>
                </a>
            ]
        } else {
            return undefined;
        }
    }

}
