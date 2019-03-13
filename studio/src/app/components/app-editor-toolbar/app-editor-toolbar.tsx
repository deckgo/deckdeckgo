import {Component, Element, Listen, Method, State} from '@stencil/core';

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

    @Listen('document:elementTouched')
    async onElementTouched($event: CustomEvent) {
        if ($event) {
            await this.displayToolbar($event.detail);
        }
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

            const center: number = (element.offsetHeight / 2) + marginTop;

            toolbar.style.top = '' + (element.offsetTop + center) + 'px';
            toolbar.style.left = '' + element.offsetLeft + 'px';

            this.displayed = true;
            this.selectedElement = element;

            const colorPicker = this.el.querySelector('input');

            if (!colorPicker) {
                resolve();
                return;
            }

            colorPicker.style.top = '' + (element.offsetTop + center) + 'px';
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
