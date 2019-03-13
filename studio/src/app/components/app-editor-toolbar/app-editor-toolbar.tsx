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

    private selectedElement: HTMLElement;

    @Listen('document:elementTouched')
    async onElementTouched($event: CustomEvent) {
        if ($event) {
            await this.displayToolbar($event.detail);
        }
    }

    @Listen('window:blur')
    async onWindowBlur(_$event) {
        if (document && !document.hasFocus()) {
            await this.hideToolbar();
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

    render() {
        return <div class={this.displayed ? "editor-toolbar displayed" : "editor-toolbar"}>
            <a onClick={() => this.activated = !this.activated} class={this.activated ? "activated open-close" : "open-close"}>
                <ion-icon ios="ios-add" md="ios-add"></ion-icon>
            </a>
            {this.renderActions()}
        </div>;
    }

    private renderActions() {
        if (this.activated) {
            return <a onClick={() => this.deleteElement()} class={this.activated ? "activated trash" : "trash"}>
                <ion-icon name="trash"></ion-icon>
            </a>
        } else {
            return undefined;
        }
    }

}
