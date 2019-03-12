import {Component, Element, Listen, State} from '@stencil/core';

@Component({
    tag: 'app-editor-toolbar',
    styleUrl: 'app-editor-toolbar.scss',
    shadow: false
})
export class AppEditorToolbar {

    @Element() el: HTMLElement;

    @State()
    private activated: boolean = false;

    private selectedElement: HTMLElement;

    @Listen('document:elementTouched')
    async onElementTouched($event: CustomEvent) {
        if ($event) {
            await this.displayToolbar($event.detail);
        }
    }

    @Listen('document:elementUnTouched')
    async onElementUnTouched(_$event: CustomEvent) {
        await this.hideToolbar();
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

            toolbar.style.top = '' + (element.offsetTop - 30) + 'px';
            toolbar.style.left = '' + (element.offsetLeft + element.offsetWidth - 20) + 'px';

            this.activated = true;
            this.selectedElement = element;

            resolve();
        });
    }

    private hideToolbar(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.activated = false;
            this.selectedElement = null;

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
        return <div class={this.activated ? "editor-toolbar activated" : "editor-toolbar"}>
            <a onClick={() => this.deleteElement()} onMouseDown={(e: UIEvent) => e.stopPropagation()}>
                <ion-icon name="trash"></ion-icon>
            </a>
        </div>;
    }

}
