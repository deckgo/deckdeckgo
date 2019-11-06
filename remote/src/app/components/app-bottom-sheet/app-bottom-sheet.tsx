import {Component, h, Element, Host, State} from '@stencil/core';

import {debounce, unifyEvent} from '@deckdeckgo/utils';

@Component({
    tag: 'app-bottom-sheet',
    styleUrl: 'app-bottom-sheet.scss'
})
export class AppBottomSheet {
    @Element() el: HTMLElement;

    private startY: number;

    private bottomSheetMinHeight: number = 48;
    private heightOffset: number;

    @State()
    private contentHeight: number;

    @State()
    private toolbarOffset: number = 56;

    @State()
    private bottomSheetTop: number = this.bottomSheetMinHeight;

    async componentDidLoad() {
        await this.initSize();
        await this.init();

        this.initWindowResize();
    }

    async componentDidUnload() {
        await this.destroy();

        this.removeWindowResize();
    }

    private initWindowResize() {
        if (window) {
            window.addEventListener('resize', debounce(this.onWindowResize));
        }
    }

    private removeWindowResize() {
        if (window) {
            window.removeEventListener('resize', debounce(this.onWindowResize));
        }
    }

    private onWindowResize = async () => {
        await this.initSize();
    };

    private initSize(): Promise<void> {
        return new Promise<void>((resolve) => {
            this.bottomSheetTop = this.bottomSheetMinHeight;
            this.heightOffset = (window.innerHeight || screen.height) * 0.8;
            this.contentHeight = (window.innerHeight || screen.height);

            const header: HTMLElement = document.querySelector('ion-nav ion-header');

            if (header) {
                this.toolbarOffset = header.offsetHeight;
            }

            resolve();
        });
    }

    private init(): Promise<void> {
        return new Promise<void>((resolve) => {
            const div: HTMLElement = this.el.querySelector('app-bottom-sheet div.container');

            if (!div) {
                resolve();
                return;
            }

            div.addEventListener('mousedown', this.startEvent, {passive: false});
            div.addEventListener('touchstart', this.startEvent, {passive: false});
            document.addEventListener('mouseup', this.endEvent, {passive: false});
            document.addEventListener('touchend', this.endEvent, {passive: false});

            resolve();
        });
    }

    private destroy(): Promise<void> {
        return new Promise<void>((resolve) => {
            const div: HTMLElement = this.el.querySelector('app-bottom-sheet div.container');

            if (!div) {
                resolve();
                return;
            }

            div.removeEventListener('mousedown', this.startEvent, true);
            div.removeEventListener('touchstart', this.startEvent, true);
            document.removeEventListener('mouseup', this.endEvent, true);
            document.removeEventListener('touchend', this.endEvent, true);

            resolve();
        });
    }

    private startEvent = ($event: MouseEvent | TouchEvent) => {
        $event.preventDefault();

        this.startY = unifyEvent($event).clientY;
    };

    private endEvent = ($event: MouseEvent) => {
        if (!this.startY || this.startY === undefined) {
            return;
        }

        $event.preventDefault();

        const toY: number = unifyEvent($event).clientY;

        const div: HTMLElement = this.el.querySelector('app-bottom-sheet div.container');

        if (this.startY > toY) {
            this.bottomSheetTop = this.bottomSheetTop <= this.bottomSheetMinHeight ? this.heightOffset : (this.bottomSheetTop + this.heightOffset >= div.offsetHeight ? div.offsetHeight : (this.bottomSheetTop + this.heightOffset));
        } else {
            this.bottomSheetTop = this.bottomSheetMinHeight;
        }

        this.startY = undefined;
    };

    render() {
        return <Host style={{'--bottom-sheet-top': `${this.bottomSheetTop}px`, '--bottom-sheet-toolbaroffset': `${this.toolbarOffset}px`, '--contentheight': `${this.contentHeight}px`}}>
            <div class="indicator"></div>
            <div class="container ion-padding-top">
                <slot></slot>
            </div>
        </Host>
    }

}
