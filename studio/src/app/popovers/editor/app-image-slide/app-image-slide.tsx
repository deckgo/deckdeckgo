import {Component, Element, Prop, h, EventEmitter} from '@stencil/core';

import {ImageAction} from '../../../utils/editor/image-action';

@Component({
    tag: 'app-image-slide',
    styleUrl: 'app-image-slide.scss'
})
export class AppImageSlide {

    @Element() el: HTMLElement;

    @Prop()
    selectedElement: HTMLElement;

    @Prop()
    deckOrSlide: boolean = false;

    @Prop()
    imgDidChange: EventEmitter<HTMLElement>;

    private async closePopoverWithoutResults() {
        await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss();
    }

    private onImgDidChange($event: CustomEvent<HTMLElement>) {
        if ($event && $event.detail) {
            this.imgDidChange.emit($event.detail);
        }
    }

    private async onAction($event: CustomEvent<ImageAction>) {
        if ($event && $event.detail) {
            await (this.el.closest('ion-popover') as HTMLIonPopoverElement).dismiss($event.detail);
        }
    }

    render() {
        return [<ion-toolbar>
            <h2>{this.deckOrSlide ? 'Slide background' : 'Image'}</h2>
            <ion-router-link slot="end" onClick={() => this.closePopoverWithoutResults()}>
                <ion-icon name="close"></ion-icon>
            </ion-router-link>
        </ion-toolbar>,
            <app-image selectedElement={this.selectedElement} slide={this.deckOrSlide}
                             onAction={($event: CustomEvent<ImageAction>) => this.onAction($event)}
                             onImgDidChange={($event: CustomEvent<HTMLElement>) => this.onImgDidChange($event)}></app-image>
        ];
    }

}
