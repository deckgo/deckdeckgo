import {EventEmitter} from '@stencil/core';

import {BusyService} from '../../services/editor/busy/busy.service';

export class PhotoHelper {

    private busyService: BusyService;

    constructor(private slideDidChange: EventEmitter<HTMLElement>, private deckDidChange: EventEmitter<HTMLElement>) {
        this.busyService = BusyService.getInstance();
    }

    appendPhoto(selectedElement: HTMLElement, photo: PixabayHit, deckOrSlide: boolean, applyToAllDeck: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!selectedElement || !photo || !document) {
                resolve();
                return;
            }

            this.busyService.deckBusy(true);

            if (deckOrSlide) {
                await this.appendBackgroundImg(selectedElement, photo, deckOrSlide, applyToAllDeck);
            } else {
                await this.appendContentImg(selectedElement, deckOrSlide, photo);
            }

            // TODO lazy loading

            resolve();
        });
    }

    private createImgElement(photo: PixabayHit): HTMLImageElement {
        const img: HTMLImageElement = document.createElement('img');
        img.src = photo.largeImageURL;
        img.alt = photo.tags ? photo.tags : photo.largeImageURL;

        return img;
    }

    private appendContentImg(selectedElement: HTMLElement, deckOrSlide: boolean, photo: PixabayHit): Promise<void> {
        return new Promise<void>((resolve) => {
            const img: HTMLImageElement = this.createImgElement(photo);

            selectedElement.appendChild(img);

            this.slideDidChange.emit(deckOrSlide ? selectedElement : selectedElement.parentElement);

            resolve();
        });
    }

    private appendBackgroundImg(selectedElement: HTMLElement, photo: PixabayHit, deckOrSlide: boolean, applyToAllDeck: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            const element: HTMLElement = applyToAllDeck ? selectedElement.parentElement : selectedElement;

            if (!element) {
                resolve();
                return;
            }

            const currentSlotElement: HTMLElement = element.querySelector(':scope > [slot=\'background\']');

            if (currentSlotElement) {
                element.removeChild(currentSlotElement);
            }

            const div: HTMLElement = document.createElement('div');
            div.setAttribute('slot', 'background');

            const img: HTMLImageElement = this.createImgElement(photo);
            div.appendChild(img);

            element.appendChild(div);

            const deckElement: HTMLElement = deckOrSlide ? selectedElement.parentElement : selectedElement.parentElement.parentElement;
            this.deckDidChange.emit(deckElement);

            if (applyToAllDeck) {
                await (element as any).loadBackground();
            }

            resolve();
        });
    }
}
