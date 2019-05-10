import {EventEmitter} from '@stencil/core';

import {BusyService} from '../../services/editor/busy/busy.service';

export class PhotoHelper {

    private busyService: BusyService;

    constructor(private slideDidChange: EventEmitter<HTMLElement>, private deckDidChange: EventEmitter<HTMLElement>) {
        this.busyService = BusyService.getInstance();
    }

    appendPhoto(selectedElement: HTMLElement, photo: UnsplashPhoto, deckOrSlide: boolean, applyToAllDeck: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!selectedElement || !photo || !document) {
                resolve();
                return;
            }

            this.busyService.deckBusy(true);

            if (deckOrSlide) {
                await this.appendBackgroundImg(selectedElement, photo, applyToAllDeck);
            } else {
                await this.appendContentImg(selectedElement, photo);
            }

            // TODO: lazy loading

            // TODO: on size change slide should be saved

            resolve();
        });
    }

    private createImgElement(photo: UnsplashPhoto): HTMLImageElement {
        const img: HTMLImageElement = document.createElement('img');
        img.src = photo.urls.regular;
        img.alt = photo.description ? photo.description : (photo.links && photo.links.html ? photo.links.html : photo.urls.regular);

        return img;
    }

    private appendContentImg(selectedElement: HTMLElement, photo: UnsplashPhoto): Promise<void> {
        return new Promise<void>((resolve) => {
            const img: HTMLImageElement = this.createImgElement(photo);
            selectedElement.appendChild(img);

            this.slideDidChange.emit(selectedElement.parentElement);

            resolve();
        });
    }

    private appendBackgroundImg(selectedElement: HTMLElement, photo: UnsplashPhoto, applyToAllDeck: boolean): Promise<void> {
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

            if (applyToAllDeck) {
                this.deckDidChange.emit(selectedElement.parentElement);

                await (element as any).loadBackground();
            } else {
                selectedElement.setAttribute('custom-background', '');

                this.slideDidChange.emit(selectedElement);
            }

            resolve();
        });
    }
}
