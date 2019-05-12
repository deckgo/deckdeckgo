import {EventEmitter} from '@stencil/core';

import {BusyService} from '../../services/editor/busy/busy.service';

export class PhotoHelper {

    private busyService: BusyService;

    constructor(private slideDidChange: EventEmitter<HTMLElement>,
                private deckDidChange: EventEmitter<HTMLElement>) {
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

            resolve();
        });
    }

    private createImgElement(photo: UnsplashPhoto): HTMLElement {
        const img: HTMLElement = document.createElement('deckgo-lazy-img');
        (img as any).imgSrc = photo.urls.regular;
        (img as any).imgAlt = photo.description ? photo.description : (photo.links && photo.links.html ? photo.links.html : photo.urls.regular);

        img.setAttribute('contentEditable', 'false');

        return img;
    }

    private appendContentImg(selectedElement: HTMLElement, photo: UnsplashPhoto): Promise<void> {
        return new Promise<void>((resolve) => {
            const img: HTMLElement = this.createImgElement(photo);
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

            const img: HTMLElement = this.createImgElement(photo);
            div.appendChild(img);

            element.appendChild(div);

            if (applyToAllDeck) {
                img.addEventListener('lazyImgDidLoad', async () => {
                    await (element as any).loadBackground();
                    this.deckDidChange.emit(selectedElement.parentElement);
                }, {once: true});
            } else {
                selectedElement.setAttribute('custom-background', '');

                img.addEventListener('lazyImgDidLoad', async () => {
                    this.slideDidChange.emit(selectedElement);
                }, {once: true});
            }

            resolve();
        });
    }
}
