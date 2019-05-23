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

    deleteBackground(selectedElement: HTMLElement, applyToAllDeck: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!selectedElement || !document) {
                resolve();
                return;
            }

            const element: HTMLElement = applyToAllDeck ? selectedElement.parentElement : selectedElement;

            if (!element) {
                resolve();
                return;
            }

            const currentSlotElement: HTMLElement = element.querySelector(':scope > [slot=\'background\']');

            if (currentSlotElement) {
                this.busyService.deckBusy(true);

                if (applyToAllDeck) {
                    element.removeChild(currentSlotElement);

                    this.deckDidChange.emit(selectedElement.parentElement);

                    await (element as any).loadBackground();
                } else if (selectedElement.hasAttribute('custom-background')) {
                    element.removeChild(currentSlotElement);

                    selectedElement.removeAttribute('custom-background');

                    this.slideDidChange.emit(selectedElement);

                    // Refresh background, deck might have one defined which need to be replicated on the slide which
                    if (element.parentElement) {
                        await (element.parentElement as any).loadBackground();
                    }
                }
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

            // If no spacer is added, no cursor will be displayed if the content editable element is selected
            const zeroWidthSpacer: Text = document.createTextNode('\u200B');
            selectedElement.appendChild(zeroWidthSpacer);

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
