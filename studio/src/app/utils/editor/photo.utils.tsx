export class PhotoUtils {

    static appendPhoto(selectedElement: HTMLElement, photo: PixabayHit, deckOrSlide: boolean, applyToAllDeck: boolean): Promise<void> {
        return new Promise<void>(async (resolve) => {
            if (!selectedElement || !photo || !document) {
                resolve();
                return;
            }

            // TODO deck busy

            if (deckOrSlide) {
                await this.appendBackgroundImg(selectedElement, photo, applyToAllDeck);
            } else {
                await this.appendContentImg(selectedElement, photo);
            }

            // TODO lazy loading

            // TODO save deck or slide

            resolve();
        });
    }

    private static createImgElement(photo: PixabayHit): HTMLImageElement {
        const img: HTMLImageElement = document.createElement('img');
        img.src = photo.largeImageURL;
        img.alt = photo.tags ? photo.tags : photo.largeImageURL;

        return img;
    }

    private static appendContentImg(selectedElement: HTMLElement, photo: PixabayHit): Promise<void> {
        return new Promise<void>((resolve) => {
            const img: HTMLImageElement = this.createImgElement(photo);

            selectedElement.appendChild(img);

            resolve();
        });
    }

    private static appendBackgroundImg(selectedElement: HTMLElement, photo: PixabayHit, applyToAllDeck: boolean): Promise<void> {
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
                await (element as any).loadBackground();
            }

            resolve();
        });
    }
}
