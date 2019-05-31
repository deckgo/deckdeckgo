import {ModalOptions} from '@ionic/core';

export class IonControllerUtils {

    static createModal(opts: ModalOptions): Promise<HTMLIonModalElement> {
        return new Promise<HTMLIonModalElement>(async (resolve) => {
            const modalController: HTMLIonModalControllerElement = document.querySelector('ion-modal-controller');
            await modalController.componentOnReady();

            const modal: HTMLIonModalElement = await modalController.create(opts);

            resolve(modal);
        });
    }

}
