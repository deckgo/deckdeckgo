import {AlertOptions, ModalOptions} from '@ionic/core';

export class IonControllerUtils {

    static createModal(opts: ModalOptions): Promise<HTMLIonModalElement> {
        return new Promise<HTMLIonModalElement>(async (resolve) => {
            const modalController: HTMLIonModalControllerElement = document.querySelector('ion-modal-controller');
            await modalController.componentOnReady();

            const modal: HTMLIonModalElement = await modalController.create(opts);

            resolve(modal);
        });
    }

    static createAlert(opts: AlertOptions): Promise<HTMLIonAlertElement> {
        return new Promise<HTMLIonAlertElement>(async (resolve) => {
            const alertController: HTMLIonAlertControllerElement = document.querySelector('ion-alert-controller');
            await alertController.componentOnReady();

            const alert: HTMLIonAlertElement = await alertController.create(opts);

            resolve(alert);
        });
    }

}
