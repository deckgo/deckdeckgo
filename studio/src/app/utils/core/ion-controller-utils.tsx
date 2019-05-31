import {AlertOptions, LoadingOptions, ModalOptions, PopoverOptions, ToastOptions} from '@ionic/core';

export class IonControllerUtils {

    static createModal(opts: ModalOptions): Promise<HTMLIonModalElement> {
        return new Promise<HTMLIonModalElement>(async (resolve) => {
            const modalController: HTMLIonModalControllerElement = document.querySelector('ion-modal-controller');
            await modalController.componentOnReady();

            const modal: HTMLIonModalElement = await modalController.create(opts);

            resolve(modal);
        });
    }

    static createPopover(opts: PopoverOptions): Promise<HTMLIonPopoverElement> {
        return new Promise<HTMLIonPopoverElement>(async (resolve) => {
            const popoverController: HTMLIonPopoverControllerElement = document.querySelector('ion-popover-controller');
            await popoverController.componentOnReady();

            const popover: HTMLIonPopoverElement = await popoverController.create(opts);

            resolve(popover);
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

    static createLoading(opts: LoadingOptions): Promise<HTMLIonLoadingElement> {
        return new Promise<HTMLIonLoadingElement>(async (resolve) => {
            const loadingController: HTMLIonLoadingControllerElement = document.querySelector('ion-loading-controller');
            await loadingController.componentOnReady();

            const loading: HTMLIonLoadingElement = await loadingController.create(opts);

            resolve(loading);
        });
    }

    static createToast(opts: ToastOptions): Promise<HTMLIonToastElement> {
        return new Promise<HTMLIonToastElement>(async (resolve) => {
            const toastController: HTMLIonToastControllerElement = document.querySelector('ion-toast-controller');
            await toastController.componentOnReady();

            const toast: HTMLIonToastElement = await toastController.create(opts);

            resolve(toast);
        });
    }

}
