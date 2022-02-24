import {toastController} from '@ionic/core';

export class ErrorEvents {
  init() {
    document.addEventListener('ddgError', this.onError, {passive: true});
  }

  destroy() {
    document.removeEventListener('ddgError', this.onError, true);
  }

  private onError = async ({detail: error}: CustomEvent<string>) => this.toastError(error);

  private async toastError(error: string) {
    const toast: HTMLIonToastElement = await toastController.create({
      message: error,
      buttons: [
        {
          text: 'Close',
          role: 'cancel'
        }
      ],
      position: 'top',
      color: 'danger',
      duration: 6000
    });

    await toast.present();
  }
}
