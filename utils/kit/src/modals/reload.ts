export const initReload = () => {
  window.addEventListener(
    'swUpdate',
    async () => {
      const registration = await navigator.serviceWorker.getRegistration();
      if (!registration || !registration.waiting) {
        return;
      }
      const toast = document.createElement('ion-toast');
      toast.message = 'A new version is available, reload to update.';
      toast.buttons = [
        {
          text: 'Reload',
          handler: () => {
            registration?.waiting?.postMessage('skipWaiting');
            window.location.reload();
          },
        },
      ];
      toast.position = 'top';
      document.body.appendChild(toast);
      await toast.present();
    },
    {once: true}
  );
};
