export class NotificationService {
  private static instance: NotificationService;

  private registration: ServiceWorkerRegistration;

  private permissionGranted: boolean = false;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!NotificationService.instance) {
      NotificationService.instance = new NotificationService();
    }
    return NotificationService.instance;
  }

  init() {
    if ('serviceWorker' in navigator && 'PushManager' in window) {
      navigator.serviceWorker.ready.then(async () => {
        navigator.serviceWorker.getRegistration().then(async (registration: ServiceWorkerRegistration) => {
          this.registration = registration;
        });
      });
    }
  }

  askPermission(): Promise<any> {
    return new Promise((resolve, reject) => {
      if (!Notification) {
        resolve('denied');
        return;
      }

      const permissionResult = Notification.requestPermission((result) => {
        resolve(result);
      });

      if (permissionResult) {
        permissionResult.then(resolve, reject);
      }
    }).then(
      (permissionResult: string) => {
        this.permissionGranted = permissionResult === 'granted';
      },
      (_err) => {
        this.permissionGranted = false;
      }
    );
  }

  showNotification(title: string, vibrate: VibratePattern): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (this.registration && this.permissionGranted) {
        const options = {
          vibrate: vibrate
        };

        await this.registration.showNotification(title, options);
      }

      resolve();
    });
  }
}
