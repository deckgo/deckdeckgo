import {BehaviorSubject, Observable, Subject} from 'rxjs';

import {get, set} from 'idb-keyval';

interface AccelerometerValues {
  enable: boolean;
  frequency: number;
  sensibility: number;
  takeUntil: number;
  delay: number;
}

export class AccelerometerService {
  private static instance: AccelerometerService;

  private enableSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);
  private enable: boolean = false;

  private permissionGranted: boolean = false;

  private sensor: LinearAccelerationSensor;

  frequency: number = 120; // measures pro seconds

  sensibility: number = 5;

  takeUntil: number = 10;
  private takeValues: number = 0;

  delay: number = 500;

  private sumAccelerationNext: number = 0;
  private sumAccelerationPrev: number = 0;

  private triggerSubject: Subject<boolean> = new Subject();

  private initializedSubject: BehaviorSubject<boolean> = new BehaviorSubject(false);

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!AccelerometerService.instance) {
      AccelerometerService.instance = new AccelerometerService();
    }
    return AccelerometerService.instance;
  }

  start(): Promise<void> {
    return new Promise<void>((resolve, reject) => {
      try {
        if (!this.permissionGranted || !this.enable) {
          resolve();
          return;
        }

        if (this.sensor && this.sensor.activated) {
          resolve();
          return;
        }

        this.initSensor();

        this.reset();

        this.sensor.start();

        resolve();
      } catch (error) {
        reject(error);
      }
    });
  }

  private reset() {
    this.takeValues = 0;

    this.sumAccelerationNext = 0;
    this.sumAccelerationPrev = 0;
  }

  private initSensor() {
    if (!this.sensor) {
      this.sensor = new LinearAccelerationSensor({frequency: this.frequency});

      this.sensor.addEventListener('error', (_event: SensorErrorEvent) => {
        try {
          this.stop();
          this.toggleEnabled(false);
        } catch (err) {
          // We tried our best
        }
      });

      this.sensor.addEventListener('reading', () => {
        // We want to avoid to do something when the phone is just moving a bit
        if (this.sensor.x > this.sensibility || this.sensor.x < this.sensibility * -1) {
          // We are emitting the direction after a bit of time
          if (this.takeValues >= this.takeUntil) {
            this.triggerSubject.next(this.sumAccelerationNext > this.sumAccelerationPrev);

            // We are stopping to listen and will start again once the slide transition is done
            this.stop();
          } else {
            // We are not interested in the strengthness of the acceleration but rather trying to guess which movement was the biggest
            if (this.sensor.x > 0) {
              this.sumAccelerationNext++;
            } else {
              this.sumAccelerationPrev++;
            }
          }

          this.takeValues++;
        }
      });
    }
  }

  stop() {
    if (this.sensor) {
      this.sensor.stop();
    }
  }

  toggle(): Promise<boolean> {
    return new Promise<boolean>(async (resolve, reject) => {
      try {
        if (this.enable) {
          this.stop();
        } else {
          await this.askPermission();
        }

        this.toggleEnabled(!this.enable);

        resolve(this.enable);
      } catch (err) {
        this.toggleEnabled(false);
        reject(err);
      }
    });
  }

  private toggleEnabled(enabled: boolean) {
    this.enable = enabled;
    this.enableSubject.next(this.enable);
  }

  private askPermission(): Promise<any> {
    return new Promise(async (resolve) => {
      if (!navigator || !('permissions' in navigator) || !LinearAccelerationSensor) {
        resolve('denied');
        return;
      }

      try {
        // @ts-ignore
        const permissions: PermissionStatus = await navigator.permissions.query({name: 'accelerometer'});

        resolve(permissions ? permissions.state : 'denied');
      } catch (err) {
        resolve('denied');
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

  watch(): Observable<boolean> {
    return this.triggerSubject.asObservable();
  }

  async save() {
    await set('deckdeckgo_accelerometer', {
      enable: this.enable,
      frequency: this.frequency,
      sensibility: this.sensibility,
      takeUntil: this.takeUntil,
      delay: this.delay
    });
  }

  async init() {
    const savedValues: AccelerometerValues = await get('deckdeckgo_accelerometer');
    if (savedValues) {
      this.enable = savedValues.enable;
      this.frequency = savedValues.frequency;
      this.sensibility = savedValues.sensibility;
      this.takeUntil = savedValues.takeUntil;
      this.delay = savedValues.delay;

      if (this.enable) {
        await this.askPermission();
      }

      this.enableSubject.next(this.enable);
    }

    this.initializedSubject.next(true);
  }

  watchInitialized(): Observable<boolean> {
    return this.initializedSubject.asObservable();
  }

  watchEnabled(): Observable<boolean> {
    return this.enableSubject.asObservable();
  }
}
