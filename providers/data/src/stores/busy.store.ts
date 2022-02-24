import {Store} from './store';

export class BusyStore extends Store<boolean> {
  private static instance: BusyStore;

  private busy: boolean = false;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!BusyStore.instance) {
      BusyStore.instance = new BusyStore();
    }
    return BusyStore.instance;
  }

  set(busy: boolean) {
    this.busy = busy;

    this.populate(busy);
  }

  get(): boolean {
    return this.busy;
  }

  override subscribe(callback: (data: boolean) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.busy);

    return unsubscribe;
  }
}
