import {Store} from './store';

export class ErrorStore extends Store<string | undefined> {
  private static instance: ErrorStore;

  private error: string | undefined = undefined;

  private constructor() {
    super();
  }

  static getInstance() {
    if (!ErrorStore.instance) {
      ErrorStore.instance = new ErrorStore();
    }
    return ErrorStore.instance;
  }

  set(error: string | undefined) {
    this.error = error;

    this.populate(error);
  }

  get(): string | undefined {
    return this.error;
  }

  override subscribe(callback: (data: string | undefined) => void): () => void {
    const unsubscribe: () => void = super.subscribe(callback);

    callback(this.error);

    return unsubscribe;
  }
}
