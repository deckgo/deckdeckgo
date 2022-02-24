import {nanoid} from 'nanoid';

export abstract class Store<T> {
  private callbacks: {id: string; callback: (data: T | null) => void}[] = [];

  protected populate(data: T | null) {
    this.callbacks.forEach(({callback}: {id: string; callback: (data: T | null) => void}) => callback(data));
  }

  subscribe(callback: (data: T | null) => void): () => void {
    const callbackId: string = nanoid();
    this.callbacks.push({id: callbackId, callback});

    return () => (this.callbacks = this.callbacks.filter(({id}: {id: string; callback: (data: T | null) => void}) => id !== callbackId));
  }
}
