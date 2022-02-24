import busyStore from '../../../stores/busy.store';

export class BusyEvents {
  init() {
    document.addEventListener('ddgBusy', this.onBusy, {passive: true});
  }

  destroy() {
    document.removeEventListener('ddgBusy', this.onBusy, true);
  }

  private onBusy = async ({detail: busy}: CustomEvent<boolean>) => (busyStore.state.busy = busy);
}
