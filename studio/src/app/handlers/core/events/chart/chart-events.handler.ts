import {get} from 'idb-keyval';

export class ChartEventsHandler {
  init(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (document) {
        document.addEventListener('chartCustomLoad', this.onCustomLoad, false);
      }

      resolve();
    });
  }

  destroy() {
    document.removeEventListener('chartCustomLoad', this.onCustomLoad, true);
  }

  private onCustomLoad = async ($event: CustomEvent) => {
    if (!$event || !$event.detail || !$event.target || !($event.target instanceof HTMLElement)) {
      return;
    }

    await this.loadChart($event);
  };

  private loadChart($event: CustomEvent<string>): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const src: string = $event.detail;

      if (!src || src === undefined) {
        resolve();
        return;
      }

      if (src.indexOf('http') === -1) {
        await this.loadLocalData($event.target as HTMLElement, src);
      } else {
        await this.loadData($event.target as HTMLElement, src);
      }

      resolve();
    });
  }

  private loadData(target: HTMLElement, src: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        const rawResponse: Response = await fetch(src);

        if (!rawResponse || !rawResponse.ok) {
          console.error(`Chart data ${src} can not be fetched.`);
          resolve();
          return;
        }

        const content = await rawResponse.text();
        await (target as any).postCustomLoad(content);

        resolve();
      } catch (err) {
        console.error(err);
        resolve();
      }
    });
  }

  private loadLocalData(target: HTMLElement, src: string): Promise<void> {
    return new Promise<void>(async (resolve) => {
      try {
        const savedData: File = await get(src);

        const URL = window.URL || window.webkitURL;
        const chartUrl: string = URL.createObjectURL(savedData);

        await this.loadData(target, chartUrl);

        resolve();
      } catch (err) {
        // If error then no chart is displayed
        console.error(err);
        resolve();
      }
    });
  }
}
