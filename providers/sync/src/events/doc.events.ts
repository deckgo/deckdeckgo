import {ChartEvents} from './chart/chart.events';
import {DocDataEvents} from './data/doc.data.events';
import {ImageLazyEvents} from './image/image.lazy.events';
import {ImageLoadEvents} from './image/image.load.events';

export class DocEvents {
  private readonly chartEvents: ChartEvents = new ChartEvents();
  private readonly imageLoadEvents: ImageLoadEvents = new ImageLoadEvents();
  private readonly imageLazyEvents: ImageLazyEvents = new ImageLazyEvents();

  private readonly docDataEvents: DocDataEvents = new DocDataEvents();

  private static instance: DocEvents;

  private constructor() {
    // Private constructor, singleton
  }

  static getInstance() {
    if (!DocEvents.instance) {
      DocEvents.instance = new DocEvents();
    }
    return DocEvents.instance;
  }

  initDomEvents(containerRef: HTMLElement) {
    this.imageLoadEvents.init();
    this.chartEvents.init();
    // TODO: code events

    this.imageLazyEvents.init(containerRef);
  }

  destroyDomEvents() {
    this.imageLoadEvents.destroy();
    this.chartEvents.destroy();
    // TODO: code events

    this.imageLazyEvents.destroy();
  }

  initDataEvents() {
    this.docDataEvents.init();
  }

  destroyDataEvents() {
    this.docDataEvents.destroy();
  }
}
