import {Component, Element, Event, EventEmitter, h, Method, Prop, State, Watch} from '@stencil/core';

// Types
import {
  DeckdeckgoDrawAction,
  DeckdeckgoEvent,
  DeckdeckgoEventDraw,
  DeckdeckgoEventEmitter,
  DeckdeckgoEventType,
  DeckdeckgoSlideAction,
  DeckdeckgoDeckDefinition,
  DeckdeckgoSlideDefinition,
  ConnectionState
} from '@deckdeckgo/types';

import {isMobile} from '@deckdeckgo/utils';

import {Arrow, Circle, Drawable, Pencil} from '@deckdeckgo/remote-utils';

import store from '../../stores/remote.store';

// Services
import {CommunicationService} from '../../services/communication/communication.service';

@Component({
  tag: 'deckgo-remote',
  styleUrl: 'deckdeckgo-remote.scss',
  shadow: true
})
export class DeckdeckgoRemote {
  @Element() el: HTMLElement;

  @Prop() room: string;
  @Prop() socketUrl: string;

  @Prop() width: number;
  @Prop() height: number;
  @Prop() length: number;

  @Prop({mutable: true}) deck: DeckdeckgoDeckDefinition;

  @Prop() autoConnect: boolean = true;

  @State() canvasWidth: number;

  @Event() state: EventEmitter<ConnectionState>;
  @Event() event: EventEmitter<DeckdeckgoEvent>;

  private destroyStateListener;
  private destroyEventListener;

  private ctx: CanvasRenderingContext2D;
  private drawables: Drawable[] = [];

  private startX: number;
  private startY: number;

  private leftOffset: number = 0;

  private communicationService: CommunicationService;

  constructor() {
    this.communicationService = CommunicationService.getInstance();
  }

  async componentDidLoad() {
    this.destroyStateListener = store.onChange('state', (state: ConnectionState) => {
      this.state.emit(state);
    });

    this.destroyEventListener = store.onChange('$event', async ($event: DeckdeckgoEvent) => {
      if ($event.emitter === DeckdeckgoEventEmitter.APP) {
        if ($event.type === DeckdeckgoEventType.SLIDES_REQUEST) {
          // If app is asking for the deck length, how many slides, we answer directly
          await this.sendSlidesToApp(DeckdeckgoEventType.SLIDES_ANSWER);
        } else if ($event.type === DeckdeckgoEventType.CLEAR_SLIDE) {
          await this.clear();
        } else if ($event.type === DeckdeckgoEventType.START_DRAWING) {
          await this.startDrawing($event as DeckdeckgoEventDraw);
        } else if ($event.type === DeckdeckgoEventType.END_DRAWING) {
          await this.endDrawing($event as DeckdeckgoEventDraw);
        } else if ($event.type === DeckdeckgoEventType.DRAW) {
          await this.draw($event as DeckdeckgoEventDraw);
        } else {
          // Else it's a command to apply on the deck, we propagate
          this.event.emit($event);
        }
      }
    });

    await this.initConnect();

    this.initCanvasWidth();

    await this.initContext();
  }

  @Watch('width')
  @Watch('length')
  private initCanvasWidth() {
    if (this.width && this.length) {
      this.canvasWidth = this.width * this.length;
    }
  }

  async disconnectedCallback() {
    await this.communicationService.disconnect();

    if (this.destroyStateListener) {
      this.destroyStateListener();
    }

    if (this.destroyEventListener) {
      this.destroyEventListener();
    }
  }

  @Watch('room')
  async onRoomChange() {
    await this.initConnect();
  }

  @Watch('socketUrl')
  async onSocketUrlChange() {
    await this.initConnect();
  }

  private async initConnect() {
    if (!this.autoConnect) {
      return;
    }

    await this.connect();
  }

  @Method()
  connect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      if (!this.room) {
        resolve();
        return;
      }

      this.communicationService.room = this.room;
      this.communicationService.socketUrl = this.socketUrl;

      await this.communicationService.disconnect();
      await this.communicationService.connect();

      resolve();
    });
  }

  @Method()
  disconnect(): Promise<void> {
    return new Promise<void>(async (resolve) => {
      await this.communicationService.disconnect();

      resolve();
    });
  }

  @Method()
  async start(appSocketId: string) {
    await this.communicationService.start(appSocketId);
  }

  private initContext(): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      this.ctx = canvas.getContext('2d');

      resolve();
    });
  }

  private setCanvasIndex(zIndex: number): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      canvas.style.zIndex = '' + zIndex;

      resolve();
    });
  }

  private startDrawing(event: DeckdeckgoEventDraw): Promise<void> {
    return new Promise<void>(async (resolve) => {
      this.startX = this.interpolateX(event) - this.leftOffset;
      this.startY = this.interpolateY(event);

      if (event.action === DeckdeckgoDrawAction.ARROW) {
        this.drawables.push(new Arrow({x: this.startX, y: this.startY}, {x: this.startX, y: this.startY}, event.color));
      }

      if (event.action === DeckdeckgoDrawAction.CIRCLE) {
        this.drawables.push(
          new Circle(
            {x: this.startX, y: this.startY},
            {
              x: this.startX,
              y: this.startY
            },
            event.color
          )
        );
      }

      await this.setCanvasIndex(1);

      resolve();
    });
  }

  private draw(event: DeckdeckgoEventDraw): Promise<void> {
    return new Promise<void>((resolve) => {
      this.ctx.beginPath();

      const toX: number = this.interpolateX(event) - this.leftOffset;
      const toY: number = this.interpolateY(event);

      if (event.action === DeckdeckgoDrawAction.PENCIL) {
        this.drawables.push(new Pencil({x: this.startX, y: this.startY}, {x: toX, y: toY}, event.color));
        this.startX = toX;
        this.startY = toY;
      }

      if (event.action === DeckdeckgoDrawAction.ARROW) {
        this.drawables[this.drawables.length - 1] = new Arrow({x: this.startX, y: this.startY}, {x: toX, y: toY}, event.color);
      }

      if (event.action === DeckdeckgoDrawAction.CIRCLE) {
        this.drawables[this.drawables.length - 1] = new Circle(
          {x: this.startX, y: this.startY},
          {
            x: toX,
            y: toY
          },
          event.color
        );
      }
      this.drawElements();

      resolve();
    });
  }

  private drawElements() {
    this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
    for (const drawable of this.drawables) {
      drawable.draw(this.ctx);
    }
  }

  private endDrawing(event: DeckdeckgoEventDraw): Promise<void> {
    return new Promise<void>(async (resolve) => {
      const toX: number = this.interpolateX(event) - this.leftOffset;
      const toY: number = this.interpolateY(event);

      if (event.action === DeckdeckgoDrawAction.ARROW) {
        this.drawables[this.drawables.length - 1] = new Arrow({x: this.startX, y: this.startY}, {x: toX, y: toY}, event.color);
      }

      if (event.action === DeckdeckgoDrawAction.CIRCLE) {
        this.drawables[this.drawables.length - 1] = new Circle(
          {x: this.startX, y: this.startY},
          {
            x: toX,
            y: toY
          },
          event.color
        );
      }

      this.startX = null;
      this.startY = null;
      await this.setCanvasIndex(0);

      resolve();
    });
  }

  private interpolateX(event: DeckdeckgoEventDraw): number {
    const ratio: number = this.width / event.windowWidth;
    return event.clientX * ratio;
  }

  private interpolateY(event: DeckdeckgoEventDraw): number {
    const ratio: number = this.height / event.windowHeight;
    return event.clientY * ratio;
  }

  private sendSlidesToApp(type: DeckdeckgoEventType): Promise<void> {
    return new Promise<void>((resolve) => {
      this.communicationService.emit({
        type: type,
        emitter: DeckdeckgoEventEmitter.DECK,
        length: this.length,
        deck: this.deck,
        mobile: isMobile()
      });

      resolve();
    });
  }

  @Method()
  async updateSlides() {
    await this.sendSlidesToApp(DeckdeckgoEventType.DECK_UPDATE);
  }

  @Method()
  async updateSlide(index: number, slide: DeckdeckgoSlideDefinition) {
    return new Promise<void>((resolve) => {
      if (!this.deck || !this.deck.slides || this.deck.slides.length <= index || index < 0) {
        resolve();
        return;
      }

      this.deck.slides[index] = slide;

      if (!slide) {
        resolve();
        return;
      }

      this.communicationService.emit({
        type: DeckdeckgoEventType.SLIDE_UPDATE,
        emitter: DeckdeckgoEventEmitter.DECK,
        index: index,
        slide: slide
      });

      resolve();
    });
  }

  @Method()
  deleteSlide(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.communicationService.emit({
        type: DeckdeckgoEventType.DELETE_SLIDE,
        emitter: DeckdeckgoEventEmitter.DECK
      });

      resolve();
    });
  }

  @Method()
  updateReveal(reveal: boolean): Promise<void> {
    return new Promise<void>((resolve) => {
      this.communicationService.emit({
        type: DeckdeckgoEventType.DECK_REVEAL_UPDATE,
        emitter: DeckdeckgoEventEmitter.DECK,
        reveal: reveal
      });

      resolve();
    });
  }

  @Method()
  moveDraw(leftOffset: number, transitionDuration: string): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      this.leftOffset = leftOffset;

      canvas.style.setProperty('--left-offset', '' + this.leftOffset + 'px');
      canvas.style.setProperty('--left-offset-transition', transitionDuration);

      resolve();
    });
  }

  @Method()
  async nextSlide(slideAnimation: boolean = false) {
    this.emitSlidePrevNext(DeckdeckgoEventType.NEXT_SLIDE, slideAnimation);
  }

  @Method()
  async prevSlide(slideAnimation: boolean = false) {
    this.emitSlidePrevNext(DeckdeckgoEventType.PREV_SLIDE, slideAnimation);
  }

  @Method()
  async slideTo(index: number, speed?: number | undefined) {
    this.communicationService.emit({
      type: DeckdeckgoEventType.SLIDE_TO,
      emitter: DeckdeckgoEventEmitter.DECK,
      index: index,
      speed: speed
    });
  }

  @Method()
  async play() {
    this.communicationService.emit({
      type: DeckdeckgoEventType.SLIDE_ACTION,
      emitter: DeckdeckgoEventEmitter.DECK,
      action: DeckdeckgoSlideAction.PLAY
    });
  }

  @Method()
  async pause() {
    this.communicationService.emit({
      type: DeckdeckgoEventType.SLIDE_ACTION,
      emitter: DeckdeckgoEventEmitter.DECK,
      action: DeckdeckgoSlideAction.PAUSE
    });
  }

  private clear(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.ctx.beginPath();
      this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
      this.ctx.stroke();
      this.ctx.closePath();

      this.drawables = [];

      resolve();
    });
  }

  private emitSlidePrevNext(type: DeckdeckgoEventType, slideAnimation: boolean) {
    this.communicationService.emit({type: type, emitter: DeckdeckgoEventEmitter.DECK, slideAnimation: slideAnimation});
  }

  render() {
    return <canvas width={this.canvasWidth} height={this.height}></canvas>;
  }
}
