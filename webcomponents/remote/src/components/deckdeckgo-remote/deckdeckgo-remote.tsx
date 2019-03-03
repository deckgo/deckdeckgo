import {Component, Element, Event, EventEmitter, Method, Prop, State, Watch} from '@stencil/core';

import {Subscription} from 'rxjs';

// Types
import {
  DeckdeckgoEventDraw,
  DeckdeckgoEvent,
  DeckdeckgoEventType,
  DeckdeckgoEventEmitter,
  DeckdeckgoDrawAction,
  DeckdeckgoSlideDefinition
} from 'deckdeckgo-types';

// Services
import {CommunicationService, ConnectionState} from '../../services/communication/communication.service';

@Component({
  tag: 'deckgo-remote',
  styleUrl: 'deckdeckgo-remote.scss',
  shadow: true
})
export class DeckdeckgoRemote {
  @Element() el: HTMLElement;

  @Prop() room: string;
  @Prop() server: string;

  @Prop() width: number;
  @Prop() height: number;
  @Prop() length: number;

  @Prop() slides: DeckdeckgoSlideDefinition[];

  @State() canvasWidth: number;

  @Event() state: EventEmitter<ConnectionState>;
  @Event() event: EventEmitter<DeckdeckgoEvent>;

  private subscriptionState: Subscription;
  private subscriptionEvent: Subscription;

  private ctx: CanvasRenderingContext2D;

  private startX: number;
  private startY: number;

  private leftOffset: number = 0;

  constructor(private communicationService: CommunicationService) {
    this.communicationService = CommunicationService.getInstance();
  }

  async componentDidLoad() {
    this.subscriptionState = this.communicationService.watchState().subscribe((state: ConnectionState) => {
      this.state.emit(state);
    });

    this.subscriptionEvent = this.communicationService.watchEvent().subscribe(async (event: DeckdeckgoEvent) => {
      if (event.emitter === DeckdeckgoEventEmitter.APP) {
        if (event.type === DeckdeckgoEventType.SLIDES_REQUEST) {
          // If app is asking for the deck length, how many slides, we answer directly
          await this.answerApp();
        } else if (event.type === DeckdeckgoEventType.CLEAR_SLIDE) {
          await this.clear();
        } else if (event.type === DeckdeckgoEventType.START_DRAWING) {
          await this.startDrawing((event as DeckdeckgoEventDraw));
        } else if (event.type === DeckdeckgoEventType.END_DRAWING) {
          await this.endDrawing((event as DeckdeckgoEventDraw));
        } else if (event.type === DeckdeckgoEventType.DRAW) {
          await this.draw((event as DeckdeckgoEventDraw));
        } else {
          // Else it's a command to apply on the deck, we propagate
          this.event.emit(event);
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

  async componentDidUnload() {
    await this.communicationService.disconnect();

    if (this.subscriptionState) {
      this.subscriptionState.unsubscribe();
    }

    if (this.subscriptionEvent) {
      this.subscriptionEvent.unsubscribe();
    }
  }

  @Watch('room')
  @Watch('server')
  private async initConnect() {
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
      this.communicationService.serverUrl = this.server;

      await this.communicationService.disconnect();
      await this.communicationService.connect();

      resolve();
    });
  }

  private initContext(): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.shadowRoot.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      this.ctx = canvas.getContext("2d");

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

      await this.setCanvasIndex(1);

      resolve();
    });
  }

  private draw(event: DeckdeckgoEventDraw): Promise<void> {
    return new Promise<void>((resolve) => {
      this.ctx.beginPath();

      const toX: number = this.interpolateX(event) - this.leftOffset;
      const toY: number = this.interpolateY(event);

      if (event.action === DeckdeckgoDrawAction.CIRCLE) {
        this.drawCircle(toX, toY);
      } else {
        this.drawPencil(toX, toY);
      }

      this.ctx.strokeStyle = event.color ? event.color : 'red';
      this.ctx.lineWidth = 6;

      this.ctx.stroke();
      this.ctx.closePath();

      resolve();
    });
  }

  private drawPencil(toX: number, toY: number) {
    this.ctx.moveTo(this.startX, this.startY);
    this.ctx.lineTo(toX, toY);

    this.startX = toX;
    this.startY = toY;
  }

  private drawCircle(toX: number, toY: number) {
    this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
    this.ctx.moveTo(this.startX, this.startY + (toY - this.startY) / 2);
    this.ctx.bezierCurveTo(this.startX, this.startY, toX, this.startY, toX, this.startY + (toY - this.startY) / 2);
    this.ctx.bezierCurveTo(toX, toY, this.startX, toY, this.startX, this.startY + (toY - this.startY) / 2);
  }

  private endDrawing(_event: DeckdeckgoEventDraw): Promise<void> {
    return new Promise<void>(async (resolve) => {
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

  private answerApp(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.communicationService.emit({
        type: DeckdeckgoEventType.SLIDES_ANSWER,
        emitter: DeckdeckgoEventEmitter.DECK,
        length: this.length,
        slides: this.slides
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
  nextSlide() {
    this.emitSlidePrevNext(DeckdeckgoEventType.NEXT_SLIDE);
  }

  @Method()
  prevSlide() {
    this.emitSlidePrevNext(DeckdeckgoEventType.PREV_SLIDE);
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

  private clear(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.ctx.beginPath();
      this.ctx.clearRect(-1 * this.leftOffset, 0, this.width, this.height);
      this.ctx.stroke();
      this.ctx.closePath();

      resolve();
    });
  }

  private emitSlidePrevNext(type: DeckdeckgoEventType) {
    this.communicationService.emit({type: type, emitter: DeckdeckgoEventEmitter.DECK});
  }

  render() {
    return <canvas width={this.canvasWidth} height={this.height}></canvas>;
  }
}
