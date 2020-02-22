import {Component, Element, h, Method, Prop, State, Watch} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';

import {Arrow, Circle, Drawable, Pencil} from '@deckdeckgo/remote-utils';

// Types
import {DeckdeckgoDrawAction, DeckdeckgoEventEmitter, DeckdeckgoEventType} from '@deckdeckgo/types';

// Services
import {CommunicationService} from '../../services/communication/communication.service';

@Component({
  tag: 'app-draw',
  styleUrl: 'app-draw.scss'
})
export class AppDraw {
  @Element() el: HTMLElement;

  @Prop() width: number;
  @Prop() height: number;

  @Prop() widthOffset: number = 0;
  @Prop() heightOffset: number = 0;

  @Prop() slides: number;

  @State() private canvasWidth: number;

  private canvas: HTMLCanvasElement;
  private ctx: CanvasRenderingContext2D;

  private startX: number;
  private startY: number;

  private drawables: Drawable[] = [];

  private deckLeftOffset: number = 0;

  @State()
  private drawEvents: boolean = false;

  private drawAction: boolean = false;

  @State()
  private action: DeckdeckgoDrawAction = DeckdeckgoDrawAction.PENCIL;

  @State()
  private color: string = 'red';

  private communicationService: CommunicationService;

  constructor() {
    this.communicationService = CommunicationService.getInstance();
  }

  async componentDidLoad() {
    this.initCanvasWidth();

    await this.initContext();
  }

  @Watch('width')
  @Watch('slides')
  private initCanvasWidth() {
    if (this.width && this.slides) {
      this.canvasWidth = this.width * this.slides;
    }
  }

  private initContext(): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      this.ctx = canvas.getContext('2d', {desynchronized: true});

      resolve();
    });
  }

  @Method()
  moveDraw(leftOffset: number, transitionDuration: string): Promise<void> {
    return new Promise<void>((resolve) => {
      const canvas: HTMLCanvasElement = this.el.querySelector('canvas');

      if (!canvas) {
        resolve();
        return;
      }

      this.deckLeftOffset = leftOffset;

      canvas.style.setProperty('--left-offset', '' + this.deckLeftOffset + 'px');
      canvas.style.setProperty('--left-offset-transition', transitionDuration);

      resolve();
    });
  }

  private start(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.canvas = this.el.querySelector('canvas');

      if (!this.canvas) {
        resolve();
        return;
      }

      this.canvas.style.pointerEvents = 'all';

      this.canvas.addEventListener('mousedown', this.startEvent, {passive: true});
      this.canvas.addEventListener('touchstart', this.startEvent, {passive: true});
      this.canvas.addEventListener('mouseup', this.endEvent, {passive: true});
      this.canvas.addEventListener('touchend', this.endEvent, {passive: true});
      this.canvas.addEventListener('mousemove', this.drawEvent, {passive: true});
      this.canvas.addEventListener('touchmove', this.drawEvent, {passive: true});

      resolve();
    });
  }

  private end(): Promise<void> {
    return new Promise<void>((resolve) => {
      this.canvas = this.el.querySelector('canvas');

      if (!this.canvas) {
        resolve();
        return;
      }

      this.canvas.style.pointerEvents = 'none';

      this.canvas.removeEventListener('mousedown', this.startEvent, true);
      this.canvas.removeEventListener('touchstart', this.startEvent, true);
      this.canvas.removeEventListener('mouseup', this.endEvent, true);
      this.canvas.removeEventListener('touchend', this.endEvent, true);
      this.canvas.removeEventListener('mousemove', this.drawEvent, true);
      this.canvas.removeEventListener('touchmove', this.drawEvent, true);

      resolve();
    });
  }

  private startEvent = (e: MouseEvent) => {
    this.emit(DeckdeckgoEventType.START_DRAWING, e);

    this.startX = unifyEvent(e).clientX - this.deckLeftOffset - this.widthOffset;
    this.startY = unifyEvent(e).clientY - this.heightOffset;

    if (this.action === DeckdeckgoDrawAction.ARROW) {
      this.drawables.push(new Arrow({x: this.startX, y: this.startY}, {x: this.startX, y: this.startY}, this.color));
    }

    if (this.action === DeckdeckgoDrawAction.CIRCLE) {
      this.drawables.push(
        new Circle(
          {x: this.startX, y: this.startY},
          {
            x: this.startX,
            y: this.startY
          },
          this.color
        )
      );
    }

    this.drawEvents = true;
  };

  private endEvent = (e: MouseEvent) => {
    this.emit(DeckdeckgoEventType.END_DRAWING, e);

    const toX: number = unifyEvent(e).clientX - this.deckLeftOffset - this.widthOffset;
    const toY: number = unifyEvent(e).clientY - this.heightOffset;

    if (this.action === DeckdeckgoDrawAction.ARROW) {
      this.drawables[this.drawables.length - 1] = new Arrow({x: this.startX, y: this.startY}, {x: toX, y: toY}, this.color);
    }

    if (this.action === DeckdeckgoDrawAction.CIRCLE) {
      this.drawables[this.drawables.length - 1] = new Circle(
        {x: this.startX, y: this.startY},
        {
          x: toX,
          y: toY
        },
        this.color
      );
    }

    this.drawEvents = false;
  };

  private drawEvent = (e: MouseEvent) => {
    if (!this.drawEvents) {
      return;
    }

    this.emit(DeckdeckgoEventType.DRAW, e);

    const toX: number = unifyEvent(e).clientX - this.deckLeftOffset - this.widthOffset;
    const toY: number = unifyEvent(e).clientY - this.heightOffset;

    if (this.action === DeckdeckgoDrawAction.PENCIL) {
      this.drawables.push(new Pencil({x: this.startX, y: this.startY}, {x: toX, y: toY}, this.color));
      this.startX = toX;
      this.startY = toY;
    }

    if (this.action === DeckdeckgoDrawAction.ARROW) {
      this.drawables[this.drawables.length - 1] = new Arrow({x: this.startX, y: this.startY}, {x: toX, y: toY}, this.color);
    }

    if (this.action === DeckdeckgoDrawAction.CIRCLE) {
      this.drawables[this.drawables.length - 1] = new Circle(
        {x: this.startX, y: this.startY},
        {
          x: toX,
          y: toY
        },
        this.color
      );
    }
    this.draw();
  };

  private draw() {
    this.ctx.clearRect(-1 * this.deckLeftOffset, 0, this.width, this.height);
    for (const drawable of this.drawables) {
      drawable.draw(this.ctx);
    }
  }

  private switchTool(e: UIEvent, action: DeckdeckgoDrawAction) {
    e.stopPropagation();

    this.action = action;
  }

  private switchColor(e: UIEvent) {
    e.stopPropagation();

    this.color = this.color === 'red' ? 'black' : 'red';
  }

  private async startStopDrawing() {
    if (this.drawAction) {
      await this.end();
    } else {
      await this.start();
    }

    this.drawAction = !this.drawAction;
  }

  private emit(type: DeckdeckgoEventType, e: any) {
    this.communicationService.emit({
      type: type,
      emitter: DeckdeckgoEventEmitter.APP,
      action: this.action,
      clientX: unifyEvent(e).clientX - this.widthOffset,
      clientY: unifyEvent(e).clientY - this.heightOffset,
      windowWidth: this.width,
      windowHeight: this.height,
      color: this.color
    });
  }

  private clear(e: UIEvent): Promise<void> {
    return new Promise<void>((resolve) => {
      e.stopPropagation();

      this.communicationService.emit({
        type: DeckdeckgoEventType.CLEAR_SLIDE,
        emitter: DeckdeckgoEventEmitter.APP
      });

      this.ctx.beginPath();
      this.ctx.clearRect(-1 * this.deckLeftOffset, 0, this.width, this.height);
      this.ctx.stroke();
      this.ctx.closePath();

      this.drawables = [];

      resolve();
    });
  }

  render() {
    const styleColorPicker = {
      color: this.color === 'red' ? 'black' : 'red'
    };

    return [
      <canvas width={this.canvasWidth} height={this.height}></canvas>,
      <ion-fab vertical="bottom" horizontal="end" slot="fixed" class={this.drawEvents ? 'hidden' : ''}>
        <ion-fab-button onClick={() => this.startStopDrawing()} color="dark">
          <ion-icon name="brush"></ion-icon>
        </ion-fab-button>
        <ion-fab-list side="start">{this.renderActions()}</ion-fab-list>
        <ion-fab-list side="top">
          <ion-fab-button color="light" onClick={(e: UIEvent) => this.switchColor(e)}>
            <ion-icon name="color-palette" style={styleColorPicker}></ion-icon>
          </ion-fab-button>
          <ion-fab-button color="light" onClick={(e: UIEvent) => this.clear(e)}>
            <ion-icon name="trash"></ion-icon>
          </ion-fab-button>
        </ion-fab-list>
      </ion-fab>
    ];
  }

  // TODO: Unleash arrows
  // <ion-fab-button
  // color={this.action === DeckdeckgoDrawAction.ARROW ? 'primary' : 'light'}
  // onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.ARROW)}>
  // <ion-icon ios="arrow-forward" md="arrow-forward"></ion-icon>
  // </ion-fab-button>,

  private renderActions() {
    return [
      <ion-fab-button
        color={this.action === DeckdeckgoDrawAction.CIRCLE ? 'primary' : 'light'}
        onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.CIRCLE)}>
        <ion-icon name="radio-button-off"></ion-icon>
      </ion-fab-button>,
      <ion-fab-button
        color={this.action === DeckdeckgoDrawAction.PENCIL ? 'primary' : 'light'}
        onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.PENCIL)}>
        <ion-icon name="pencil"></ion-icon>
      </ion-fab-button>
    ];
  }
}
