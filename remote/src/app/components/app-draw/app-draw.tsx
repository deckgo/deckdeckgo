import {Component, Element, Event, EventEmitter, h, Method, Prop, State, Watch} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';
// Types
import {DeckdeckgoDrawAction, DeckdeckgoEventEmitter, DeckdeckgoEventType} from '@deckdeckgo/types';
// Services
import {CommunicationService} from '../../services/communication/communication.service';

interface Point {
  x: number;
  y: number;
}

interface Drawable {
  draw(ctx: CanvasRenderingContext2D);
}

class Circle implements Drawable {
  private readonly from: Point;
  private readonly to: Point;
  private readonly color: string;

  constructor(from: Point, to: Point, color: string) {
    this.from = from;
    this.to = to;
    this.color = color;
  }

  draw(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();

    ctx.moveTo(this.from.x, this.from.y + (this.to.y - this.from.y) / 2);
    ctx.bezierCurveTo(this.from.x, this.from.y, this.to.x, this.from.y, this.to.x, this.from.y + (this.to.y - this.from.y) / 2);
    ctx.bezierCurveTo(this.to.x, this.to.y, this.from.x, this.to.y, this.from.x, this.from.y + (this.to.y - this.from.y) / 2);

    ctx.strokeStyle = this.color;
    ctx.lineWidth = 3;

    ctx.stroke();
    ctx.closePath();
  }
}

class Pencil implements Drawable {
  private readonly from: Point;
  private readonly to: Point;
  private readonly color: string;

  constructor(from: Point, to: Point, color: string) {
    this.from = from;
    this.to = to;
    this.color = color;
  }

  draw(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();

    ctx.moveTo(this.from.x, this.from.y);
    ctx.lineTo(this.to.x, this.to.y);
    ctx.strokeStyle = this.color;
    ctx.lineWidth = 3;

    ctx.stroke();
    ctx.closePath();
  }
}

class Arrow implements Drawable {
  private readonly from: Point;
  private readonly to: Point;
  private readonly color: string;

  constructor(from: Point, to: Point, color: string) {
    this.from = from;
    this.to = to;
    this.color = color;
  }

  draw(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();

    ctx.moveTo(this.from.x, this.from.y);
    ctx.lineTo(this.to.x, this.to.y);
    ctx.strokeStyle = this.color;
    ctx.lineWidth = 3;

    ctx.stroke();
    ctx.closePath();

    this.drawTriangle(ctx);
  }

  private drawTriangle(ctx: CanvasRenderingContext2D) {
    ctx.beginPath();

    const length: number = Math.sqrt(Math.pow(this.to.y - this.from.y, 2) + Math.pow(this.to.x - this.from.x, 2));
    const size: number = Math.min(length, 20);

    // https://stackoverflow.com/a/36805543/5404186

    let angle: number = Math.atan2(this.to.y - this.from.y, this.to.x - this.from.x);
    let x: number = size * Math.cos(angle) + this.to.x;
    let y: number = size * Math.sin(angle) + this.to.y;

    ctx.moveTo(x, y);

    angle += (1 / 3) * (2 * Math.PI);
    x = size * Math.cos(angle) + this.to.x;
    y = size * Math.sin(angle) + this.to.y;

    ctx.lineTo(x, y);

    angle += (1 / 3) * (2 * Math.PI);
    x = size * Math.cos(angle) + this.to.x;
    y = size * Math.sin(angle) + this.to.y;

    ctx.lineTo(x, y);

    ctx.fillStyle = this.color;
    ctx.fill();

    ctx.closePath();
  }
}

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

  private drawEvents: boolean = false;
  private drawAction: boolean = false;

  @State()
  private action: DeckdeckgoDrawAction = DeckdeckgoDrawAction.PENCIL;

  @Event() drawing: EventEmitter<boolean>;

  @State() private color: string = 'red';

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

    this.drawing.emit(this.drawAction);
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
      <ion-fab vertical="bottom" horizontal="end" slot="fixed">
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

  private renderActions() {
    return [
      <ion-fab-button
        color={this.action === DeckdeckgoDrawAction.ARROW ? 'primary' : 'light'}
        onClick={(e: UIEvent) => this.switchTool(e, DeckdeckgoDrawAction.ARROW)}>
        <ion-icon ios="arrow-forward" md="arrow-forward"></ion-icon>
      </ion-fab-button>,
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
