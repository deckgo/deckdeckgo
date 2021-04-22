import {Component, h, Host, Listen, Prop, State} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-flashlight',
  styleUrl: 'flashlight.scss',
  shadow: true,
})
export class DeckgoMdParser {
  /**
   * The RGB red color
   */
  @Prop()
  r: number = 61;

  /**
   * Decrease RGB red color with life span
   */
  @Prop()
  animateR: boolean = true;

  /**
   * The RGB green color (which will fade with time)
   */
  @Prop()
  g: number = 194;

  /**
   * Decrease RGB green color with life span
   */
  @Prop()
  animateG: boolean = false;

  /**
   * The RGB blue color (which will fade with time)
   */
  @Prop()
  b: number = 255;

  /**
   * Decrease RGB blue color with life span
   */
  @Prop()
  animateB: boolean = true;

  @State()
  private mouse: 'moving' | 'idle' = 'idle';

  @State()
  private viewport: {width: number; height: number};

  private points: {x: number; y: number; lifetime: number}[] = [];

  private idleMouseTimer: number;
  private readonly idleMouseTimeout: number = 2000;

  private canvasRef!: HTMLCanvasElement;

  componentWillLoad() {
    this.initViewportSize();
  }

  componentDidLoad() {
    if (matchMedia('(pointer:fine)').matches) {
      this.startAnimation();
      this.initWindowResize();
    }
  }

  disconnectedCallback() {
    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }

    window.removeEventListener('resize', () => this.initViewportSize());
  }

  private initViewportSize() {
    this.viewport = {
      width: screen.width > window.innerWidth ? screen.width : window.innerWidth,
      height: screen.height > window.innerHeight ? screen.height : window.innerHeight,
    };
  }

  private initWindowResize() {
    window.addEventListener('resize', () => this.initViewportSize());
  }

  private startAnimation() {
    if (!this.canvasRef) {
      return;
    }

    const ctx: CanvasRenderingContext2D | null = this.canvasRef.getContext('2d');

    if (!ctx) {
      return;
    }

    // https://noahyamamoto.com/blog/mousetrailanimation
    const animatePoints = () => {
      ctx.clearRect(0, 0, ctx.canvas.width, ctx.canvas.height);
      const duration = (0.7 * 1000) / 60; // Last 80% of a frame per point

      for (let i = 0; i < this.points.length; ++i) {
        const point: {x: number; y: number; lifetime: number} = this.points[i];
        const lastPoint: {x: number; y: number; lifetime: number} = this.points[i - 1] !== undefined ? this.points[i - 1] : point;

        point.lifetime += 1;

        if (point.lifetime > duration) {
          // If the point dies, remove it.
          this.points.shift();
        } else {
          // Otherwise animate it:

          // As the lifetime goes on, lifePercent goes from 0 to 1.
          const lifePercent = point.lifetime / duration;
          const spreadRate = 16 * (1 - lifePercent);

          ctx.lineJoin = 'round';
          ctx.lineWidth = spreadRate;

          const red: number = this.animateR ? Math.floor(this.r - this.r * lifePercent) : this.r;
          const green: number = this.animateG ? Math.floor(this.g - this.g * lifePercent) : this.g;
          const blue: number = this.animateB ? Math.floor(255 + 255 * lifePercent) : this.b;

          const rgbColor: string = `rgb(${red},${green},${blue}`;

          ctx.strokeStyle = rgbColor;

          ctx.beginPath();

          ctx.moveTo(lastPoint.x, lastPoint.y);
          ctx.lineTo(point.x, point.y);

          ctx.stroke();

          if (i === this.points.length - 1) {
            this.drawCircle({ctx, rgbColor, point, lifePercent});
          }

          ctx.closePath();
        }
      }
      requestAnimationFrame(animatePoints);
    };

    animatePoints();
  }

  private drawCircle({
    ctx,
    rgbColor,
    point,
    lifePercent,
  }: {
    ctx: CanvasRenderingContext2D;
    rgbColor: string;
    point: {x: number; y: number; lifetime: number};
    lifePercent: number;
  }) {
    ctx.fillStyle = rgbColor;
    ctx.beginPath();
    ctx.arc(point.x, point.y, 7.4 * (1 - lifePercent), 0, 2 * Math.PI);
    ctx.fill();
  }

  @Listen('mousemove', {passive: true, target: 'document'})
  mousemove($event: MouseEvent) {
    this.move($event);
  }

  @Listen('touchmove', {passive: true, target: 'document'})
  touchmove($event: TouchEvent) {
    this.move($event);
  }

  private move($event: Event) {
    this.clearMouseTimeout();

    if (this.mouse === 'idle') {
      this.mouse = 'moving';
    }

    this.points.push({
      x: unifyEvent($event).clientX,
      y: unifyEvent($event).clientY,
      lifetime: 0,
    });

    this.idleMouseTimer = setTimeout(() => {
      this.mouse = 'idle';
    }, this.idleMouseTimeout);
  }

  private clearMouseTimeout() {
    if (this.idleMouseTimer > 0) {
      clearTimeout(this.idleMouseTimer);
    }
  }

  render() {
    return (
      <Host class={this.mouse === 'moving' ? 'show' : 'hidden'}>
        <canvas ref={(el: HTMLCanvasElement | null) => (this.canvasRef = el)} width={this.viewport.width} height={this.viewport.height} />
      </Host>
    );
  }
}
