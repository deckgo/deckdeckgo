import {Component, h, Host, Listen, State} from '@stencil/core';

import {unifyEvent} from '@deckdeckgo/utils';

@Component({
  tag: 'deckgo-flashlight',
  styleUrl: 'flashlight.scss',
  shadow: true,
})
export class DeckgoMdParser {
  @State()
  private mouse: 'moving' | 'idle' = 'idle';

  private points: {x: number; y: number; lifetime: number}[] = [];

  private idleMouseTimer: number;
  private readonly idleMouseTimeout: number = 2000;

  private canvasRef!: HTMLCanvasElement;

  @State()
  private viewport: {width: number; height: number};

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
        const point = this.points[i];
        let lastPoint;

        if (this.points[i - 1] !== undefined) {
          lastPoint = this.points[i - 1];
        } else {
          lastPoint = point;
        }

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

          // As time increases decrease r and b, increase g to go from purple to green.
          const red = Math.floor(190 - 190 * lifePercent);
          const green = 0;
          const blue = Math.floor(210 + 210 * lifePercent);
          ctx.strokeStyle = `rgb(${red},${green},${blue}`;

          ctx.beginPath();

          ctx.moveTo(lastPoint.x, lastPoint.y);
          ctx.lineTo(point.x, point.y);

          ctx.stroke();

          if (i === this.points.length - 1) {
            ctx.fillStyle = `rgb(${red},${green},${blue}`;
            ctx.beginPath();
            ctx.arc(point.x, point.y, 6 * (1 - lifePercent), 0, 2 * Math.PI);
            ctx.fill();
          }

          ctx.closePath();
        }
      }
      requestAnimationFrame(animatePoints);
    };

    animatePoints();
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
