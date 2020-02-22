import {Drawable, Point} from './drawable';

export class Arrow implements Drawable {
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
