import {Drawable, Point} from './drawable';

export class Pencil implements Drawable {
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
