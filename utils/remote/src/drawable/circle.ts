import {Drawable, Point} from './drawable';

export class Circle implements Drawable {
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
