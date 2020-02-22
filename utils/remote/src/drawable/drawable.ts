export interface Point {
  x: number;
  y: number;
}

export interface Drawable {
  draw(ctx: CanvasRenderingContext2D): void;
}
