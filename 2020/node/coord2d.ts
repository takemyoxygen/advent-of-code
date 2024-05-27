export type Point = { x: number; y: number };

export function move(start: Point, delta: Point, steps: number = 1): Point {
  return { x: start.x + delta.x * steps, y: start.y + delta.y * steps };
}

export function manhattan({ x, y }: Point): number {
  return Math.abs(x) + Math.abs(y);
}
