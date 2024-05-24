export type Point = { x: number; y: number };

export function move(start: Point, delta: Point): Point {
  return { x: start.x + delta.x, y: start.y + delta.y };
}
