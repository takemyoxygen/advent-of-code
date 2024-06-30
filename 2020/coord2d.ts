export type Point = { x: number; y: number };

export function move(start: Point, delta: Point, steps: number = 1): Point {
  return { x: start.x + delta.x * steps, y: start.y + delta.y * steps };
}

export function manhattan({ x, y }: Point): number {
  return Math.abs(x) + Math.abs(y);
}

export class PointSet {
  private points = new Map<string, Point>();

  has(p: Point): boolean {
    return this.points.has(PointSet.key(p));
  }

  add(p: Point) {
    this.points.set(PointSet.key(p), p);
  }

  size() {
    return this.points.size;
  }

  values() {
    return Array.from(this.points.values());
  }

  delete(p: Point) {
    this.points.delete(PointSet.key(p));
  }

  static key(p: Point) {
    return `${p.x}-${p.y}`;
  }
}
