import { type Day } from "./common";
import _ from "lodash";

function round(p1: number[], p2: number[]) {
  const c1 = p1.shift()!;
  const c2 = p2.shift()!;

  if (c1 > c2) {
    p1.push(c1, c2);
  } else {
    p2.push(c2, c1);
  }
}

class PlayedRounds {
  private roundHashes = new Set<string>();

  track(p1: number[], p2: number[]) {
    this.roundHashes.add(this.hash(p1, p2));
  }

  has(p1: number[], p2: number[]) {
    return this.roundHashes.has(this.hash(p1, p2));
  }

  private hash(p1: number[], p2: number[]) {
    return p1.join(",") + " vs " + p2.join(",");
  }
}

function gameRec(p1: number[], p2: number[]): 1 | 2 {
  const played = new PlayedRounds();

  while (p1.length > 0 && p2.length > 0) {
    if (played.has(p1, p2)) {
      return 1;
    }

    played.track(p1, p2);

    roundRec(p1, p2);
  }

  return p1.length === 0 ? 2 : 1;
}

function roundRec(p1: number[], p2: number[]) {
  const c1 = p1.shift()!;
  const c2 = p2.shift()!;

  if (c1 <= p1.length && c2 <= p2.length) {
    if (gameRec(p1.slice(0, c1), p2.slice(0, c2)) === 1) {
      p1.push(c1, c2);
    } else {
      p2.push(c2, c1);
    }
  } else {
    if (c1 > c2) {
      p1.push(c1, c2);
    } else {
      p2.push(c2, c1);
    }
  }
}

function score(hand: number[]): number {
  return hand.reduce((acc, card, idx) => acc + card * (hand.length - idx), 0);
}

const day22: Day<[number[], number[]]> = {
  parseInput(txt) {
    const [player1, player2] = txt.split("\n\n");

    const cards = (pl: string) => pl.split("\n").slice(1).map(Number);

    return [cards(player1), cards(player2)];
  },
  part1([player1, player2]) {
    player1 = [...player1];
    player2 = [...player2];

    while (player1.length > 0 && player2.length > 0) {
      round(player1, player2);
    }

    return score(player1.length === 0 ? player2 : player1);
  },
  part2([player1, player2]) {
    const winner = gameRec(player1, player2);
    return score(winner === 1 ? player1 : player2);
  },
};

export default day22;
