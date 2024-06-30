import { readNumbers, type Day } from "./common";

type Window = {
  count: number;
  numbers: Map<number, number>;
};

function addToWindow(window: Window, x: number) {
  window.count++;
  window.numbers.set(x, (window.numbers.get(x) ?? 0) + 1);
}

function isSumOfTwp(window: Window, target: number) {
  for (const [n, cnt] of window.numbers.entries()) {
    const remaining = target - n;
    if ((remaining === n && cnt > 1) || window.numbers.has(remaining)) {
      return true;
    }
  }
  return false;
}

function removeFromWindow(window: Window, x: number) {
  const current = window.numbers.get(x);
  if (!current) {
    throw new Error(`${x} is not present in the sliding window`);
  }

  window.count--;

  if (current === 1) {
    window.numbers.delete(x);
  } else {
    window.numbers.set(x, current - 1);
  }
}

function findInvalidNumber(
  slidingWindowSize: number,
  numbers: number[]
): number {
  const window: Window = {
    count: 0,
    numbers: new Map(),
  };

  for (let i = 0; i < numbers.length; i++) {
    if (window.count === slidingWindowSize && !isSumOfTwp(window, numbers[i])) {
      return numbers[i];
    }

    addToWindow(window, numbers[i]);
    if (window.count > slidingWindowSize) {
      removeFromWindow(window, numbers[i - slidingWindowSize]);
    }
  }

  throw new Error("Invalid number not found");
}

function findSliceSumUp(nums: number[], target: number): [number, number] {
  const window = {
    start: 0,
    end: -1,
    sum: 0,
    length() {
      return this.end - this.start + 1;
    },
  };

  let next = 0;

  while (next < nums.length || window.sum < target) {
    if (window.sum === target && window.length() > 1) {
      return [window.start, window.end];
    }

    if (window.sum < target) {
      window.end++;
      window.sum += nums[next];
      next++;
    } else {
      window.sum -= nums[window.start];
      window.start++;
    }
  }

  throw new Error("Set of numbers not found");
}

const size = 25;

const day9: Day<number[]> = {
  parseInput: readNumbers,
  part1: (nums) => findInvalidNumber(size, nums),
  part2(numbers) {
    const invalid = findInvalidNumber(size, numbers);
    const [first, last] = findSliceSumUp(numbers, invalid);
    const slice = numbers.slice(first, last + 1);
    return Math.min(...slice) + Math.max(...slice);
  },
};

export default day9;
