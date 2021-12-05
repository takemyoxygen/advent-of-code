import unittest


class Reader:
    def __init__(self, input: str):
        self.input = input
        self.position = 0

    def peek(self) -> str:
        return self.input[self.position]

    def pop(self) -> str:
        char = self.peek()
        self.position += 1
        return char

    def __bool__(self):
        return self.position < len(self.input)


def pop_expected(reader: Reader, expected: str) -> str:
    actual = reader.pop()
    if actual != expected:
        raise Exception("Expected", expected, "got", actual)
    return actual


def consume_until(reader: Reader, until_char: str) -> int:
    consumed = 0
    while reader and reader.peek() != until_char:
        next_char = reader.pop()
        if next_char == '!':
            reader.pop()
        else:
            consumed += 1
    return consumed


def read_garbage(reader: Reader) -> int:
    pop_expected(reader, '<')
    consumed_garbage = consume_until(reader, '>')
    pop_expected(reader, '>')
    return consumed_garbage


def read_group(reader: Reader, score: int = 1) -> tuple[int, int]:
    pop_expected(reader, "{")
    total_score = score
    total_garbage = 0

    while reader.peek() != '}':
        next_char = reader.peek()
        if next_char == '<':
            garbage = read_garbage(reader)
            total_garbage += garbage
        elif next_char == '{':
            group_score, group_garbage = read_group(reader, score + 1)
            total_score += group_score
            total_garbage += group_garbage
        elif next_char == ',':
            reader.pop()

    pop_expected(reader, '}')

    return total_score, total_garbage


score_cases = [
    ('{}', 1),
    ('{{{}}}', 6),
    ('{{},{}}', 5),
    ('{{{},{},{{}}}}', 16),
    ('{<a>,<a>,<a>,<a>}', 1),
    ('{{<ab>},{<ab>},{<ab>},{<ab>}}', 9),
    ('{{<!!>},{<!!>},{<!!>},{<!!>}}', 9),
    ('{{<a!>},{<a!>},{<a!>},{<ab>}}', 3)
]

garbage_cases = [
    ('<>', 0),
    ('<random characters>', 17),
    ('<<<<>', 3),
    ('<{!>}>', 2),
    ('<!!>', 0),
    ('<!!!>>', 0),
    ('<{o"i!a,<{i<a>', 10),
]


class Tests(unittest.TestCase):
    def test_score(self):
        for input, expected in score_cases:
            reader = Reader(input)
            actual, _ = read_group(reader)
            self.assertEqual(actual, expected, msg=input)

    def test_garbage(self):
        for input, expected in garbage_cases:
            reader = Reader(input)
            actual = read_garbage(reader)
            self.assertEqual(actual, expected, msg=input)


input = open('input/day9.txt').read()
reader = Reader(input)
print(read_group(reader))