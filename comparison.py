def solve9():
  n = 1000
  for a in range(1, n):
    for b in range(a, n + 1):
      c = n - a - b
      if a * a + b * b == c * c:
        print(a, '*', b, '*', c, '=', a * b * c)

def solve22():
  text = None
  with open('names.txt', 'r') as f:
    text = f.read()
  names = sorted(n.strip('"').upper() for n in text.split(','))

  def score_char(c):
    return ord(c) - ord('A') + 1

  def score_name(n):
    return sum(score_char(c) for c in n)

  print(sum((i + 1) * score_name(n) for i, n in enumerate(names)))

solve9()
solve22()
