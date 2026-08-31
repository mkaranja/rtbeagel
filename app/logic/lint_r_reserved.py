#!/usr/bin/env python3
"""
Lint R sources for reserved words used as list/`$` accessors.

WHY THIS EXISTS
    `next` is a loop-control keyword in R, so `node$next` is a PARSE error -
    the whole module fails to load, before any code runs. Bracket-balance
    checking cannot see this: the parens balance perfectly.

    That is exactly how a broken fct_workflows.R shipped: `n$next` counted
    as balanced, and nothing else looked at it.

Also flags reserved words as data.frame/list construction keys, e.g.
    list(next = 1)      # parse error
    df$repeat           # parse error

Usage:  python3 lint_r_reserved.py FILE [FILE...]
Exit 1 if anything is found.
"""
import re
import sys

# R's reserved words. ?Reserved
RESERVED = [
    'if', 'else', 'repeat', 'while', 'function', 'for', 'next', 'break',
    'TRUE', 'FALSE', 'NULL', 'Inf', 'NaN', 'NA',
    'NA_integer_', 'NA_real_', 'NA_character_', 'in',
]

def strip_strings_and_comments(line: str) -> str:
    """Blank out string literals and trailing comments so we only lint code."""
    out, i, n, quote = [], 0, len(line), None
    while i < n:
        c = line[i]
        if quote:
            if c == '\\':
                out.append(' '); out.append(' '); i += 2; continue
            if c == quote:
                quote = None
            out.append(' '); i += 1; continue
        if c in ('"', "'"):
            quote = c; out.append(' '); i += 1; continue
        if c == '#':
            break
        out.append(c); i += 1
    return ''.join(out)


def lint(path: str):
    problems = []
    for lineno, raw in enumerate(open(path, encoding='utf-8'), 1):
        code = strip_strings_and_comments(raw)
        for w in RESERVED:
            # $reserved  or  @reserved  not already backticked
            for m in re.finditer(r'[$@]\s*(?<![`\w.])' + re.escape(w) + r'\b', code):
                problems.append((lineno, f'${w}',
                                 'reserved word as accessor -> parse error',
                                 raw.rstrip()))
            # list(reserved = ...) / data.frame(reserved = ...)
            for m in re.finditer(r'(?<![`\w.$@])' + re.escape(w) + r'\s*=(?!=)', code):
                if w in ('TRUE', 'FALSE', 'NULL', 'NA', 'Inf', 'NaN'):
                    continue  # legal as VALUES, only illegal as names
                problems.append((lineno, f'{w} =',
                                 'reserved word as argument name -> parse error',
                                 raw.rstrip()))
    return problems


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2
    total = 0
    for path in argv[1:]:
        problems = lint(path)
        if problems:
            print(f'\n{path}')
            for lineno, tok, why, src in problems:
                print(f'  line {lineno:4}  {tok:14} {why}')
                print(f'                  {src.strip()[:76]}')
        total += len(problems)
    if total:
        print(f'\n{total} parse error(s) found.')
        return 1
    print(f'clean: {len(argv) - 1} file(s), no reserved-word accessors')
    return 0


if __name__ == '__main__':
    sys.exit(main(sys.argv))
