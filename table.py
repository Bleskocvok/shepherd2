
from typing import List, Any


def create_table(header: List[str], data: List[tuple]) -> str:

    sizes: List[int] = [len(h) for h in header]

    for row in data:

        if len(row) != len(sizes):
            raise RuntimeError('table is misaligned')

        for i, size in enumerate(sizes):
            sizes[i] = max(size, len(str(row[i])))

    delim:  str = ' │ '
    hdelim: str = '═╪═'
    hline:  str = '═'

    res: str = ''

    for i, heading in enumerate(header):
        if i != 0:
            res += delim
        res += heading.upper().ljust(sizes[i])

    res += '\n'

    for i, size in enumerate(sizes):
        if i != 0:
            res += hdelim
        res += hline * size

    res += '\n'

    for row in data:
        for i, val in enumerate(row):
            if i != 0:
                res += delim
            if type(val) is int:
                res += str(val).rjust(sizes[i])
            else:
                res += str(val).ljust(sizes[i])
        res += '\n'

    return res


