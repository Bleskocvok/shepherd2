
from typing import List, Any, Optional, Tuple


def create_table(header: List[str],
                 data:   List[tuple],
                 line:   Tuple[str, str] = ('', ''),
                 head:   Tuple[str, str] = ('', '')) -> str:

    sizes: List[int] = [len(h) for h in header]

    for row in data:

        if len(row) != len(sizes):
            raise RuntimeError('table is misaligned')

        for i, size in enumerate(sizes):
            sizes[i] = max(size, len(str(row[i])))

    delim:  str = ' │ '
    hdelim: str = '═╪═'
    hline:  str = '═'

    beg = line[0]
    end = line[1]

    res: str = head[0]

    for i, heading in enumerate(header):
        if i != 0:
            res += delim
        res += heading.upper().ljust(sizes[i])

    res += head[1] + '\n'

    res += beg
    for i, size in enumerate(sizes):
        if i != 0:
            res += hdelim
        res += hline * size
    res += end + '\n'

    for row in data:
        res += beg
        for i, val in enumerate(row):
            if i != 0:
                res += delim
            if type(val) is int:
                res += str(val).rjust(sizes[i])
            else:
                res += str(val).ljust(sizes[i])
        res += end + '\n'

    return res


