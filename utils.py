

def str_to_days(s: str) -> int:
    if s == 'day':   return 1
    if s == 'month': return 30
    if s == 'year':  return 365
    raise ValueError('invalid time period')

