#!/usr/bin/env python

def sorted_dict_values(adict):
    items = adict.items()
    items.sort()
    return [(key, value) for key, value in items]

def ziplists(ls):
    if len(ls) == 0:
        return []
    minlen = len(ls[0])
    for l in ls:
        minlen = min(minlen, len(l))

    ll = []
    for i in range(minlen):
        for l in ls:
            ll.append(l[i])
    return ll

def switch_tuple(t):
    t1, t2 = t
    return t2, t1

class Color:
    def __init__(self, r, g, b):
        self.red = r
        self.green = g
        self.blue = b
    def __str__(self):
        return '%d %d %d' % (self.red, self.green, self.blue)

class Human:
    def __init__(self, first_name, last_name):
        self.name = first_name + " " + last_name
    def __init__(self, name):
        self.name = name

class Date:
    def __init__(self, y, m, d):
        self.year = y
        self.month = m
        self.day = d
