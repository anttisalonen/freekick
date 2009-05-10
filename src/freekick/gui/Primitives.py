#!/usr/bin/env python

from lxml import etree

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

def clamp(v, mn, mx):
    if v < mn:
        return mn
    if v > mx:
        return mx
    return v

def switch_tuple(t):
    t1, t2 = t
    return t2, t1

class Color:
    def __init__(self, r = 0, g = 0, b = 0):
        self.red = r
        self.green = g
        self.blue = b

    def __str__(self):
        return '%d %d %d' % (self.red, self.green, self.blue)

    def to_xml(self):
        return etree.Element("color", r = str(self.red), g = str(self.green), b = str(self.blue))

class Human:
    def __init__(self, first_name, last_name):
        self.name = first_name + " " + last_name

    def __init__(self, name):
        self.name = name

class Square:
    def __init__(self, min_x, max_x, min_y, max_y):
        self.min_x = min_x
        self.max_x = max_x
        self.min_y = min_y
        self.max_y = max_y
