#!/usr/bin/python

import sys

def round_robin(ls):
    clubs = list(ls)
    num_clubs = len(clubs)
    # add dummy club if necessary
    if num_clubs % 2 == 1:
        have_dummy = True
        clubs.append(0)
        num_clubs += 1
    else:
        have_dummy = False

    # take last club as base
    baseclub = clubs[-1]
    clubs = clubs[:-1]

    num_rounds = num_clubs - 1
    half_clubs = num_clubs / 2

    rounds = []
    for r in range(num_rounds):
        homeclubs = []
        awayclubs = []
        homeclubs.append(baseclub)
        
        for i in range(half_clubs + 1):
            homeclubs.append(clubs[i])

        for i in range(num_clubs - 2, half_clubs - 2, -1):
            awayclubs.append(clubs[i])

        if r % 2 == 0:
            rounds.append(zip(homeclubs, awayclubs))
        else:
            rounds.append(zip(awayclubs, homeclubs))
        clubs.append(clubs.pop(0))

    if have_dummy:
        for matches in rounds:
            del matches[0]

    return rounds

if __name__ == "__main__":
    default_num_clubs = 6

    # parse command line
    if len(sys.argv) > 1:
        num_clubs = int(sys.argv[1])
    else:
        num_clubs = default_num_clubs

    # generate clubs
    clubs = range(1, num_clubs + 1)

    rounds = round_robin(clubs)

    print len(rounds)
    for r in rounds:
        print len(r),
        print r
