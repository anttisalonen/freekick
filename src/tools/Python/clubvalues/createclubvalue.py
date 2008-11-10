#!/usr/bin/python

import sys
import re
import types

def usage():
    print "Usage: %s filename clubvaluemax clubvaluemin" % sys.argv[0]
    print "          filename: league table you want to process"
    print "          clubvalues: max/min values of the top/bottom clubs"
    print
    print "Output will be to standard output."

def main():
    try:
        filename = sys.argv[1]
        clubmax = int(sys.argv[2])
        clubmin = int(sys.argv[3])
    except IndexError:
        usage()
        sys.exit(1)

    leaguepattern = re.compile(r'^ *Latest ([ a-zA-Z]*?) *Table')
    teampattern = re.compile(r'^ *([ a-zA-Z]*?) \d')
    teamlist = []
    leaguedef = ""
    try:
        f = open(filename, 'r')
        flines = f.readlines()
        for line in flines:
            leaguename = leaguepattern.search(line)
            if type(leaguename) != types.NoneType:
                if len(leaguename.groups()) > 0:
                    leaguedef = leaguename.groups()[0]

            teamname = teampattern.search(line)
            if type(teamname) != types.NoneType:
                if len(teamname.groups()) > 0:
                    a = 5
                    teamlist.append(teamname.groups()[0])
        f.close()
    except IOError:
        print "file %s not found" % filename
        sys.exit(1)                                    

    numteams = len(teamlist)
    if numteams > 0:
        pointstep = (clubmax - clubmin) / numteams
        for i in range(numteams):
            teamvalue = clubmax - i * pointstep
            print teamlist[i] + "\t   ", teamvalue
        
if __name__ == '__main__':
    main()
