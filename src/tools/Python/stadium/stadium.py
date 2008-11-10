#!/usr/bin/python

import sys
import re
import types
import random

import lxml
from lxml import etree

from copy import deepcopy

def usage():
    print "Usage: %s countryfile regionname valuefile clubfile1 [clubfile2...]" % sys.argv[0]
    print "          countryfile:   XML file of the country"
    print "          regionname:    stadium region"
    print "          valuefile:     values of clubs"
    print "          clubfile[s]:   files with the data of the clubs"
    print
    print "Countryfile will be changed. Club output will be to standard output."

def main():
    try:
        countryfilename = sys.argv[1]
        regionname = sys.argv[2]
        valuefilename = sys.argv[3]
        clubfilenames = sys.argv[4:]
        
    except IndexError:
        usage()
        sys.exit(1)

    try:
        countryfile = open(countryfilename, 'r')
        countryroot = etree.parse(countryfile)
        countryfile.close()
    except IOError:
        print "could not open", countryfilename
        sys.exit(1)

    regions = countryroot.findall(".//region")
    countrynode = countryroot.find(".//country")
    countryname = countrynode.get("name")
    ourregion = None
    for element in regions:
        if element.get("name") == regionname:
            ourregion = element
            break
    if type(ourregion) == types.NoneType:
        # Region not found; creating one
        ourregion = etree.Element("region", name=regionname)
        regions.append(ourregion)
        stadiums = []
    stadiums = ourregion.findall(".//stadium")
    countrychanged = False
    dooutput = True

    valuetable = {}
    try:
        valuefile = open(valuefilename, 'r')
    except IOError:
        print "could not open %s" % valuefilename
        sys.exit(1)
    for line in valuefile.readlines():
        info = line.split('\t', 1)
        valuetable[info[0].strip()] = int(info[1].strip())
    valuefile.close()

    clubsroot = etree.Element("Clubs")
    for element in clubfilenames:
        try:
            clubfile = open(element, 'r')
            clublines = clubfile.read()
            clubfile.close()
        except IOError:
            print "could not open %s" % clubfilenames[0]
            sys.exit(1)

        clubname = re.compile(r'^ *([a-zA-Z 0-9-\'&]*)$', re.M).search(clublines, 1)
        stadiumname = re.compile(r'^ *Ground \(ground history\) *([a-zA-Z 0-9-\'&]*?) *$', re.M).search(clublines, 1)
        if type(stadiumname) != types.NoneType:
            stadname = stadiumname.groups()[0]
        else:
            stadname = clubname.groups()[0] + " Stadium"

        stadiumnode = etree.Element("stadium", name=stadname)
        try:
            thisvalue = valuetable[clubname.groups()[0]]
        except:
            if dooutput == True:
                print "Could not find team %s in the values file" % clubname.groups()[0]
                print "File that was being processed: %s" % element
                print "No changes will be made."
                dooutput = False
            else:
                print "%s - %s" % (clubname.groups()[0], element)

        stadfound = False
        for element in stadiums:
            if element.get("name") == stadname:
                stadfound = True
                break

        if stadfound == False:
            countrystadiumnode = deepcopy(stadiumnode)
            stadiumcapacity = int(thisvalue**(2.1)/25)/100*100
            capnode = etree.Element("capacity", value="%d" % stadiumcapacity)
            countrystadiumnode.append(capnode)
            ourregion.append(countrystadiumnode)
            stadiums.append(countrystadiumnode)
            countrychanged = True

        clubnode = etree.Element("club", name=clubname.groups()[0])

        kit1node = etree.Element("kit")
        jerseynode = etree.Element("jersey")
        jerseynode.set("type", "0")
        shortsnode = etree.Element("shorts")
        socksnode = etree.Element("socks")
        colornode = etree.Element("color")
        colornode.set("r", "255")
        colornode.set("g", "255")
        colornode.set("b", "255")
        imagenode = etree.Element("image", value="")
        jerseynode.append(deepcopy(colornode))
        jerseynode.append(imagenode)
        shortsnode.append(deepcopy(colornode))
        socksnode.append(colornode)
        kit1node.append(jerseynode)
        kit1node.append(shortsnode)
        kit1node.append(socksnode)
        kitsnode = etree.Element("kits")
        kitsnode.append(deepcopy(kit1node))
        kitsnode.append(deepcopy(kit1node))

        clubnode.append(kitsnode)

        clcountrynode = etree.Element("country", name=countryname)
        clregionnode = etree.Element("region", name=regionname)
        clubnode.append(clcountrynode)
        clubnode.append(clregionnode)
        clubnode.append(stadiumnode)
        clubsroot.append(clubnode)

    if dooutput == True:
        print (etree.tostring(clubsroot, pretty_print=True, encoding="UTF-8"))

        if countrychanged:
            parser = etree.XMLParser(remove_blank_text=True)
            countrynew = etree.fromstring(etree.tostring(countryroot), parser)
            countrystring = etree.tostring(countrynew, pretty_print=True, encoding="UTF-8")
            countryfile = open(countryfilename, 'w')
            countryfile.write(countrystring)
            countryfile.close()

if __name__ == '__main__':
    main()
