#!/usr/bin/python

import sys
import re
import types
import random

import lxml
from lxml import etree

def usage():
    print "Usage: %s startid culturefile valuefile clubsfile outputfile plfile1 [plfile2 ...]" % sys.argv[0]
    print "          startid:      first ID to assign to a player"
    print "          culturefile:  XML file where from to load cultures"
    print "          valuefile:    reference table for the players' skills"
    print "          clubsfile:    XML file with the clubs inside"
    print "          outputfile:   XML file where the players will be saved"
    print "          plfile[s]:    files with the personal data of the players"
    print
    print "Output will be to outputfile. Culture file and clubs file will be changed."

def getColorFromNode(n):
    orig = []
    orig.append(n.get("r"))
    orig.append(n.get("g"))
    orig.append(n.get("b"))
    ret = []
    for i in range(3):
        try:
            ret.append (int(orig[i]))
        except:
            comp = re.compile(r'(\d{1,3})-(\d{1,3})', re.M).search(orig[i], 0)
            ret.append (random.randrange(int(comp.groups()[0]), int(comp.groups()[1])))
    return ret

def processCultureXML(cultures):
    culturalroot = etree.XML(cultures)

    culturetemplates = culturalroot.find("Templates")
    if type(culturetemplates) == types.NoneType:
        print "Invalid culture XML file found (no templates); will update file" % culturefilename
        culturetemplates = etree.Element("Templates")
        culturalroot.append(culturetemplates)

    culturegroups = culturalroot.find("Groups")
    if type(culturegroups) == types.NoneType:
        print "Invalid culture XML file found (no groups); will update file" % culturefilename
        culturegroups = etree.Element("Groups")
        culturalroot.append(culturegroups)

    cultabbrs = []
    print "Following culture templates found: "
    for c in list(culturetemplates):
        print c.get("abbr")
        cultabbrs.append(c.get("abbr"))

    cultgroupnames = []
    print "Following culture groups found: "
    for c in list(culturegroups):
        print c.get("name")
        cultgroupnames.append(c.get("name"))
    return (culturalroot, culturetemplates, culturegroups, cultabbrs, cultgroupnames)

def getphysskill(level, wing, pos):
    physmu = 500 + (level / 3)
    physsigma = 150 - (level / 10)
    retval = 0
    
    while retval <= 0 or retval >= 1000:
        retval = random.gauss(physmu, physsigma)
        if wing == 1:
            retval = retval * 1.1
        if pos == 0:
            retval = retval * 0.9
    return retval

def getgauss(mu, sigma, min, max):
    retval = min
    while retval <= min or retval >= max:
        retval = random.gauss(mu, sigma)
    return retval

def main():
    try:
        startid  = int(sys.argv[1])
        culturefilename = sys.argv[2]
        valuefilename = sys.argv[3]
        clubsfilename = sys.argv[4]
        outfilename = sys.argv[5]
        plfiles  = sys.argv[6:]
    except IndexError:
        usage()
        sys.exit(1)

    try:
        culturefile = open(culturefilename, 'r')
        cultures = culturefile.read()
        culturefile.close()
    except IOError:
        print "could not open %s" % culturefilename
        sys.exit(1)

    culturechanged = False
    (culturalroot, culturetemplates, culturegroups, cultabbrs, culturegroupnames) = processCultureXML(cultures)

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

    try:
        clubsfile = open(clubsfilename, 'r')
        clubsroot = etree.parse(clubsfile)
        clubsfile.close()
    except IOError:
        print "could not open", clubsfilename
        sys.exit(1)

    allclubnodes = clubsroot.findall(".//club")
    dooutput = True
    plid = startid
    root = etree.Element("Players")
    for thisplnum in range(len(plfiles)):
        try:
            plfile = open(plfiles[thisplnum], 'r')
            plfilecontent = plfile.read()
            plfile.close()
        except IOError:
            print "could not open %s" % plfiles[thisplnum]
            sys.exit(0)

#        print "Processing file %s" % plfiles[thisplnum]
        plname = re.compile(r'^ *Real name *([a-zA-Z-\']*) ([a-z-A-Z \']*)$', re.M).search(plfilecontent, 1)
        try:
            firstname = plname.groups()[0]
            lastname = plname.groups()[1]
        except:
            print "Could not read name; skipping"

#        print "Name: %s %s" % (plname.groups()[0], plname.groups()[1])
        plclubname = re.compile(r'^ *Currently playing for *([a-zA-Z \'&-]*)$', re.M).search(plfilecontent, 1)
        if type(plclubname) == types.NoneType:
            print "Player (%s) has no club; skipping player." % plfiles[thisplnum]
            continue
#        print plclubname.groups()[0]
        plclub = plclubname.groups()[0]
        if plclub == "Man Utd":
            plclub = "Manchester United"
        if plclub == "Man City":
            plclub = "Manchester City"
        if plclub == "West Ham":
            plclub = "West Ham United"
        if plclub == "Tottenham":
            plclub = "Tottenham Hotspur"
        if plclub == "Newcastle":
            plclub = "Newcastle United"
        if plclub == "Middlesbro":
            plclub = "Middlesbrough"
        if plclub == "Wigan":
            plclub = "Wigan Athletic"
        if plclub == "Bolton":
            plclub = "Bolton Wanderers"
        if plclub == "Derby":
            plclub = "Derby County"
        if plclub == "West Brom":
            plclub = "West Bromwich"
        if plclub == "Stoke":
            plclub = "Stoke City"
        if plclub == "Bristol C":
            plclub = "Bristol County"
        if plclub == "C Palace":
            plclub = "Crystal Palace"
        if plclub == "Wolves":
            plclub = "Wolverhampton"
        if plclub == "Sheff Utd":
            plclub = "Sheffield Utd"
        if plclub == "QPR":
            plclub = "Queen's Park Rangers"
        if plclub == "Preston":
            plclub = "Preston North End"
        if plclub == "Sheff Wed":
            plclub = "Sheffield Wednesday"
        if plclub == "Leicester":
            plclub = "Leicester City"
        if plclub == "Nottm Forest":
            plclub = "Nottingham Forest"
        if plclub == "Leeds":
            plclub = "Leeds United"
        if plclub == "Southend":
            plclub = "Southend United"
        if plclub == "Huddersfield":
            plclub = "Huddersfield Town"
        if plclub == "Tranmere":
            plclub = "Tranmere Rovers"
        if plclub == "Bristol R":
            plclub = "Bristol Rovers"
        if plclub == "Luton":
            plclub = "Luton Town"
        if plclub == "Notts Co":
            plclub = "Notts County"
        if plclub == "Wycombe":
            plclub = "Wycombe Wanderers"
        if plclub == "Grimsby":
            plclub = "Grimsby Town"
        if plclub == "Grays":
            plclub = "Grays Athletic"
        if plclub == "Halifax":
            plclub = "Halifax Town"
        if plclub == "Hereford":
            plclub = "Hereford United"
        if plclub == "Ipswich":
            plclub = "Ipswich Town"
        if plclub == "Kidderminster":
            plclub = "Kidderminster Harriers"
        if plclub == "Lincoln":
            plclub = "Lincoln City"
        if plclub == "Macclesfield":
            plclub = "Macclesfield Town"
        if plclub == "Mansfield":
            plclub = "Mansfield Town"
        if plclub == "Norwich":
            plclub = "Norwich City"
        if plclub == "Oldham":
            plclub = "Oldham Athletic"
        if plclub == "Oxford":
            plclub = "Oxford United"
        if plclub == "Peterborough":
            plclub = "Peterborough United"
        if plclub == "Plymouth":
            plclub = "Plymouth Argyle"
        if plclub == "Rushden":
            plclub = "Rushden & Diamonds"
        if plclub == "Rotherham":
            plclub = "Rotherham United"
        if plclub == "Scunthorpe":
            plclub = "Scunthorpe United"
        if plclub == "Shrewsbury":
            plclub = "Shrewsbury Town"
        if plclub == "Sheffield Utd":
            plclub = "Sheffield United"
        if plclub == "Bradford":
            plclub = "Bradford City"
        if plclub == "Stockport":
            plclub = "Stockport County"
        if plclub == "Stafford":
            plclub = "Stafford Rangers"
        if plclub == "Stevenage":
            plclub = "Stevenage Borough"
        if plclub == "Swansea":
            plclub = "Swansea City"
        if plclub == "Swindon":
            plclub = "Swindon Town"
        if plclub == "Torquay":
            plclub = "Torquay United"
        if plclub == "West Bromwich":
            plclub = "West Bromwich Albion"
        if plclub == "Wolverhampton":
            plclub = "Wolverhampton Wanderers"
        if plclub == "Yeovil":
            plclub = "Yeovil Town"
        if plclub == "York":
            plclub = "York City"
        if plclub == "Birmingham":
            plclub = "Birmingham City"
        if plclub == "Blackburn":
            plclub = "Blackburn Rovers"
        if plclub == "Brighton":
            plclub = "Brighton & Hove Albion"
        if plclub == "Accrington":
            plclub = "Accrington Stanley"
        if plclub == "Burton":
            plclub = "Burton Albion"
        if plclub == "Cambridge":
            plclub = "Cambridge United"
        if plclub == "Cardiff":
            plclub = "Cardiff City"
        if plclub == "Charlton":
            plclub = "Charlton Athletic"
        if plclub == "Cheltenham":
            plclub = "Cheltenham Town"
        if plclub == "Colchester":
            plclub = "Colchester United"
        if plclub == "Carlisle":
            plclub = "Carlisle United"
        if plclub == "Crawley":
            plclub = "Crawley Town"
        if plclub == "Crewe":
            plclub = "Crewe Alexandra"
        if plclub == "Chester":
            plclub = "Chester City"
        if plclub == "Doncaster":
            plclub = "Doncaster Rovers"
        if plclub == "Aldershot":
            plclub = "Aldershot Town"
        if plclub == "Exeter":
            plclub = "Exeter City"
        if plclub == "Farsley":
            plclub = "Farsley Celtic"
        if plclub == "Forest Green":
            plclub = "Forest Green Rovers"

        clubnode = None
        for element in allclubnodes:
            if element.get("name") == plclub:
                clubnode = element
                break
        if type(clubnode) == types.NoneType:
            print "Player (%s) club could not be found in the clubs XML file; skipping" % plfiles[thisplnum]
            continue

        try:
            thisvalue = valuetable[plclub]
        except KeyError:
            print "Value for the club %s could not be found; please enter value" % plclub
            thisvalue = int(raw_input())

        try:
            plbirth = re.compile(r'^ *Date of birth *(\d{2})-(\d{2})-(\d{4})', re.M).search(plfilecontent, 1)
#            print plbirth.groups()
            bdday = plbirth.groups()[0]
            bdmonth = plbirth.groups()[1]
            bdyear = plbirth.groups()[2]
        except:
            print "Could not read birthdate; assuming 1980-01-01"
            bdyear = "1980"
            bdmonth = "1"
            bdday = "1"
        plnat = re.compile(r'^ *Nationality *([a-zA-Z &\'-]*)$', re.M).search(plfilecontent, 1)
#        print plnat.groups()[0]
        culturenode = None
        plnationality = plnat.groups()[0]
        if len(plnationality) <= 0:
#            print "Empty nationality found (%s); will ask nationality " % plfiles[thisplnum]
#            print "Hint: player name is %s %s" % (firstname, lastname)
#            plnationality = raw_input()
            print "Empty nationality found (%s); skipping player" % plfiles[thisplnum]
            continue
        if plnationality in culturegroupnames:
            for c in culturegroups:
                if c.get("name") == plnationality:
                    ctemplatename = c.get("template")
                    for c2 in list(culturetemplates):
                        if c2.get("abbr") == ctemplatename:
                            culturenode = c2
                            break
                    if type(culturenode) == types.NoneType:
                        print "Culture template %s not found while looking for the template for %s!" % (ctemplatename, plnationality)
                        sys.exit(1)
            if type(culturenode) == types.NoneType:
                print "Internal error; culture group found but not there?"
                sys.exit(1)

        else:
            print "Nationality '%s' (%s) not found in cultures" % (plnationality, plfiles[thisplnum])
            cctype = raw_input ("Please input the culture template for %s: " % plnationality)
            if not cctype in cultabbrs:
                print "Culture template not found; I'll ask once more before I quit."
                cctype = raw_input ("Please input the culture template for %s: " % plnationality)
                if not cctype in cultabbrs:
                    print "Culture template not found."
                    print "Please define the culture template first (edit culture XML file manually)."
                    sys.exit(1)
            else:
                for c in list(culturetemplates):
                    if c.get("abbr") == cctype:
                        culturenode = c
                        newgroup = etree.Element("Group")
                        newgroup.set("name", plnationality)
                        newgroup.set("template", c.get("abbr"))
                        culturegroups.append(newgroup)
                        culturegroupnames.append(plnationality)
                        if culturechanged == False:
                            culturechanged = True
                            print "Will update culture file."
                if type(culturenode) == types.NoneType:
                    print "Internal error; node found but not there?"
                    sys.exit(1)

        activity = 0
        while activity <= 0 or activity >= 1000:
            activity = int(random.gauss(500,150))
        risky = 0
        while risky <= 0 or risky >= 1000:
            risky = int(random.gauss(500,150))
        offensive = 0
        while offensive <= 0 or offensive >= 1000:
            offensive = int(random.gauss(500,150))
        consistency = 0
        while consistency <= 0 or consistency >= 1000:
            consistency = int(random.gauss(500,150))
        creativity = 0
        while creativity <= 0 or creativity >= 1000:
            creativity = int(random.gauss(500,150))

        try:
            cardstotal = re.compile(r' *[yY]ellow [cC]ards: *(\d*) *[rR]ed [cC]ards: *(\d*)', re.M).search(plfilecontent, 1)
            cards = int(cardstotal.groups()[0]) + 2 * int(cardstotal.groups()[1])
            if cards < 0:
                cards = 0
            if cards > 15:
                cards = 15
            print "Cards found on the player: %d" % cards
        except:
            cards = int(random.gauss(1, 4))
            if cards < 0:
                cards = 0
            if cards > 15:
                cards = 15
            print "Could not read cards; assume %d." % cards

        aggressive = 0
        while aggressive <= 0 or aggressive >= 1000:
            aggressive = int(random.gauss(cards * 32 + 300, 150))

        years = 2008 - int(bdyear)
        if years < 16:
            years = 16
        if years > 40:
            years = 40
        experience = 0
        expmiddle = (years - 16) * 32 + 100
        while experience <= expmiddle - 100 or experience >= expmiddle + 100:
            experience = int(random.gauss(expmiddle, 50))

        try:
            position = re.compile(r'^ *Position *([a-zA-Z]*)', re.M).search(plfilecontent, 1)
            if position.groups()[0] == "Goalkeeper":
                pos = 0
            elif position.groups()[0] == "Defender":
                pos = 1
            elif position.groups()[0] == "Midfielder":
                pos = 2
            elif position.groups()[0] == "Forward":
                pos = 3
            else:
                print "Player has no position; randomizing."
                pos = random.randrange(1,3)
        except:
            print "Player has no position; skipping player."
            continue

        sideint = random.randrange(0,100)
        if sideint < 20:
            foot = 1          # Left footed
        else:
            foot = 0          # Right footed

        if (pos == 1 or pos == 2) and (random.randrange(0,2) == 1):
            wing = 1
        else:
            wing = 0

        stamina = getphysskill(thisvalue, wing, pos)
        dexterity = getphysskill(thisvalue, wing, pos)
        speed = getphysskill(thisvalue, wing, pos)
        
        if pos == 0: # goalkeeper
            tackling    = getgauss(thisvalue * 0.3, 100, 10, 500)
            passing     = getgauss(thisvalue * 0.7, 100, 10, 900)
            shooting    = getgauss(thisvalue * 0.8, 100, 10, 800)
            control     = getgauss(thisvalue * 0.6, 100, 10, 700)
            accuracy    = getgauss(thisvalue * 0.6, 100, 10, 600)
            goalkeeping = getgauss(thisvalue,       100, 10, 1000)
            heading     = getgauss(thisvalue * 0.3, 100, 10, 500)
        elif pos == 1: # defender
            tackling    = getgauss(thisvalue,       100, 10, 1000)
            passing     = getgauss(thisvalue,       100, 10, 1000)
            shooting    = getgauss(thisvalue * 0.9, 100, 10, 1000)
            control     = getgauss(thisvalue,       100, 10, 1000)
            accuracy    = getgauss(thisvalue,       100, 10, 1000)
            goalkeeping = getgauss(thisvalue * 0.3, 100, 10, 400)
            heading     = getgauss(thisvalue * 0.9, 100, 10, 1000)
        elif pos == 2: # middlefielder
            tackling    = getgauss(thisvalue,       100, 10, 1000)
            passing     = getgauss(thisvalue,       100, 10, 1000)
            shooting    = getgauss(thisvalue,       100, 10, 1000)
            control     = getgauss(thisvalue,       100, 10, 1000)
            accuracy    = getgauss(thisvalue,       100, 10, 1000)
            goalkeeping = getgauss(thisvalue * 0.3, 100, 10, 400)
            heading     = getgauss(thisvalue,       100, 10, 1000)
        elif pos == 3: # forward
            tackling    = getgauss(thisvalue,       100, 10, 1000)
            passing     = getgauss(thisvalue,       100, 10, 1000)
            shooting    = getgauss(thisvalue,       100, 10, 1000)
            control     = getgauss(thisvalue,       100, 10, 1000)
            accuracy    = getgauss(thisvalue,       100, 10, 1000)
            goalkeeping = getgauss(thisvalue * 0.3, 100, 10, 400)
            heading     = getgauss(thisvalue,       100, 10, 1000)
        else:
            print "Internal error; unknown position?"
            sys.exit(1)

        skinnode = culturenode.find("Skins")
        eyesnode = culturenode.find("Eyes")
        hairnode = culturenode.find("Hairs")
        ourskin = skinnode.find("Skin")
        oureyes = eyesnode.find("Eye")
        ourhair = hairnode.find("Hair")
        (skinr, sking, skinb) = getColorFromNode(ourskin)
        (eyesr, eyesg, eyesb) = getColorFromNode(oureyes)
        (hairr, hairg, hairb) = getColorFromNode(ourhair)

        try:
            heightft = re.compile(r'^ *Height *(\d).(\d*)', re.M).search(plfilecontent, 1)
            height = int(float(float(heightft.groups()[0]) * 30.5) + float(float(heightft.groups()[1]) * 2.5))
#            print heightft.groups(), height
        except:
            print "Could not read height; assume 178."
            height = 178

        playernode = etree.Element("player", id="%d" % plid)
        root.append(playernode)
        personalnode = etree.Element("personal")
        personalnode.set("first", firstname)
        personalnode.set("last", lastname)
        playernode.append(personalnode)
        datanode = etree.SubElement(personalnode, "birth")
        datanode.set ("year", bdyear)
        datanode.set ("month", bdmonth)
        datanode.set ("day", bdday)
        plskinnode = etree.SubElement(personalnode, "skin")
        plskinnode.set ("r", "%d" % skinr)
        plskinnode.set ("g", "%d" % sking)
        plskinnode.set ("b", "%d" % skinb)
        plhairnode = etree.SubElement(personalnode, "hair")
        plhairnode.set ("r", "%d" % hairr)
        plhairnode.set ("g", "%d" % hairg)
        plhairnode.set ("b", "%d" % hairb)
        pleyesnode = etree.SubElement(personalnode, "eyes")
        pleyesnode.set ("r", "%d" % eyesr)
        pleyesnode.set ("g", "%d" % eyesg)
        pleyesnode.set ("b", "%d" % eyesb)
        etree.SubElement(personalnode, "height", value="%d" % height)
        etree.SubElement(personalnode, "nationality", value=plnat.groups()[0])

        personalitynode = etree.SubElement(playernode, "personality")
        personalitynode.set ("active", "%d" % activity)
        personalitynode.set ("risktaking", "%d" % risky)
        personalitynode.set ("offensive", "%d" % offensive)
        personalitynode.set ("aggressive", "%d" % aggressive)
        personalitynode.set ("consistent", "%d" % consistency)
        personalitynode.set ("creative", "%d" % creativity)
        personalitynode.set ("experienced", "%d" % experience)
        skillsnode = etree.SubElement(playernode, "skills")
        skillsnode.set ("stamina", "%d" % stamina)
        skillsnode.set ("dexterity", "%d" % dexterity)
        skillsnode.set ("speed", "%d" % speed)
        skillsnode.set ("tackling", "%d" % tackling)
        skillsnode.set ("passing", "%d" % passing)
        skillsnode.set ("shooting", "%d" % shooting)
        skillsnode.set ("control", "%d" % control)
        skillsnode.set ("accuracy", "%d" % accuracy)
        skillsnode.set ("goalkeeping", "%d" % goalkeeping)
        skillsnode.set ("heading", "%d" % heading)
        positionnode = etree.SubElement(playernode, "position")
        positionnode.set ("pos", "%d" % pos)
        positionnode.set ("foot", "%d" % foot)
        positionnode.set ("wing", "%d" % wing)

        plclubnode = etree.SubElement(playernode, "club")
        plclubnode.set ("name", plclub)

        contractsnode = clubnode.find(".//contracts")
        if type(contractsnode) == types.NoneType:
            contractsnode = etree.SubElement(clubnode, "contracts")
        contractnode = etree.SubElement(contractsnode, "contract")
        contractnode.set("player", "%d" % plid)
        print "Added player (%s) to club %s" % (plfiles[thisplnum], plclub)
        plid = plid + 1

    if culturechanged:
        print "Writing new culture file"
        parser = etree.XMLParser(remove_blank_text=True)
        culturenew = etree.fromstring(etree.tostring(culturalroot), parser)
        culturestring = etree.tostring(culturenew, pretty_print=True, encoding="UTF-8")
        culturefile = open(culturefilename, 'w')
        culturefile.write(culturestring)
        culturefile.close()

    if dooutput:
        print "Writing new clubs file"
        parser = etree.XMLParser(remove_blank_text=True)
        clubsnew = etree.fromstring(etree.tostring(clubsroot), parser)
        clubsstring = etree.tostring(clubsnew, pretty_print=True, encoding="UTF-8")
        clubsfile = open(clubsfilename, 'w')
        clubsfile.write(clubsstring)
        clubsfile.close()

        xmlstring = etree.tostring(root, pretty_print=True, encoding="UTF-8")
        outfile = open(outfilename, 'w')
        outfile.write(xmlstring)
        outfile.close()
        print "%d players processed" % len(plfiles)
        sys.exit(0)

if __name__ == '__main__':
    main()
