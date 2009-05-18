#!/usr/bin/python

from lxml import etree
from SoccerData import PlayerTactic, PitchTactic

def main():
    defensive_lines = []
    midfielder_lines = []
    forward_lines = []
    complete_tactics = []
    for defenders in 3, 4, 5, 6:
        defensive_lines.append(make_defensive_line(defenders))
    for midfielders in 1, 2, 3, 4, 5:
        midfielder_lines.append(make_midfield_line(midfielders))
    for forwards in 1, 2, 3, 4, 5:
        forward_lines.append(make_forward_line(forwards))
    for def_line in defensive_lines:
        for mid_line in midfielder_lines:
            for forward_line in forward_lines:
                if len(def_line) + len(mid_line) + len(forward_line) != 10:
                    continue
                comp_name = "%d-%d-%d" % (len(def_line), len(mid_line), \
                        len(forward_line))
                complete_tactics.append(PitchTactic(comp_name, len(def_line), 
                    len(mid_line), len(forward_line), def_line +
                    mid_line + forward_line))

    for complete_tactic in complete_tactics:
        root = etree.Element("PitchTactics")
        root.append(complete_tactic.to_xml())
        tree = etree.ElementTree(root)
        tree.write(''.join([complete_tactic.name, ".xml"]), pretty_print=True,
                encoding='UTF-8')

def full_back():
    pt = PlayerTactic("full back")
    pt.offensive = 0.6
    pt.pos = 0.15, 0.25
    return pt

def centre_back():
    pt = PlayerTactic("back")
    pt.offensive = 0.2
    pt.pos = 0.3, 0.2
    return pt

def centre_back_middle():
    pt = PlayerTactic("back")
    pt.offensive = 0.2
    pt.pos = 0.5, 0.2
    return pt

def centre_back_support():
    pt = PlayerTactic("supporting back")
    pt.offensive = 0.3
    pt.pos = 0.35, 0.22
    return pt

def centre_midfielder_middle():
    pt = PlayerTactic("midfielder")
    pt.offensive = 0.5
    pt.pos = 0.5, 0.5
    return pt

def centre_midfielder():
    pt = PlayerTactic("midfielder")
    pt.offensive = 0.5
    pt.pos = 0.4, 0.45
    return pt

def wing_midfielder():
    pt = PlayerTactic("winger")
    pt.offensive = 0.5
    pt.pos = 0.2, 0.55
    return pt

def centre_midfielder_support():
    pt = PlayerTactic("supporting midfielder")
    pt.offensive = 0.5
    pt.pos = 0.3, 0.5
    return pt

def centre_forward_middle():
    pt = PlayerTactic("forward")
    pt.offensive = 0.8
    pt.pos = 0.5, 0.75
    return pt

def centre_forward():
    pt = PlayerTactic("forward")
    pt.offensive = 0.8
    pt.pos = 0.35, 0.75
    return pt

def centre_forward_wing():
    pt = PlayerTactic("wing forward")
    pt.offensive = 0.7
    pt.pos = 0.2, 0.7
    return pt

def complete_tactic_name(tactic, central = True, flip = False):
    if central:
        desc = "Central"
    elif flip:
        desc = "Left"
        xp, yp = tactic.pos
        tactic.pos = 1.0 - xp, yp
    else:
        desc = "Right"
    tactic.name = ' '.join([desc, tactic.name])
    return tactic

def make_defensive_line(defenders):
    line = []
    # full backs
    line.append(complete_tactic_name(full_back(), False, False))
    line.append(complete_tactic_name(full_back(), False, True))
    if defenders == 3 or defenders == 5:
        # lone central back
        line.append(complete_tactic_name(centre_back_middle(), True, False))
    if defenders == 4 or defenders == 6:
        # double centre backs
        line.append(complete_tactic_name(centre_back(), False, False))
        line.append(complete_tactic_name(centre_back(), False, True))
    if defenders == 5 or defenders == 6:
        # supporting centre backs
        line.append(complete_tactic_name(centre_back_support(), False, False))
        line.append(complete_tactic_name(centre_back_support(), False, True))
    return line

def make_midfield_line(midfielders):
    line = []
    if midfielders == 1 or midfielders == 3 or midfielders == 5:
        # central midfielder
        line.append(complete_tactic_name(centre_midfielder_middle(), True,
            False))
    if midfielders == 2 or midfielders == 4:
        # two central midfielders
        line.append(complete_tactic_name(centre_midfielder(), False,
            False))
        line.append(complete_tactic_name(centre_midfielder(), False,
            True))
    if midfielders == 3 or midfielders == 4 or midfielders == 5:
        # wingers
        line.append(complete_tactic_name(wing_midfielder(), False,
            False))
        line.append(complete_tactic_name(wing_midfielder(), False,
            True))
    if midfielders == 5:
        # support
        line.append(complete_tactic_name(centre_midfielder_support(), False,
            False))
        line.append(complete_tactic_name(centre_midfielder_support(), False,
            True))
    return line

def make_forward_line(forwards):
    line = []
    if forwards == 1 or forwards == 3 or forwards == 5:
        # central forward
        line.append(complete_tactic_name(centre_forward_middle(),
            True, False))
    if forwards == 2 or forwards == 3 or forwards == 4 or forwards == 5:
        # two central forwards
        line.append(complete_tactic_name(centre_forward(),
            False, False))
        line.append(complete_tactic_name(centre_forward(),
            False, True))
    if forwards == 4 or forwards == 5:
        # two winger forwards
        line.append(complete_tactic_name(centre_forward_wing(),
            False, False))
        line.append(complete_tactic_name(centre_forward_wing(),
            False, True))
    return line

if __name__ == "__main__":
    main()

