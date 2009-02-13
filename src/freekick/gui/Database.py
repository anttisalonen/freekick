#!/usr/bin/env python

import sys
import os
import glob
from lxml import etree

import Primitives
import SoccerData

def get_db(path):
    db = SoccerData.DB()
    db.clubs = {}
    db.players = {}
    xml_list = glob.glob(os.path.join(path, '*.xml'))
    for fn in xml_list:
        tree = etree.parse(fn)
        root = tree.getroot()
        if root.tag == "Clubs":
            print "Parsing clubs in %s" % fn
            db.clubs.update(get_clubs(root))
        elif root.tag == "Players":
            print "Parsing players in %s" % fn
            db.players.update(get_players(root))
        else:
            print "Don't know how to parse %s" % fn
    print "%d clubs parsed" % len(db.clubs)
    print "%d players parsed" % len(db.players)
    return db

def get_players(root):
    players = {}
    for player in root:
        pid, pl = parse_player(player)
        players[pid] = pl
    return players

def parse_player(plnode):
    pid = int(plnode.get("id"))
    pl = SoccerData.Player(pid)
    for node in plnode.iter():
        if node.tag == "personal":
            pl.name = node.get("name")
            for prnode in node.iter():
                if prnode.tag == "birth":
                    pl.birthdate = Primitives.Date(int(prnode.get("year")), int(prnode.get("month")), int(prnode.get("day")))
                elif prnode.tag == "skin":
                    pl.skin_color = parse_color(prnode)
                elif prnode.tag == "hair":
                    pl.hair_color = parse_color(prnode)
                elif prnode.tag == "eyes":
                    pl.eyes_color = parse_color(prnode)
                elif prnode.tag == "height":
                    pl.height = prnode.get("value")
                elif prnode.tag == "nationality":
                    pl.nationality = prnode.get("value")
                elif prnode.tag == "appearance":
                    pl.appearance = int(prnode.get("value"))
        elif node.tag == "personality":
            pl.personality = parse_player_personality(node)
        elif node.tag == "skills":
            pl.skills = parse_player_skills(node)
        elif node.tag == "position":
            pl.position = parse_player_position(node)
            pass
        elif node.tag == "club":
            pl.club_name = node.get("name")
            pass
    return pid, pl

def parse_player_position(node):
    return SoccerData.player_position(SoccerData.num_to_pos(int(node.get("pos"))), node.get("left") != "0", node.get("wing") != "0")

def parse_player_personality(node):
    p = SoccerData.player_personality()
    for n in ["active", "risktaking", "offensive", "aggressive", "consistent", "creative", "experienced"]:
        setattr(p, n, node.get(n))
    return p

def parse_player_skills(node):
    p = SoccerData.player_skills()
    for n in ["stamina", "dexterity", "speed", "tackling", "passing", "shooting", "control", "accuracy", "goalkeeping", "heading"]:
        setattr(p, n, node.get(n))
    return p

def get_clubs(root):
    clubs = {}
    for club in root:
        n, c = parse_club(club)
        clubs[n] = c
    return clubs

def parse_club(clubnode):
    name = clubnode.get("name")
    club = SoccerData.Club(name)
    for node in clubnode.iter():
        if node.tag == "coach":
            club.coach = SoccerData.Coach(node.get("name"))
        if node.tag == "kits":
            club.kits = parse_kits(node)
        elif node.tag == "country":
            club.org_name = node.get("name")
        elif node.tag == "region":
            club.org_name = node.get("name")
        elif node.tag == "stadium":
            club.stadium = node.get("name")
        elif node.tag == "contracts":
            club.contracts = parse_contracts(node)
    return name, club

def parse_contracts(contractsnode):
    cs = []
    for node in contractsnode.iter():
        if node.tag == "contract":
            cs.append(int(node.get("player")))
    return cs

def parse_kits(kitsnode):
    kits = []
    for node in kitsnode.iter():
        if node.tag == "kit":
            kits.append(parse_kit(node))
    return kits

def parse_kit(kitnode):
    kit = SoccerData.Kit()
    for node in kitnode.iter():
        if node.tag == "jersey":
            kit.jersey_type = int(node.get("type"))
            for unode in node.iter():
                if unode.tag == "color":
                    kit.jersey_color = parse_color(unode)
                elif unode.tag == "image":
                    kit.jersey_image = unode.get("value")
        elif node.tag == "shorts":
            kit.shorts_color = parse_color(node[0])
        elif node.tag == "socks":
            kit.socks_color = parse_color(node[0])
    return kit

def parse_color(n):
    return Primitives.Color(int(n.get("r")), int(n.get("g")), int(n.get("b")))

def print_usage():
    print "Usage: %s path_to_club_xml" % sys.argv[0]

if __name__ == '__main__':
    if len(sys.argv) < 2:
        print_usage()
        sys.exit(1)
    db = get_db(sys.argv[1])
    print "%d clubs parsed" % len(db.clubs)
    print "%d players parsed" % len(db.players)

"""
    for pl in db.players:
        print pl
"""
