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
    db.countries = {}
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
        elif root.tag == "Countries":
            print "Parsing countries in %s" % fn
            db.countries.update(get_countries(root))
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
        elif node.tag == "club":
            pl.club_name = node.get("name")
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

def get_countries(root):
    countries = {}
    for country in root:
        n, c = parse_country(country)
        countries[n] = c
    return countries

def parse_country(countrynode):
    name = countrynode.get("name")
    country = SoccerData.Country(name)
    for node in countrynode.iter():
        if node.tag == "Regions":
            country.regions = parse_regions(node)
        elif node.tag == "leaguesystem":
            country.leaguesystem = parse_leaguesystem(node)
        elif node.tag == "Tournaments":
            country.tournaments.append(parse_tournaments(node))
    return name, country

def parse_regions(regnode):
    regs = []
    for node in regnode.iter():
        if node.tag == "Region":
            regs.append(parse_region(node))
    return regs

def parse_region(regnode):
    name = regnode.get("name")
    region = SoccerData.Region(name)
    for node in regnode.iter():
        if node.tag == "Region" and node.get("name") != name:
            region.subregions.append(parse_region(node))
        elif node.tag == "stadium":
            region.stadiums.append(parse_stadium(node))
    return region

def parse_stadium(stadnode):
    return SoccerData.Stadium(stadnode.get("name"), stadnode.get("capacity"))

def parse_leaguesystem(lsnode):
    name = lsnode.get("name")
    ls = SoccerData.Leaguesystem(name)
    for node in lsnode.iter():
        if node.tag == "Levels":
            ls.levels = parse_levels(node)
    return ls

def parse_levels(levels_node):
    return parse_list(levels_node, "level", parse_level)

def parse_exchange(node):
    exchange = SoccerData.Exchange()
    for a in ["num", "tournament", "stage"]:
        val = node.get(a)
        if val != None:
            setattr(exchange, a, val)
    return exchange

def parse_level(level_node):
    l = SoccerData.Level()
    l.branches = []
    for node in level_node.iter():
        if node.tag == "branch":
            l.branches.append(parse_branch(node))
        elif node.tag == "leagueprs":
            for prnode in node.iter():
                if prnode.tag == "leaguepr":
                    exchange = parse_exchange(prnode)
                    l.promotions.append(exchange)
        elif node.tag == "leaguerls":
            for prnode in node.iter():
                if prnode.tag == "leaguerl":
                    exchange = parse_exchange(prnode)
                    l.relegations.append(exchange)
    return l

def parse_branch(branch_node):
    stages = parse_list(branch_node, "stage", parse_stage)
    return SoccerData.Branch(stages)

def parse_stage(stage_node):
    name = stage_node.get("name")
    type = stage_node.get("type")
    stage = SoccerData.Stage(name, type)
    for node in stage_node.iter():
        if node.tag == "setup":
            for n in ["seeded", "participantnum", "groups", "rounds", "pointsperwin", "extratime", "penalties", "replays", "awaygoals"]:
                a = node.get(n)
                if a != None:
                    setattr(stage.setup, n, a)
        elif node.tag == "trophy":
            stage.trophy = SoccerData.Trophy(node.get("name"))
        elif node.tag == "attendances":
            stage.attendances = parse_list(node, "attendance", parse_attendance)
        elif node.tag == "cuppr":
            exchange = parse_exchange(node)
            stage.promotions.append(exchange)
        elif node.tag == "preset":
            for clubnode in node.iter():
                clubname = clubnode.get("name")
                if clubname != None and clubname != "None":
                    stage.clubs.append(clubnode.get("name"))
    return stage

def parse_attendance(attendance_node):
    return attendance_node.get("tournament"), attendance_node.get("stage")

def parse_list(top_node, child_node_name, f):
    l = []
    for node in top_node.iter():
        if node.tag == child_node_name:
            l.append(f(node))
    return l

def parse_tournaments(tnode):
    tournaments = []
    for node in tnode.iter():
        if node.tag == "tournament":
            tournaments.append(parse_tournament(node))
    return tournaments

def parse_tournament(tnode):
    tournament = SoccerData.Tournament(tnode.get("name"))
    for node in tnode.iter():
        if node.tag == "stage":
            tournament.stages.append(parse_stage(node))
    return tournament

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
