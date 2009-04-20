#!/usr/bin/env python

import datetime
from lxml import etree

import Primitives
import Match

class player_position_type:
    Goalkeeper = 0
    Defender = 1
    Midfielder = 2
    Forward = 3

def num_to_pos(num):
    if num == 0:
        return player_position_type.Goalkeeper
    elif num == 1:
        return player_position_type.Defender
    elif num == 2:
        return player_position_type.Midfielder
    else:
        return player_position_type.Forward

class player_position:
    def __init__(self, pos, left = False, winger = False):
        self.pos = pos
        self.left = left
        self.winger = winger

    def to_xml(self):
        positionnode = etree.Element("position")
        positionnode.set("pos", str(self.pos))
        positionnode.set("left", str(int(self.left)))
        positionnode.set("wing", str(int(self.winger)))
        return positionnode

    def equals(self, other):
        return (self.pos == other.pos and self.left == other.left and self.winger == other.winger)

player_personalities_list = ["active", "risktaking", "offensive", "aggressive", "consistent", "creative", "experienced"]
player_skills_list = ["stamina", "dexterity", "speed", "tackling", "passing", "shooting", "control", "accuracy", "goalkeeping", "heading"]

class player_skills:
    def to_xml(self):
        root = etree.Element("skills")
        for skill in player_skills_list:
            root.set(skill, str(getattr(self, skill)))
        return root

class player_personality:
    def to_xml(self):
        root = etree.Element("personality")
        for personality in player_personalities_list:
            root.set(personality, str(getattr(self, personality)))
        return root

class Player(Primitives.Human):
    def __init__(self, pid, name = ""):
        Primitives.Human.__init__(self, name)
        self.id = pid
        self.club_name = ""
        self.rating = -1

    def __str__(self):
        return self.name

    def calculate_rating(self):
        self.rating = 0
        if self.position.pos == player_position_type.Goalkeeper:
            self.rating += self.skills.goalkeeping
        elif self.position.pos == player_position_type.Defender:
            self.rating += self.skills.tackling * 2
            self.rating += self.skills.passing
            self.rating += self.skills.control
            self.rating = self.rating / 4
        elif self.position.pos == player_position_type.Midfielder:
            self.rating += self.skills.passing * 2
            self.rating += self.skills.control
            self.rating += self.skills.stamina
            self.rating += self.skills.tackling
            self.rating = self.rating / 5
        elif self.position.pos == player_position_type.Forward:
            self.rating += self.skills.shooting * 2
            self.rating += self.skills.accuracy * 2
            self.rating += self.skills.speed
            self.rating = self.rating / 5

    def get_rating(self):
        if self.rating < 0:
            self.calculate_rating()
        # print "Rating for player %s: %d" % (self.name, self.rating)
        return self.rating

    def to_xml(self):
        playernode = etree.Element("player", id = str(self.id))
        personalnode = etree.SubElement(playernode, "personal", name = self.name)
        appearancenode = etree.SubElement(personalnode, "appearance", value = str(self.appearance))
        nationalitynode = etree.SubElement(personalnode, "nationality", value = self.nationality)
        personalitynode = self.personality.to_xml()
        playernode.append(personalitynode)
        skillsnode = self.skills.to_xml()
        playernode.append(skillsnode)
        positionnode = self.position.to_xml()
        playernode.append(positionnode)
        return playernode

class Coach(Primitives.Human):
    def __init__(self, name):
        Primitives.Human.__init__(self, name)

class player_factory:
    def __init__(self):
        self.pidcounter = 1
    def new_player(self, name):
        p = Player(self.pidcounter, name)
        self.pidcounter += 1
        return p

class Kit:
    def __init__(self):
        self.jersey_type = 0
        self.jersey_image = ""
        self.jersey_colors = []
        self.shorts_color = Primitives.Color()
        self.socks_color = Primitives.Color()

    def __str__(self):
        return '%s; %s; %s' % (self.jersey_colors[0], self.shorts_color, self.socks_color)

    def to_xml(self):
        kitnode = etree.Element("kit")
        jerseynode = etree.SubElement(kitnode, "jersey", type = str(self.jersey_type))
        for jcolor in self.jersey_colors:
            jerseynode.append(jcolor.to_xml())
        shortsnode = etree.SubElement(kitnode, "shorts")
        shortsnode.append(self.shorts_color.to_xml())
        socksnode = etree.SubElement(kitnode, "socks")
        socksnode.append(self.socks_color.to_xml())
        return kitnode

tactic_attributes_list = ["active", "risktaking", "offensive"]

class PlayerTactic:
    def __init__(self, name = "", pos = None, active = 0.5, risktaking = 0.5, offensive = 0.5):
        self.pos = pos
        self.name = ""
        self.active = active
        self.risktaking = risktaking
        self.offensive = offensive
        self.areas = []
        for i in range(4):
            self.areas.append(Primitives.Square(0, 0, 0, 0))

class Lineup:
    def __init__(self):
        self.positions = {}
        self.substitutes = []

    def to_xml(self):
        root = etree.Element("lineup")
        for k, v in self.positions.items():
            etree.SubElement(root, "position", name = k, player = str(v))
        for subid in self.substitutes:
            etree.SubElement(root, "substitute", player = str(subid))
        return root

class Formation:
    def __init__(self, name):
        self.name = name
        self.tactics = []

    def to_xml(self):
        root = etree.Element("formation", name = self.name)
        for tactic in self.tactics:
            tacticnode = etree.SubElement(root, "tactic", name = tactic.name)
            tacticnode.append(tactic.pos.to_xml())
            attribnode = etree.SubElement(tacticnode, "attributes", active = str(tactic.active), risktaking = str(tactic.risktaking), offensive = str(tactic.offensive))
            for i in range(4):
                area = tactic.areas[i]
                if i == 0:
                    areanode = etree.SubElement(tacticnode, "area", offensive = "0", own = "0")
                elif i == 1:
                    areanode = etree.SubElement(tacticnode, "area", offensive = "0", own = "1")
                elif i == 2:
                    areanode = etree.SubElement(tacticnode, "area", offensive = "1", own = "0")
                else:
                    areanode = etree.SubElement(tacticnode, "area", offensive = "1", own = "1")
                areanode.set("min_x", str(area.min_x))
                areanode.set("max_x", str(area.max_x))
                areanode.set("min_y", str(area.min_y))
                areanode.set("max_y", str(area.max_y))
        return root        

class Club:
    def __init__(self, name):
        self.name = name
        self.kits = []
        self.contracts = []
        self.players = {}
        self.lineup = Lineup()
        self.org_name = ""
        self.stadium = ""
        self.rating = -1

    def __str__(self):
        return '%s\n%s\n%s\n%s' % (self.name, self.contracts, self.org_name, self.stadium)

    def get_players(self, plsdb):
        for contract in self.contracts:
            self.players[contract] = plsdb[contract]

    def get_formation(self, fmdb):
        self.formation = fmdb[self.formation_name]
        for tactic in self.formation.tactics:
            found = False
            for plid, player in self.players.items():
                if player.position.equals(tactic.pos):
                    self.lineup.positions[tactic.name] = plid
                    found = True
                    break
            if not found:
                for plid in self.players.keys():
                    if plid not in self.lineup.positions.values():
                        self.lineup.positions[tactic.name] = plid
                        break
        for plid, player in self.players.items():
            if plid not in self.lineup.positions.values():
                self.lineup.substitutes.append(plid)

    def calculate_rating(self):
        self.rating = 0
        pls = []
        min_val = 0
        num_pls = 0
        for p in self.players.values():
            val = p.get_rating()
            pls.append((val, p))
        chosen = sorted(pls)[:12]
        for v, p in chosen:
            self.rating += v

    def get_rating(self):
        if self.rating < 0:
            self.calculate_rating()
        # print "Rating for club %s: %d" % (self.name, self.rating)
        return self.rating

    def to_xml(self):
        root = etree.Element("club", name = self.name)
        coachnode = etree.SubElement(root, "coach", name = self.coach.name)
        kitsnode = etree.SubElement(root, "kits")
        for kit in self.kits:
            kitnode = kit.to_xml()
            kitsnode.append(kitnode)
        countrynode = etree.SubElement(root, "country", name = self.org_name)
        stadiumnode = etree.SubElement(root, "stadium", name = self.stadium)
        contractsnode = etree.SubElement(root, "contracts")
        for contract in self.contracts:
            contractnode = etree.SubElement(contractsnode, "contract", player = str(contract))
        return root

class Pitch:
    def __init__(self, name, friction, length, width, size_length, size_width, pattern = [], state = []):
        self.name = name
        self.size_length = size_length
        self.size_width = size_width
        self.length = length
        self.width = width
        self.pattern = pattern
        self.state = state

    def to_xml(self):
        root = etree.Element("pitch", name = self.name)
        etree.SubElement(root, "area", length = str(self.length), width = str(self.width))
        etree.SubElement(root, "size", length = str(self.size_length), width = str(self.size_width))
        patnode = etree.SubElement(root, "patterns")
        for pattern in self.pattern:
            etree.SubElement(patnode, "pattern", keyword = pattern)
        statenode = etree.SubElement(root, "states")
        for state in self.state:
            etree.SubElement(statenode, "state", keyword = state)
        return root

def default_pitch():
    p = Pitch("grass01", 0.95, 100, 70, 110, 80)
    return p

class Stadium:
    def __init__(self, name, capacity = 0):
        self.name = name
        self.capacity = capacity
        self.pitch = default_pitch()

    def to_xml(self):
        root = etree.Element("stadium", name = self.name, capacity = str(self.capacity), pitch = self.pitch.name)
        return root

class Region:
    def __init__(self, name):
        self.name = name
        self.subregions = {}
        self.stadiums = {}

class Country:
    def __init__(self, name):
        self.name = name
        self.tournaments = {}
    def get_stages(self):
        ret = []
        for l in self.leaguesystem.levels:
            for b in l.branches:
                for s in b.stages:
                    ret.append(s)
        return ret

class Branch:
    def __init__(self, stages, prs, rls):
        self.stages = stages
        self.promotions = prs
        self.relegations = rls

def stage_number_to_stage_name(s, max_stages):
    if s < 1 or max_stages < 1:
        return ""
    if s == 1:
        return "Final"
    elif s == 2:
        return "Semifinals"
    elif s == 3:
        return "Quarterfinals"
    else:
        if s == max_stages:
            return "First round"
        elif s == max_stages - 1:
            return "Second round"
        elif s == max_stages - 2:
            return "Third round"
        elif s == max_stages - 3:
            return "Fourth round"
        elif s == max_stages - 4:
            return "Fifth round"
        elif s == max_stages - 5:
            return "Sixth round"
        elif s == max_stages - 6:
            return "Seventh round"
        elif s == max_stages - 7:
            return "Eighth round"
    return ""

class Exchange:
    def __init__(self, highest = 0, num = 0, tournament = "", stage = ""):
        self.highest = highest
        self.num = num
        self.tournament = tournament
        self.stage = stage

    def __str__(self):
        return "%s %s %d" % (self.tournament, self.stage, self.num)

class Trophy:
    def __init__(self, name):
        self.name = name

class Level:
    def __init__(self):
        self.branches = []
        self.promotions = []
        self.relegations = []

class Leaguesystem:
    def __init__(self, name):
        self.name = name

    def get_higher_level(self, stagename):
        for i in range(len(self.levels)):
            for b in self.levels[i].branches:
                for j in range(len(b.stages)):
                    if stagename == b.stages[i].name:
                        if i == 0:
                            raise ValueError("No higher level")
                        return self.levels[i - 1]
        raise ValueError("Stage not found")

    def get_lower_level(self, stagename):
        for i in range(len(self.levels)):
            for b in self.levels[i].branches:
                for j in range(len(b.stages)):
                    if stagename == b.stages[i].name:
                        if i == len(self.levels) - 1:
                            raise ValueError("No lower levels")
                        return self.levels[i + 1]
        raise ValueError("Stage not found")

class Preset:
    def __init__(self, name):
        self.name = name

class DB:
    def __init__(self):
        self.clubs = {}
        self.players = {}
        self.countries = {}
        self.tournaments = {}
        self.stadiums = {}
        self.formations = {}
        self.pitches = {}
        self.clubs["unknown"] = Club("unknown")

if __name__ == '__main__':
    pf = player_factory()
    p1 = pf.new_player("John", "Smith")
    p2 = pf.new_player("Eric", "Doe")
    print p1
    print p2
