#!/usr/bin/env python

import datetime
from lxml import etree

import Primitives
import Match
import Tactics

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

def to_stars(skill):
    num_stars = (skill + 100) / 200
    plus = skill % 200 < 100 and num_stars < 5
    s = "*" * num_stars
    if plus:
        s += "+"
    return s

class Player(Primitives.Human):
    def __init__(self, pid, name = ""):
        Primitives.Human.__init__(self, name)
        self.id = pid
        self.club_name = ""
        self.rating = -1

    def __str__(self):
        return "%-30s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s" % (self.name,
                to_stars(self.skills.stamina),
                to_stars(self.skills.speed),
                to_stars(self.skills.dexterity),
                to_stars(self.skills.goalkeeping),
                to_stars(self.skills.tackling),
                to_stars(self.skills.passing),
                to_stars(self.skills.control),
                to_stars(self.skills.heading),
                to_stars(self.skills.shooting),
                to_stars(self.skills.accuracy))

    def str2(self):
        return "%-30s %-5d %-5d %-5d %-5d %-5d %-5d %-5d %-5d %-5d %-5d" % (self.name,
                self.skills.stamina,
                self.skills.speed,
                self.skills.dexterity,
                self.skills.goalkeeping,
                self.skills.tackling,
                self.skills.passing,
                self.skills.control,
                self.skills.heading,
                self.skills.shooting,
                self.skills.accuracy)

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

class Club:
    def __init__(self, name):
        self.name = name
        self.kits = []
        self.contracts = []
        self.players = {}
        self.org_name = ""
        self.stadium = ""
        self.rating = -1
        self.player_ratings = []

    def __str__(self):
        retval = ""
        retval += "%s - %s - %s\n" % (self.name, self.org_name, self.stadium)
        retval += "%-30s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s %-5s\n" % ("Name",
                "Stamn", "Speed", "Dextr", "Goalk", "Tackl", "Passn", "Contr",
                "Headn", "Shoot", "Accur")
        for p in self.players.values():
            retval += "%s\n" % str(p)
        # retval += self.formation.__str__()
        return retval

    def get_players(self, dbase):
        plsdb = dbase.players
        for contract in self.contracts:
            self.players[contract] = plsdb[contract]
        self.setup_formation(dbase.pitch_tactics)

    def setup_formation(self, pitch_tactics):
        """Setups the formation based on given players.

        This simply takes the best players the club has and assigns them
        appropriately. A better way (TODO) would be to also consider the
        areas on the pitch where players are needed, even if the best players
        aren't available on those areas.
        """
        pltacs = []
        if not self.player_ratings:
            self.setup_player_ratings()
        gk = []
        dfc = []
        dfw = []
        mdc = []
        mdw = []
        fw = []
        subs = []
        for v, p in self.player_ratings:
            tot_pls = len(dfc) + len(mdc) + len(fw)
            if len(gk) < 1 and p.position.pos == player_position_type.Goalkeeper:
                gk.append(p)
            elif tot_pls >= 10:
                subs.append(p)
            elif len(dfc) < 6 and p.position.pos == player_position_type.Defender:
                dfc.append(p)
            elif len(mdc) < 5 and p.position.pos == player_position_type.Midfielder:
                mdc.append(p)
            elif len(mdw) < 4 and p.position.pos == player_position_type.Forward:
                fw.append(p)
            else:
                subs.append(p)
        # print "tot pls: %d" % tot_pls
        self.subs_to_pos(subs, dfc, player_position_type.Defender, 3)
        self.subs_to_pos(subs, mdc, player_position_type.Midfielder, 2)
        self.subs_to_pos(subs, fw, player_position_type.Forward, 1)
        while len(dfc) + len(mdc) + len(fw) > 10:
            if len(dfc) > 3: # just take the first position that fits
                subs.append(dfc.pop())
            elif len(mdc) > 2:
                subs.append(mdc.pop())
            elif len(fw) > 1:
                subs.append(fw.pop())
        form_name = "%d-%d-%d" % (len(dfc), len(mdc), len(fw))
        gentac = Tactics.GeneralTactic()
        pitch_tactic = pitch_tactics[form_name]
        self.formation = Tactics.Formation(gentac, pitch_tactic)
        for player in gk + dfc + mdc + fw:
            self.formation.lineup.add_player(player.id, True)
        # self.pos_to_wing(dfc, dfw)
        # self.pos_to_wing(mdc, mdw)
        # self.formation.setup(gk, dfc, dfw, mdc, mdw, fw, subs)

    def pos_to_wing(self, pos, wings):
        if len(pos) < 3:
            return
        for p in pos:
            if p.position.winger:
                wings.append(p)
                pos.remove(p)
                if len(wings) >= 2:
                    return
        while len(wings) < 2 and len(pos) > 0:
            wings.append(pos.pop(0))

    def subs_to_pos(self, subs, pos, postype, min):
        if len(pos) < min:
            for p in subs:
                if p.position.pos == postype:
                    pos.append(p)
                    subs.remove(p)
                    if len(pos) == min:
                        break
            if len(pos) < min:
                raise ValueError("%s: Not enough players for position %d!" %
                        (self.name, postype))

    def setup_player_ratings(self):
        self.player_ratings = []
        for p in self.players.values():
            val = p.get_rating()
            self.player_ratings.append((val, p))
        self.player_ratings.sort()
        self.player_ratings.reverse()

    def calculate_rating(self):
        if not self.player_ratings:
            self.setup_player_ratings()
        self.rating = 0
        chosen = self.player_ratings[:12]
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
        self.pitch_tactics = {}
        self.clubs["unknown"] = Club("unknown")

if __name__ == '__main__':
    pf = player_factory()
    p1 = pf.new_player("John", "Smith")
    p2 = pf.new_player("Eric", "Doe")
    print p1
    print p2
