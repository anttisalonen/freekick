#!/usr/bin/env python

import Primitives

class player_position_type:
    Goalkeeper = 1
    Defender = 2
    Midfielder = 3
    Forward = 4

def num_to_pos(num):
    if num == 1:
        return player_position_type.Goalkeeper
    elif num == 2:
        return player_position_type.Defender
    elif num == 3:
        return player_position_type.Midfielder
    else:
        return player_position_type.Forward

class player_position:
    def __init__(self, pos, left = False, winger = False):
        self.pos = pos
        self.left = left
        self.winger = winger

class player_skills:
    pass

class player_personality:
    pass

class Player(Primitives.Human):
    def __init__(self, pid, name = ""):
        Primitives.Human.__init__(self, name)
        self.id = pid
        self.club_name = ""
    def __str__(self):
        return self.name

class Coach(Primitives.Human):
    pass

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
    def __str__(self):
        return '%s; %s; %s' % (self.jersey_color, self.shorts_color, self.socks_color)

class Club:
    def __init__(self, name):
        self.name = name
        self.kits = []
        self.contracts = []
        self.players = []
        self.org_name = ""
        self.stadium = ""
    def __str__(self):
        return '%s\n%s\n%s\n%s\n%s' % (self.name, self.kits[0], self.contracts, self.org_name, self.stadium)

class Stadium:
    def __init__(self, name, capacity = 0):
        self.name = name
        self.capacity = capacity

class Region:
    def __init__(self, name):
        self.name = name
        self.subregions = []
        self.stadiums = []

class Country:
    def __init__(self, name):
        self.name = name
        self.tournaments = []
    def get_stages(self):
        ret = []
        for l in self.leaguesystem.levels:
            for b in l.branches:
                for s in b.stages:
                    ret.append(s)
        return ret

class StageType:
    League = 0
    Cup = 1

class StageSetup:
    def __init__(self):
        self.seeded = False
        self.rounds = 1

class LeagueSetup(StageSetup):
    def __init__(self):
        StageSetup.__init__(self)
        self.pointsperwin = 3
        self.groups = 1

class CupSetup(StageSetup):
    def __init__(self):
        StageSetup.__init__(self)
        self.extratime = 1
        self.replays = 0
        self.awaygoals = 0
        self.penalties = 1

class Branch:
    def __init__(self, stages = []):
        self.stages = stages

class Stage:
    def __init__(self, name, type):
        self.name = name
        self.type = type
        self.clubs = []
        self.promotions = []
        self.relegations = []
        if self.type == StageType.League:
            self.setup = LeagueSetup()
        else:
            self.setup = CupSetup()
    def get_clubs(self):
        return self.clubs

class Tournament:
    def __init__(self, name, stages = []):
        self.name = name
        self.stages = stages

class Exchange:
    def __init__(self, highest = 0, num = 0):
        self.highest = highest
        self.num = num

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

class Preset:
    def __init__(self, name):
        self.name = name

class DB:
    pass

if __name__ == '__main__':
    pf = player_factory()
    p1 = pf.new_player("John", "Smith")
    p2 = pf.new_player("Eric", "Doe")
    print p1
    print p2
