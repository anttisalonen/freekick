#!/usr/bin/python

import SoccerData
import Match
import round_robin

class StageType:
    League = 0
    Cup = 1

class StageSetup:
    def __init__(self):
        self.seeded = False
        self.rounds = 1
        self.matchrules = Match.MatchRules()
        self.participantnum = 0

class LeagueSetup(StageSetup):
    def __init__(self):
        StageSetup.__init__(self)
        self.pointsperwin = 3
        self.groups = 1

class CupSetup(StageSetup):
    def __init__(self):
        StageSetup.__init__(self)
        self.matchrules.extratime = True
        self.matchrules.penalties = True

class Tournament:
    def __init__(self, name):
        self.name = name
        self.stages = []
        self.current_stage = 0

    def add_stage(self, stage):
        self.stages.append(stage)
        self.current_stage = len(self.stages) - 1

    def add_clubs(self, clubs):
        for s in self.stages:
            s.feed_club_names(clubs)
            if len(clubs) == 0:
                break

    def play_next_round(self):
        print "Current stage:", self.stages[self.current_stage].name
        winners = self.stages[self.current_stage].play_next_round()
        print "Winners: %s\n" % winners
        self.update_stage(winners)

    def get_next_round(self):
        return self.stages[self.current_stage].get_next_round()

    def round_played(self):
        winners = self.stages[self.current_stage].round_played()
        return self.update_stage(winners)

    def update_stage(self, winners):
        if len(winners) > 0:
            self.current_stage -= 1
            if self.current_stage >= 0:
                self.stages[self.current_stage].update_club_names(winners)
            return True
        return False

    def pretty_print(self):
        for stage in reversed(self.stages):
            print "%s:" % stage.name
            stage.pretty_print()

class LeagueTableRow:
    def __init__(self, name):
        self.name = name
        self.played = 0
        self.won = 0
        self.drawn = 0
        self.lost = 0
        self.points = 0

class LeagueTable:
    def __init__(self):
        self.rows = {}

    def add_match(self, match):
        if match.club1 not in self.rows:
            self.rows[match.club1] = LeagueTableRow(match.club1)
        if match.club2 not in self.rows:
            self.rows[match.club2] = LeagueTableRow(match.club2)

        rt = match.mr.result_type()
        if rt != Match.MatchResultType.NotPlayed:
            self.rows[match.club1].played += 1
            self.rows[match.club2].played += 1
            if rt == Match.MatchResultType.Club1:
                self.rows[match.club1].won += 1
                self.rows[match.club1].points += 3       # TODO
                self.rows[match.club2].lost += 1
            elif rt == Match.MatchResultType.Club2:
                self.rows[match.club2].won += 1
                self.rows[match.club2].points += 3       # TODO
                self.rows[match.club1].lost += 1
            elif rt == Match.MatchResultType.Draw:
                self.rows[match.club1].drawn += 1
                self.rows[match.club2].drawn += 1
                self.rows[match.club1].points += 1
                self.rows[match.club2].points += 1

    def get_top(self, num):
        names = get_sorted_league_table_clubs(self)
        return names[:num]

def get_sorted_league_table_clubs(table):
    clubs = []
    club_names = []
    for k, v in table.rows.items():
        clubs.append((v.points, v.name))      # TODO: no goal differences
    clubs.sort(reverse = True)
    for points, name in clubs:
        club_names.append(name)
    return club_names

def create_league_table(rounds):
    l = LeagueTable()
    for round in rounds:
        for match in round:
            l.add_match(match)
    return l

def print_league_table(table):
    strings = []
    for k, v in table.rows.items():
        strings.append((v.points, "%-20s %3d %3d %3d %3d %3d" % (v.name, v.played, v.won, v.drawn, v.lost, v.points)))
    strings.sort(reverse = True)
    for points, string in strings:
        print string

class Stage:
    def __init__(self, name, type):
        self.name = name
        self.type = type
        self.club_names = []
        self.promotions = []
        self.relegations = []
        self.rounds = []
        self.current_round = 0
        if self.type == StageType.League:
            self.setup = LeagueSetup()
        else:
            self.setup = CupSetup()

    def get_club_names(self):
        return self.club_names

    def feed_club_names(self, club_names):
        while len(club_names) > 0 and len(self.club_names) < self.setup.participantnum:
            self.club_names.append(club_names.pop(0))

    def to_rounds(self):
        if len(self.rounds) > 0:
            return self.rounds
        missing_clubs = self.setup.participantnum - len(self.club_names)
        while missing_clubs > 0:
            missing_clubs -= 1
            self.club_names.append("unknown")

        if len(self.club_names) == 0:
            return self.rounds
        if self.type == StageType.League:
            plan = round_robin.round_robin(self.club_names)
        else:
            i = False
            l = []
            for c in self.club_names:
                i = not i 
                if i:
                    c1 = c
                else:
                    c2 = c
                    l.append((c1, c2))
            plan = [l]

        for round in plan:
            matches = []
            for c1, c2 in round:
                matches.append(Match.Match(c1, c2, self.setup.matchrules))
            if len(matches) > 0:
                self.rounds.append(matches)
        # self.club_names = []
        return self.rounds

    def get_next_round(self):
        return self.rounds[self.current_round]

    def play_next_round(self):
        if self.current_round >= len(self.rounds):
            return self.get_winners()
        for m in self.rounds[self.current_round]:
            print "%-40s %-20s" % (m, m.play_random())
        self.round_played()

    def round_played(self):
        self.current_round += 1
        if self.current_round >= len(self.rounds):
            print "Stage finished"
            return self.get_winners()
        return []

    def get_winners(self):
        retval = []
        if self.type == StageType.Cup:
            for m in self.rounds[0]:
                w = m.get_winner()
                if w != "unknown":
                    retval.append(w)
        else:
            table = create_league_table(self.rounds)
            retval = table.get_top(self.promotions[0].num)     # TODO - only returns top x
        return retval

    def update_club_names(self, new_club_names):
        self.rounds = []
        self.club_names = [name for name in self.club_names if name != "unknown"]
        self.feed_club_names(new_club_names)
        self.to_rounds()

    def pretty_print(self):
        if self.type == StageType.Cup:
            for round in self.rounds:
                for match in round:
                    if match.mr.result_type() != Match.MatchResultType.NotPlayed:
                        mr = match.mr.__str__()
                    else:
                        mr = ""
                    print "%-40s %-80s" % (match, mr)
        else:
            table = create_league_table(self.rounds)
            print_league_table(table)
