#!/usr/bin/python

import random

import Primitives
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
        self.in_league_system = False

    def add_stage(self, stage):
        self.stages.append(stage)
        self.current_stage = len(self.stages) - 1

    def add_clubs(self, clubs):
        for s in self.stages:
            s.feed_club_names(clubs)
            if len(clubs) == 0:
                break

    def clear_clubs(self):
        for s in self.stages:
            s.clear_clubs()

    def get_next_round(self):
        if self.current_stage < 0:
            return []
        return self.stages[self.current_stage].get_next_round()

    def round_played(self, db):
        if self.current_stage < 0:
            return True
        winners = self.stages[self.current_stage].round_played()
        return self.update_stage(winners, db)

    def update_stage(self, winners, db):
        if len(winners) > 0:
            self.current_stage -= 1
            if self.current_stage >= 0:
                self.stages[self.current_stage].update_club_names(winners, db)
            return True
        return False

    def get_attendances(self):
        return self.stages[0].attendances

    def get_promotions(self):
        return self.stages[0].get_promotions()

    def get_relegations(self):
        return self.stages[0].get_relegations()

    def get_staying(self):
        return self.stages[0].get_staying()

    def finished(self):
        if len(self.stages) > 1:
            return self.current_stage < 0
        elif len(self.stages) == 0:
            return False
        else:
            return self.stages[self.current_stage].finished()

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
        self.goals_for = 0
        self.goals_against = 0
        self.goals_total = 0
        self.points = 0

class LeagueTable:
    def __init__(self):
        self.rows = {}

    def add_match(self, match, pointsperwin):
        if match.club1.name not in self.rows:
            self.rows[match.club1.name] = LeagueTableRow(match.club1.name)
        if match.club2.name not in self.rows:
            self.rows[match.club2.name] = LeagueTableRow(match.club2.name)

        rt = match.mr.result_type()
        if rt != Match.MatchResultType.NotPlayed:
            self.rows[match.club1.name].played += 1
            self.rows[match.club2.name].played += 1
            self.rows[match.club1.name].goals_for += match.mr.g1
            self.rows[match.club1.name].goals_against += match.mr.g2
            self.rows[match.club2.name].goals_for += match.mr.g2
            self.rows[match.club2.name].goals_against += match.mr.g1
            self.rows[match.club1.name].goals_total = self.rows[match.club1.name].goals_for - self.rows[match.club1.name].goals_against 
            self.rows[match.club2.name].goals_total = self.rows[match.club2.name].goals_for - self.rows[match.club2.name].goals_against
            if rt == Match.MatchResultType.Club1:
                self.rows[match.club1.name].won += 1
                self.rows[match.club1.name].points += pointsperwin
                self.rows[match.club2.name].lost += 1
            elif rt == Match.MatchResultType.Club2:
                self.rows[match.club2.name].won += 1
                self.rows[match.club2.name].points += pointsperwin
                self.rows[match.club1.name].lost += 1
            elif rt == Match.MatchResultType.Draw:
                self.rows[match.club1.name].drawn += 1
                self.rows[match.club2.name].drawn += 1
                self.rows[match.club1.name].points += 1
                self.rows[match.club2.name].points += 1

    def get_top(self, num):
        names = get_sorted_league_table_clubs(self)
        return names[:num]

    def get_bottom(self, num):
        names = get_sorted_league_table_clubs(self)
        return names[-num:]

    def get_all(self):
        return get_sorted_league_table_clubs(self)

def get_sorted_league_table_clubs(table):
    clubs = []
    club_names = []
    for k, v in table.rows.items():
        clubs.append((v.points, v.goals_total, v.goals_for, v.name))
    clubs.sort(reverse = True)
    for points, goals_total, goals_for, name in clubs:
        club_names.append(name)
    return club_names

def create_league_table(rounds, club_names, pointsperwin):
    l = LeagueTable()
    for round in rounds:
        for match in round:
            if match.club1.name in club_names and match.club2.name in club_names:
                l.add_match(match, pointsperwin)
    return l

def print_league_table(table):
    strings = []
    for k, v in table.rows.items():
        strings.append(((v.points, v.goals_total, v.goals_for), "%-20s %3d %3d %3d %3d %3d %3d %3d %3d" % (v.name, v.played, v.won, v.drawn, v.lost, v.goals_for, v.goals_against, v.goals_total, v.points)))
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
        self.attendances = []
        if self.type == StageType.League:
            self.setup = LeagueSetup()
            self.num_planned_rounds = len(self.rounds)
        else:
            self.setup = CupSetup()
            self.num_planned_rounds = self.setup.rounds
            if self.setup.matchrules.replays != Match.TiebreakerType.Off:
                self.num_planned_rounds += 1

    def get_club_names(self):
        return self.club_names

    def feed_club_names(self, club_names):
        while len(club_names) > 0 and len(self.club_names) < self.setup.participantnum:
            self.club_names.append(club_names.pop(0))

    def clear_clubs(self):
        self.club_names = []
        self.rounds = []
        self.current_round = 0

    def to_rounds(self, db):
        if len(self.rounds) > 0:
            return self.rounds
        missing_clubs = self.setup.participantnum - len(self.club_names)
        while missing_clubs > 0:
            missing_clubs -= 1
            self.club_names.append("unknown")

        if len(self.club_names) == 0:
            return self.rounds
        random.shuffle(self.club_names)
        if self.type == StageType.League:
            num_clubs_per_club = self.setup.participantnum / self.setup.groups
            plans = []
            self.groups_club_names = []
            for i in range(0, self.setup.groups):
                self.groups_club_names.append(self.club_names[i * num_clubs_per_club:(i + 1) * num_clubs_per_club])
            for i in range(0, self.setup.groups):
                plans.append(round_robin.round_robin(self.groups_club_names[i]))
            plan = Primitives.ziplists(plans) * self.setup.rounds
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
            plan = []
            i = False
            l_sw = [Primitives.switch_tuple(cp) for cp in l]
            for i in range(self.setup.rounds):
                i = not i
                if i:
                    plan.append(l)
                else:
                    plan.append(l_sw)

        for round in plan:
            matches = []
            self.multiple_legs = self.type == StageType.Cup and self.setup.rounds > 1
            used_rules = self.setup.matchrules
            if self.multiple_legs or self.setup.matchrules.replays != Match.TiebreakerType.Off:   # TODO: replays.AfterET
                used_rules = Match.std_match_rules()
            for c1, c2 in round:
                m = Match.Match(db.clubs[c1], db.clubs[c2], used_rules, Match.MatchResult())
                matches.append(m)
            if len(matches) > 0:
                self.rounds.append(matches)
        if self.setup.matchrules.replays != Match.TiebreakerType.Off:
            self.rounds.append([])
        if self.type == StageType.League:
            self.num_planned_rounds = len(self.rounds)
        else:
            self.num_planned_rounds = self.setup.rounds
            if self.setup.matchrules.replays != Match.TiebreakerType.Off:
                self.num_planned_rounds += 1
        return self.rounds

    def get_next_round(self):
        if self.current_round >= self.num_planned_rounds:
            return []
        if self.multiple_legs and self.current_round == len(self.rounds) - 1:
            for i in range(len(self.rounds[self.current_round])):
                self.rounds[self.current_round][i].prev_res = self.rounds[self.current_round - 1][i].mr
                self.rounds[self.current_round][i].rules = self.setup.matchrules
        elif self.type == StageType.Cup and self.setup.matchrules.replays != Match.TiebreakerType.Off:
            matches = []
            for i in range(len(self.rounds[self.current_round - 1])):
                if self.rounds[self.current_round - 1][i].mr.result_type() == Match.MatchResultType.Draw:
                    c1 = self.rounds[self.current_round - 1][i].club2
                    c2 = self.rounds[self.current_round - 1][i].club1
                    m = Match.Match(c1, c2, self.setup.matchrules, Match.MatchResult())
                    matches.append(m)
                    # print "Added match:", m
            self.rounds[self.current_round].extend(matches)

        return self.rounds[self.current_round]

    def round_played(self):
        self.current_round += 1
        if self.current_round >= self.num_planned_rounds:
            print "Stage finished"
            return self.get_winners()
        return []

    def finished(self):
        return self.num_planned_rounds > 0 and self.current_round >= self.num_planned_rounds

    def get_winners(self):
        retval = []
        if self.type == StageType.Cup:
            if self.setup.matchrules.replays != Match.TiebreakerType.Off:
                for m in self.rounds[0]:
                    w = m.get_winner_name()
                    if w != "unknown" and w != "draw":
                        retval.append(w)
                for m in self.rounds[1]:
                    w = m.get_winner_name()
                    if w == "draw":
                        raise ValueError("Replay match was a draw!")
                    if w != "unknown":
                        retval.append(w)
            else:
                if self.setup.rounds == 1:
                    for m in self.rounds[0]:
                        w = m.get_winner_name()
                        if w != "unknown":
                            retval.append(w)
                elif self.setup.rounds == 2:
                    ms = zip(self.rounds[0], self.rounds[1])
                    for m1, m2 in ms:
                        w = Match.double_match_result(m1.mr, m2.mr, self.setup.matchrules)
                        if w == Match.MatchResultType.Club1:
                            retval.append(m2.club1.name)
                        elif w == Match.MatchResultType.Club2:
                            retval.append(m2.club2.name)
                        else:
                            raise ValueError("Match.double_match_result() returned draw!")
                else:
                    raise ValueError("Only up to two rounds per knockout stage supported at the moment")
        else:
            for group in self.groups_club_names:
                table = create_league_table(self.rounds, group, self.setup.pointsperwin)
                if len(self.promotions) > 0:
                    retval.extend(table.get_top(self.promotions[0].num / self.setup.groups))  # TODO - only returns top x
        return retval

    def get_attendances(self):
        return self.attendances()

    def get_promotions(self):
        if self.type == StageType.League:
            ret = []
            clubs = self.get_winners()
            for club in clubs:
                ret.append((self.promotions[0].tournament, self.promotions[0].stage, club))
            return ret
        else:
            return []

    def get_relegations(self):
        if self.type == StageType.League:
            clubs = []
            ret = []
            for group in self.groups_club_names:
                table = create_league_table(self.rounds, group, self.setup.pointsperwin)
                if len(self.relegations) > 0:
                    clubs.extend(table.get_bottom(self.promotions[0].num / self.setup.groups))  # TODO - only returns bottom x
            for club in clubs:
                ret.append((self.relegations[0].tournament, self.relegations[0].stage, club))
            return ret
        else:
            return []

    def get_staying(self):
        if self.type == StageType.League:
            proms = self.get_promotions()
            rels = self.get_relegations()
            all = []
            for group in self.groups_club_names:
                table = create_league_table(self.rounds, group, self.setup.pointsperwin)
                all += table.get_all()
            ret = []
            for club in all:
                if club not in proms and club not in rels:
                    ret.append(club)
            return ret
        else:
            return []

    def update_club_names(self, new_club_names, db):
        self.rounds = []
        self.club_names = [name for name in self.club_names if name != "unknown"]
        self.feed_club_names(new_club_names)
        self.to_rounds(db)

    def pretty_print(self):
        if self.type == StageType.Cup:
            for round in self.rounds:
                for match in round:
                    print match
        else:
            for i in range(len(self.groups_club_names)):
                print "Group", (i + 1)
                table = create_league_table(self.rounds, self.groups_club_names[i], self.setup.pointsperwin)
                print "%-20s %3s %3s %3s %3s %3s %3s %3s %3s" % ("Name", "P", "W", "D", "L", "F", "A", "T", "P")
                print_league_table(table)
