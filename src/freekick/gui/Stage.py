#!/usr/bin/python

import random
import copy

import Primitives
import round_robin
import Match
import LeagueTable
import SoccerData

class StageType:
    """Enumeration for stage types."""
    League = 0
    Cup = 1

class StageSetup:
    """Superclass for all stage setups (league and cup)."""
    def __init__(self):
        """Default stage setup."""
        self.seeded = False
        self.rounds = 1
        self.matchrules = Match.MatchRules()
        self.participantnum = 0

class LeagueSetup(StageSetup):
    """League setup class."""
    def __init__(self):
        StageSetup.__init__(self)
        self.pointsperwin = 3
        self.groups = 1

class CupSetup(StageSetup):
    """Cup setup class."""
    def __init__(self):
        StageSetup.__init__(self)
        self.matchrules.extratime = True
        self.matchrules.penalties = True

def gen_preliminary_stage(name, oth_stage, num_participants):
    """Generate preliminary stage.

    Generates a stage. The rules of the other stage will be copied, but no
    names of clubs or results. This stage will be a feeder to the other
    stage. The stage will accept num_participants participants.
    """
    s = Stage(name, oth_stage.type)
    if num_participants % 2 == 1:
        raise ValueError("Invalid number of participants")
    s.name = name
    s.type = oth_stage.type
    s.club_names = []
    s.promotions = []
    s.promotions.append(SoccerData.Exchange(0, num_participants / 2, 
        oth_stage.promotions[0].tournament, oth_stage.name))
    s.relegations = []
    s.rounds = []
    s.current_round = 0
    s.attendances = list(oth_stage.attendances)
    s.setup = copy.deepcopy(oth_stage.setup)
    s.setup.participantnum = num_participants
    s.num_planned_rounds = oth_stage.num_planned_rounds
    return s

class Stage:
    """Class for stages."""
    def __init__(self, name, type):
        """Stage constructor.

        :param name: stage name.
        :param type: StageType (Cup or League).
        """
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
        """Return list of names of all clubs in stage."""
        return self.club_names

    def feed_club_names(self, club_names):
        """Feed stage with club names."""
        while len(club_names) > 0 and len(self.club_names) < self.setup.participantnum:
            self.club_names.append(club_names.pop(0))

    def clear_clubs(self):
        """Clear all club names."""
        self.club_names = []
        self.rounds = []
        self.current_round = 0

    def to_rounds(self, db):
        """Return rounds.
        
        Given DB, create rounds of the schedule. Call feed_club_names first
        in order to feed stage with club names. If club names missing, rounds
        will be filled with 'unknown'. """
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
            for ind in range(self.setup.rounds):
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
                m = Match.Match(db.clubs[c1], db.clubs[c2], used_rules,
                        Match.MatchResult(), stage_name = self.name)
                if db.clubs[c1].name != "unknown":
                    m.stadium = db.stadiums[db.clubs[c1].stadium]
                m.stage_name = self.name
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
        """Return next round in stage or an empty list if no more rounds."""
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
                    m = Match.Match(c1, c2, self.setup.matchrules,
                            Match.MatchResult(), 
                            self.rounds[self.current_round - 1][i].stadium, 
                            stage_name = self.name)
                    m.tournament_name = self.rounds[self.current_round - 1][i].tournament_name
                    m.stage_name = self.rounds[self.current_round - 1][i].stage_name
                    matches.append(m)
                    # print "Added match:", m
            self.rounds[self.current_round].extend(matches)

        return self.rounds[self.current_round]

    def round_played(self):
        """Returns winners of the stage when finished or an empty list
        otherwise.

        Call this function after playing the last round.
        """
        self.current_round += 1
        if self.current_round >= self.num_planned_rounds:
            print "Stage finished"
            return self.get_winners()
        return []

    def finished(self):
        """Return True if stage finished."""
        return self.num_planned_rounds > 0 and self.current_round >= self.num_planned_rounds

    def get_winners(self):
        """Get stage winners.

        If the stage was a cup, returns winning clubs of match pairs.
        If the stage was a league, returns top x clubs of the league, x
        being the number of promotions in stage setup.
        If the stage was not yet finished, will probably fail.
        """
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
                table = LeagueTable.create_league_table(self.rounds, group, self.setup.pointsperwin)
                if len(self.promotions) > 0:
                    retval.extend(table.get_top(self.promotions[0].num / self.setup.groups))  # TODO - only returns top x
        return retval

    def get_attendances(self):
        """Return list of attendances for this stage."""
        return self.attendances()

    def get_promotions(self):
        """Return promoted club names.

        If cup, will return an empty list (TODO). If league, will return a
        triple of lists of (tournamentname, stagename, clubname), with the
        target tournament/stage of the club.
        """
        if self.type == StageType.League:
            ret = []
            clubs = self.get_winners()
            for club in clubs:
                ret.append((self.promotions[0].tournament, self.promotions[0].stage, club))
            return ret
        else:
            return []

    def get_relegations(self):
        """Return relegated club names.

        If cup, will return an empty list (TODO). If league, will return names
        of bottom x clubs in the league, x being as defined somewhere (probably
        nowhere).
        """
        if self.type == StageType.League:
            clubs = []
            ret = []
            for group in self.groups_club_names:
                table = LeagueTable.create_league_table(self.rounds, group, self.setup.pointsperwin)
                if len(self.relegations) > 0:
                    clubs.extend(table.get_bottom(self.relegations[0].num / self.setup.groups))  # TODO - only returns bottom x
            for club in clubs:
                ret.append((self.relegations[0].tournament, self.relegations[0].stage, club))
            return ret
        else:
            return []

    def get_staying(self):
        """Return the clubs not promoted or relegated.

        If cup, will return an empty list. Otherwise, will return the clubs not
        listed in promotions or relegations.
        """
        if self.type == StageType.League:
            proms = self.get_promotions()
            rels = self.get_relegations()
            exc_clubnames = []
            for t, s, c in proms + rels:
                exc_clubnames.append(c)
            all = []
            for group in self.groups_club_names:
                table = LeagueTable.create_league_table(self.rounds, group, self.setup.pointsperwin)
                all += table.get_all()
            ret = []
            for club in all:
                if club not in exc_clubnames:
                    ret.append(club)
            return ret
        else:
            return []

    def update_club_names(self, new_club_names, db):
        """Updates club names.

        :param new_club_names: names of clubs to add to the stage.
        :param db: database.

        You can use this instead of calling feed_club_names and to_rounds.
        """

        self.rounds = []
        self.club_names = [name for name in self.club_names if name != "unknown"]
        self.feed_club_names(new_club_names)
        self.to_rounds(db)

    def pretty_print(self):
        """Print stage, either cup match pairs or league table."""
        if self.type == StageType.Cup:
            for round in self.rounds:
                for match in round:
                    print match
        else:
            for i in range(len(self.groups_club_names)):
                print "Group", (i + 1)
                table = LeagueTable.create_league_table(self.rounds, self.groups_club_names[i], self.setup.pointsperwin)
                print "%-20s %3s %3s %3s %3s %3s %3s %3s %3s" % ("Name", "P", "W", "D", "L", "F", "A", "T", "P")
                LeagueTable.print_league_table(table)


