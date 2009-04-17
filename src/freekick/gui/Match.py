#!/usr/bin/python

import random
import tempfile
from lxml import etree

import Primitives
import SoccerData

class MatchRules:
    """Match rules class."""
    def __init__(self, extratime = False, penalties = False):
        self.extratime = extratime
        self.replays = TiebreakerType.Off
        self.awaygoals = TiebreakerType.Off
        self.penalties = penalties

    def allow_draw(self):
        return self.penalties == False and self.awaygoals == TiebreakerType.Off

def std_match_rules():
    return MatchRules(False, False)

class MatchResultType:
    """Enum for match result types."""
    NotPlayed = 0
    Club1 = 1
    Club2 = 2
    Draw = 3

class MatchResult:
    """Match result class."""
    def __init__(self, g1 = -1, g2 = -1, et1 = 0, et2 = 0, pen1 = 0, pen2 = 0):
        """Creates match result. With g1 = -1 and g2 = -1, type will be
        NotPlayed."""
        self.g1 = g1
        self.g2 = g2
        self.et1 = et1
        self.et2 = et2
        self.pen1 = pen1
        self.pen2 = pen2

    def __str__(self):
        s1 = ""
        s2 = ""
        s3 = ""
        if self.pen1 != 0 or self.pen2 != 0:
            s3 = " --- after penalties: %d - %d" % (self.pen1, self.pen2)
        if self.pen1 != 0 or self.pen2 != 0 or self.et1 != 0 or self.et2 != 0:
            s2 = " --- A.E.T. %d - %d" % (self.et1, self.et2)
        s1 = "%d - %d" % (self.g1, self.g2)
        return s1 + s2 + s3

    def result_type(self):
        """Return corresponding MatchResultType."""
        if self.g1 < 0 or self.g2 < 0:
            return MatchResultType.NotPlayed
        if self.g1 > self.g2:
            return MatchResultType.Club1
        elif self.g2 > self.g1:
            return MatchResultType.Club2
        elif self.et1 == 0 and self.et2 == 0 and self.pen1 == 0 and self.pen2 == 0:
            return MatchResultType.Draw
        elif self.et1 > self.et2:
            return MatchResultType.Club1
        elif self.et2 > self.et1:
            return MatchResultType.Club2
        elif self.pen1 == 0 and self.pen2 == 0:
            return MatchResultType.Draw
        elif self.pen1 > self.pen2:
            return MatchResultType.Club1
        elif self.pen2 > self.pen1:
            return MatchResultType.Club2
        else:
            return MatchResultType.Draw

    def play_random_90min(self):
        """Fully randomizes 90 minutes result."""
        max_goals_per_side = 6.0
        self.g1 = int(random.betavariate(2, 5) * max_goals_per_side)
        self.g2 = int(random.betavariate(2, 5) * max_goals_per_side)

    def play_random_et(self):
        """Fully randomizes ET result."""
        max_goals_et = 1
        self.et1 = random.randint(self.g1, self.g1 + max_goals_et)
        self.et2 = random.randint(self.g2, self.g2 + max_goals_et)

    def play_random_penalties(self):
        """Fully randomizes penalty kicks result."""
        while self.pen1 == self.pen2:
            self.pen1 = random.randint(3, 5)
            self.pen2 = random.randint(3, 5)

class TiebreakerType:
    Off = 0
    After90Min = 1
    AfterET = 2

def double_match_result(res1, res2, matchrules):
    """Given two match results and rules, returns corresponding match result
    type.
    
    If results are unfinished according to rules (e.g. no extra time played,
    even though it should have), will also return invalid result (e.g. draw in
    the example.) So only call when results are really final.
    """
    tot_after_90_1 = res1.g2 + res2.g1
    tot_after_90_2 = res1.g1 + res2.g2
    if tot_after_90_1 > tot_after_90_2:
        return MatchResultType.Club1
    elif tot_after_90_2 > tot_after_90_1:
        return MatchResultType.Club2
    awg_1 = res1.g2
    awg_2 = res2.g2
    if matchrules.awaygoals == TiebreakerType.After90Min:
        if awg_1 > awg_2:
            return MatchResultType.Club1
        elif awg_2 > awg_1:
            return MatchResultType.Club2
    if matchrules.awaygoals == TiebreakerType.AfterET:
        awg_1 = res1.g2 + res2.et1
        awg_2 = res2.g2 + res2.et2
        if awg_1 > awg_2:
            return MatchResultType.Club1
        elif awg_2 > awg_1:
            return MatchResultType.Club2
    if matchrules.extratime:
        if tot_after_90_1 + res2.et1 > tot_after_90_2 + res2.et2:
            return MatchResultType.Club1
        if tot_after_90_2 + res2.et2 > tot_after_90_1 + res2.et1:
            return MatchResultType.Club2
    if matchrules.penalties:
        if res2.pen1 > res2.pen2:
            return MatchResultType.Club1
        if res2.pen2 > res2.pen1:
            return MatchResultType.Club2
    # print "Fall through: 90: %d-%d, et: %d-%d, penalties: %d-%d" % (tot_after_90_1, tot_after_90_2, res2.et1, res2.et2, res2.pen1, res2.pen2)
    return MatchResultType.Draw

def generate_simple_random_match_result(matchrules):
    """Given rules, returns a randomized match result."""
    mr = MatchResult()
    mr.play_random_90min()
    if mr.g1 == mr.g2:
        if matchrules.extratime:
            mr.play_random_et()
        if matchrules.penalties and mr.et1 == mr.et2:
            mr.play_random_penalties()
    return mr

def generate_second_leg_random_match_result(prev_res, matchrules):
    """Generates a valid but randomized second leg result."""
    # First play 90 minutes
    res2 = MatchResult()
    res2.play_random_90min()

    # Clear winner?
    tot_after_90_1 = prev_res.g2 + res2.g1
    tot_after_90_2 = prev_res.g1 + res2.g2
    if tot_after_90_1 > tot_after_90_2:
        return res2, MatchResultType.Club1
    elif tot_after_90_2 > tot_after_90_1:
        return res2, MatchResultType.Club2

    # Winner on away goals after 90 minutes?
    if matchrules.awaygoals == TiebreakerType.After90Min:
        awg_1 = prev_res.g2
        awg_2 = res2.g2
        if awg_1 > awg_2:
            return res2, MatchResultType.Club1
        elif awg_2 > awg_1:
            return res2, MatchResultType.Club2

    # Play extra time
    res2.play_random_et()

    # Winner on away goals after extra time?
    if matchrules.awaygoals == TiebreakerType.AfterET:
        awg_1 = prev_res.g2 + res2.et1
        awg_2 = res2.g2 + res2.et2
        if awg_1 > awg_2:
            return res2, MatchResultType.Club1
        elif awg_2 > awg_1:
            return res2, MatchResultType.Club2

    # Winner after extra time?
    if matchrules.extratime:
        if res2.et1 > res2.et2:
            return res2, MatchResultType.Club1
        if res2.et2 > res2.et1:
            return res2, MatchResultType.Club2

    # Play penalties
    res2.play_random_penalties()

    # Winner on penalties?
    if matchrules.penalties:
        if res2.pen1 > res2.pen2:
            return res2, MatchResultType.Club1
        if res2.pen2 > res2.pen1:
            return res2, MatchResultType.Club2
    return res2, MatchResultType.Draw

class Match:
    """Match class."""
    def __init__(self, club1, club2, rules, prev_res, stadium = None, tournament_name = "", stage_name = ""):
        """Match constructor.
        
        :param club1: club 1 (actual club, not just the name)
        :param club2: club 2
        :param rules: match rules
        :param prev_res: previous result (use MatchResult() if unimportant or
        first leg)
        :param stadium: stadium
        :param tournament_name: tournament name
        :param stage_name: stage name
        """
        self.mr = MatchResult()
        self.rules = rules
        self.club1 = club1
        self.club2 = club2
        self.prev_res = prev_res
        self.stadium = stadium
        self.tournament_name = tournament_name
        self.stage_name = stage_name

    def __str__(self):
        s0 = "%-15s - %-15s: %-40s" % (self.tournament_name, self.stage_name, self.club1.name + " - " + self.club2.name)
        s1 = ""
        s2 = ""
        s3 = ""
        s4 = ""
        if self.mr.result_type() != MatchResultType.NotPlayed:
            if self.mr.pen1 != 0 or self.mr.pen2 != 0:
                s3 = " --- after penalties: %d - %d" % (self.mr.pen1, self.mr.pen2)
            if self.mr.pen1 != 0 or self.mr.pen2 != 0 or self.mr.et1 != 0 or self.mr.et2 != 0:
                s2 = " --- A.E.T. %d - %d" % (self.mr.et1, self.mr.et2)
            if self.prev_res.result_type() != MatchResultType.NotPlayed:
                s4 = " --- (%d - %d on agg.)" % (self.mr.g1 + self.prev_res.g2 + self.mr.et1, self.mr.g2 + self.prev_res.g1 + self.mr.et2)
            s1 = "%d - %d" % (self.mr.g1, self.mr.g2)
        return s0 + s1 + s2 + s3 + s4

    def play_match(self):
        """Play match. Save result in the match and return the result."""
        if self.club1.name == "unknown" or self.club2.name == "unknown":
            print self
            raise ValueError("'unknown' plays a match")
        if self.prev_res.result_type() == MatchResultType.NotPlayed:
            if self.rules.allow_draw():
                self.mr = self.generate_simulated_result(False)
            else:
                self.mr = self.generate_simulated_result(True)
        else:
            self.mr = self.generate_simulated_second_match_result()
        return self.mr

    def generate_simulated_second_match_result(self):
        """Create a result that fits the rules for second leg."""
        res, strength = self.generate_general_result(False)
        std_mr = self.generate_desired_result(res, strength, std_match_rules())
        tot_res = double_match_result(self.prev_res, std_mr, self.rules)
        if tot_res == MatchResultType.Draw:
            std_mr.play_random_et()
            tot_res = double_match_result(self.prev_res, std_mr, self.rules)
            if tot_res == MatchResultType.Draw:
                std_mr.play_random_penalties()
        return std_mr

    def generate_general_result(self, tiebreaker):
        """Create a simulated result.
        
        :param tiebreaker: true if this is a tiebreaker (no draw allowed)
        """
        c1 = self.club1.get_rating() / 1000.0
        c2 = self.club2.get_rating() / 1000.0
        c1r = int(c1 ** 2.0)
        c2r = int(c2 ** 2.0)
        if c1r > c2r * 1.5:
            return MatchResultType.Club1, 1.0
        elif c2r > c1r * 1.5:
            return MatchResultType.Club2, 1.0
        tot_rating = c1r + c2r
        chosen_point = random.randint(0, tot_rating)
        draw_point_1 = min(c1r, c2r) - (min(c1r, c2r) / 3)
        draw_point_2 = min(c1r, c2r) + (min(c1r, c2r) / 3)
        # print "club values: %d %d; tot: %d, chosen: %d, d1: %d, d2: %d" % (c1r, c2r, tot_rating, chosen_point, draw_point_1, draw_point_2)
        if tiebreaker:
            mid_point = draw_point_1 + ((draw_point_2 - draw_point_1) / 2)
            draw_point_1 = mid_point
            draw_point_2 = mid_point
        area_0_len = draw_point_1
        area_1_len = draw_point_2 - draw_point_1
        area_2_len = tot_rating - draw_point_2
        if chosen_point < draw_point_1:
            res = MatchResultType.Club1
            strength = abs(chosen_point - draw_point_1) / float(area_0_len)
        elif chosen_point > draw_point_2 or tiebreaker:
            res = MatchResultType.Club2
            strength = abs(chosen_point - draw_point_2) / float(area_2_len)
        else:
            res = MatchResultType.Draw
            strength = 0
        return res, strength

    def generate_simulated_result(self, tiebreaker):
        """Create and return simulated result.

        :param tiebreaker: true if this is a tiebreaker (no draw allowed)
        """
        res, strength = self.generate_general_result(tiebreaker)
        return self.generate_desired_result(res, strength, self.rules)

    def generate_desired_result(self, type, strength, rules):
        """Generate the result as desired in type.
        
        :param type: match result type
        :param strength: an integer that represents the strength relationship
        between clubs.
        :param rules: match rules
        """
        thismr = MatchResult()
        if type == MatchResultType.Draw:
            thismr.g1 = random.randint(0, 3)
            thismr.g2 = thismr.g1
            if rules.extratime:
                thismr.et1 = thismr.g1
                thismr.et2 = thismr.g2
        else:
            et1 = 0
            et2 = 0
            pen1 = 0
            pen2 = 0
            if rules.extratime and strength < 0.3:
                g1 = random.randint(0, 2)
                g2 = g1
                if rules.penalties and strength < 0.2:
                    et1 = g1 + random.randint(0, 1)
                    et2 = et1
                    pen1 = random.randint(4, 5)
                    pen2 = random.randint(3, (pen1 - 1))
                else:
                    et1 = g1 + 1
                    et2 = g2
            else:
                winner_max_goals = max(1, int(strength * 6))
                g1 = random.randint(1, winner_max_goals)
                g2 = random.randint(0, min(2, (g1 - 1)))
                if g1 > 4:
                    g2 = min(g2, 1)
            if type == MatchResultType.Club1:
                thismr.g1 = g1
                thismr.g2 = g2
                thismr.et1 = et1
                thismr.et2 = et2
                thismr.pen1 = pen1
                thismr.pen2 = pen2
            else:
                thismr.g1 = g2
                thismr.g2 = g1
                thismr.et1 = et2
                thismr.et2 = et1
                thismr.pen1 = pen2
                thismr.pen2 = pen1
        return thismr

    def play_random(self):
        """Generate random result."""
        self.mr = generate_simple_random_match_result(rules)
        return self.mr

    def get_winner_name(self):
        """Returns winner club name or 'draw' if draw."""
        rt = self.mr.result_type()
        if rt == MatchResultType.Club1:
            return self.club1.name
        elif rt == MatchResultType.Club2:
            return self.club2.name
        else:
            return "draw"

    def create_temp_xml(self, db):
        """Create XML representing the match (for server)."""
        temp_file = tempfile.NamedTemporaryFile()
        temp_file_name = temp_file.name
        root = etree.Element("Match")

        clubs = etree.SubElement(root, "Clubs")
        club1node = self.club1.to_xml()
        club1node.set("home", "1")
        clubs.append(club1node)
        club2node = self.club2.to_xml()
        club2node.set("home", "0")
        clubs.append(club2node)

        club1plnode = etree.SubElement(root, "HomeClubPlayers")
        club2plnode = etree.SubElement(root, "AwayClubPlayers")
        for player in self.club1.players.values():
            pnode = player.to_xml()
            statusnode = etree.SubElement(pnode, "status")
            statusnode.set("warnings", "0")   # TODO
            statusnode.set("form", "1.0")     # TODO
            club1plnode.append(pnode)
        for player in self.club2.players.values():
            pnode = player.to_xml()
            statusnode = etree.SubElement(pnode, "status")
            statusnode.set("warnings", "0")
            statusnode.set("form", "1.0")
            club2plnode.append(pnode)

        otherkitsnode = etree.SubElement(root, "OtherKits")
        gk1kit = SoccerData.Kit() # TODO - better kits
        gk1kit.jersey_colors.append(Primitives.Color())
        gk2kit = SoccerData.Kit()
        gk2kit.jersey_colors.append(Primitives.Color())
        refkit = SoccerData.Kit()
        refkit.jersey_colors.append(Primitives.Color())
        gk1kitnode = gk1kit.to_xml()
        gk2kitnode = gk2kit.to_xml()
        refkitnode = refkit.to_xml()
        gk1kitnode.set("owner", self.club1.name)
        gk2kitnode.set("owner", self.club1.name)
        refkitnode.set("owner", "referee")
        otherkitsnode.append(gk1kitnode)
        otherkitsnode.append(gk2kitnode)
        otherkitsnode.append(refkitnode)

        tournamentnode = etree.SubElement(root, "Tournament", name = self.tournament_name, stage = self.stage_name)
        stadiumnode = self.stadium.to_xml()
        stadiumnode.set("attendance", str(random.randint(10, self.stadium.capacity)))  # TODO: more realistic attendance and third party host flag
        root.append(stadiumnode)

        pitchnode = self.stadium.pitch.to_xml()
        root.append(pitchnode)

        startnode = etree.SubElement(root, "start", date = str(self.date), time = str(self.time))
        weathernode = etree.SubElement(root, "weather", description = "normal")
        ballnode = etree.SubElement(root, "ball", description = "normal")

        formation1node = self.club1.formation.to_xml()
        formation2node = self.club2.formation.to_xml()
        formation1node.set("owner", self.club1.name)
        formation2node.set("owner", self.club2.name)
        root.append(formation1node)
        root.append(formation2node)

        lineup1node = self.club1.lineup.to_xml()
        lineup2node = self.club2.lineup.to_xml()
        lineup1node.set("owner", self.club1.name)
        lineup2node.set("owner", self.club2.name)
        root.append(lineup1node)
        root.append(lineup2node)

        temp_file.write(etree.tostring(root, pretty_print = True))
        temp_file.flush()
        print "Temp file saved as", temp_file_name
        f = raw_input()

if __name__ == "__main__":
    simple = MatchRules()
    cl = MatchRules(True, True)
    cl.awaygoals = TiebreakerType.After90Min
    cup = MatchRules(True, True)
    complex = MatchRules(True, True)
    complex.awaygoals = TiebreakerType.AfterET
    random.seed(24)
    num_simples = 4
    for i in range(0, num_simples):
        simpleres = generate_simple_random_match_result(simple)
        print "Simple: \t%-30s   %s" % (simpleres, simpleres.result_type())

    num_cups = 4
    for i in range(0, num_cups):
        cupres = generate_simple_random_match_result(cup)
        print "Cup:    \t%-30s   %s" % (cupres, cupres.result_type())

    num_cls = 4
    for i in range(0, num_cls):
        simpleres = generate_simple_random_match_result(simple)
        clres, clwinner = generate_second_leg_random_match_result(simpleres, cl)
        print "CL:     \t%-30s   %-30s   %s (%s)" % (simpleres, clres, clwinner, double_match_result(simpleres, clres, cl))

    num_complex = 4
    for i in range(0, num_complex):
        simpleres = generate_simple_random_match_result(simple)
        complexres, complexwinner = generate_second_leg_random_match_result(simpleres, complex)
        print "Complex:\t%-30s   %-30s   %s (%s)" % (simpleres, complexres, complexwinner, double_match_result(simpleres, complexres, complex))
