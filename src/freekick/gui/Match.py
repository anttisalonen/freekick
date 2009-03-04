#!/usr/bin/python

import random

class MatchRules:
    def __init__(self, extratime = False, penalties = False):
        self.extratime = extratime
        self.replays = TiebreakerType.Off
        self.awaygoals = TiebreakerType.Off
        self.penalties = penalties

class MatchResultType:
    NotPlayed = 0
    Club1 = 1
    Club2 = 2
    Draw = 3

class MatchResult:
    def __init__(self, g1 = -1, g2 = -1, et1 = 0, et2 = 0, pen1 = 0, pen2 = 0):
        self.g1 = g1
        self.g2 = g2
        self.et1 = et1
        self.et2 = et2
        self.pen1 = pen1
        self.pen2 = pen2

    def result_type(self):
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
        max_goals_per_side = 6.0
        self.g1 = int(random.betavariate(2, 5) * max_goals_per_side)
        self.g2 = int(random.betavariate(2, 5) * max_goals_per_side)

    def play_random_et(self):
        max_goals_et = 1
        self.et1 = random.randint(self.g1, self.g1 + max_goals_et)
        self.et2 = random.randint(self.g2, self.g2 + max_goals_et)

    def play_random_penalties(self):
        while self.pen1 == self.pen2:
            self.pen1 = random.randint(1, 5)
            self.pen2 = random.randint(1, 5)

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

class TiebreakerType:
    Off = 0
    After90Min = 1
    AfterET = 2

def double_match_result(res1, res2, extratime, penalties, away_goals):
    tot_after_90_1 = res1.g2 + res2.g1
    tot_after_90_2 = res1.g1 + res2.g2
    if tot_after_90_1 > tot_after_90_2:
        return MatchResultType.Club1
    elif tot_after_90_2 > tot_after_90_1:
        return MatchResultType.Club2
    awg_1 = res1.g2
    awg_2 = res2.g2
    if away_goals == TiebreakerType.After90Min:
        if awg_1 > awg_2:
            return MatchResultType.Club1
        elif awg_2 > awg_1:
            return MatchResultType.Club2
    if away_goals == TiebreakerType.AfterET:
        awg_1 = res1.g2 + res2.et1
        awg_2 = res2.g2 + res2.et2
        if awg_1 > awg_2:
            return MatchResultType.Club1
        elif awg_2 > awg_1:
            return MatchResultType.Club2
    if extratime:
        if res2.et1 > res2.et2:
            return MatchResultType.Club1
        if res2.et2 > res2.et1:
            return MatchResultType.Club2
    if penalties:
        if res2.pen1 > res2.pen2:
            return MatchResultType.Club1
        if res2.pen2 > res2.pen1:
            return MatchResultType.Club2
    return MatchResultType.Draw

def generate_simple_random_match_result(matchrules):
    mr = MatchResult()
    mr.play_random_90min()
    if mr.g1 == mr.g2:
        if matchrules.extratime:
            mr.play_random_et()
        if matchrules.penalties and mr.et1 == mr.et2:
            mr.play_random_penalties()
    return mr

def generate_second_leg_random_match_result(extratime, penalties, away_goals, prev_res):
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
    if away_goals == TiebreakerType.After90Min:
        awg_1 = res1.g2
        awg_2 = res2.g2
        if awg_1 > awg_2:
            return res2, MatchResultType.Club1
        elif awg_2 > awg_1:
            return res2, MatchResultType.Club2

    # Play extra time
    res2.play_random_et()

    # Winner on away goals after extra time?
    if away_goals == TiebreakerType.AfterET:
        awg_1 = res1.g2 + res2.et1
        awg_2 = res2.g2 + res2.et2
        if awg_1 > awg_2:
            return res2, MatchResultType.Club1
        elif awg_2 > awg_1:
            return res2, MatchResultType.Club2

    # Winner after extra time?
    if extratime:
        if res2.et1 > res2.et2:
            return res2, MatchResultType.Club1
        if res2.et2 > res2.et1:
            return res2, MatchResultType.Club2

    # Play penalties
    res2.play_random_penalties()

    # Winner on penalties?
    if penalties:
        if res2.pen1 > res2.pen2:
            return res2, MatchResultType.Club1
        if res2.pen2 > res2.pen1:
            return res2, MatchResultType.Club2
    return res2, MatchResultType.Draw

class Match:
    def __init__(self, club1, club2, rules, place = None, date = None, time = None, match_name = ""):
        self.mr = MatchResult()
        self.rules = rules
        self.club1 = club1
        self.club2 = club2

    def __str__(self):
        return self.club1.__str__() + " - " + self.club2.__str__()

    def play_random(self):
        self.mr = generate_simple_random_match_result(self.rules)
        return self.mr

    def get_winner(self):
        rt = self.mr.result_type()
        if rt == MatchResultType.Club1:
            return self.club1
        elif rt == MatchResultType.Club2:
            return self.club2
        else:
            return "draw"

