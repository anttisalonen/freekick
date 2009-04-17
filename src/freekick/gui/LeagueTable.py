#!/usr/bin/python

import Match

class LeagueTableRow:
    """League table row class."""
    def __init__(self, name):
        """Initializes a league table with the given club name."""
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
    """League table class."""
    def __init__(self):
        """Initializes an empty table."""
        self.rows = {}

    def add_match(self, match, pointsperwin):
        """Add match to the table."""
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
        """Returns sorted names of the top num clubs of the league table."""
        names = get_sorted_league_table_clubs(self)
        return names[:num]

    def get_bottom(self, num):
        """Returns sorted names of the bottom num clubs of the league table."""
        names = get_sorted_league_table_clubs(self)
        return names[-num:]

    def get_all(self):
        """Returns sorted names of the clubs of the league table."""
        return get_sorted_league_table_clubs(self)

def get_sorted_league_table_clubs(table):
    """Given league table, return list of club names, sorted by league pos."""
    clubs = []
    club_names = []
    for k, v in table.rows.items():
        clubs.append((v.points, v.goals_total, v.goals_for, v.name))
    clubs.sort(reverse = True)
    for points, goals_total, goals_for, name in clubs:
        club_names.append(name)
    return club_names

def create_league_table(rounds, club_names, pointsperwin):
    """Create league table from rounds, club names and ppw."""
    l = LeagueTable()
    for round in rounds:
        for match in round:
            if match.club1.name in club_names and match.club2.name in club_names:
                l.add_match(match, pointsperwin)
    return l

def print_league_table(table):
    """Pretty print league table."""
    strings = []
    for k, v in table.rows.items():
        strings.append(((v.points, v.goals_total, v.goals_for), "%-20s %3d %3d %3d %3d %3d %3d %3d %3d" % (v.name, v.played, v.won, v.drawn, v.lost, v.goals_for, v.goals_against, v.goals_total, v.points)))
    strings.sort(reverse = True)
    for points, string in strings:
        print string


