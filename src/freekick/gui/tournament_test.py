#!/usr/bin/python

import sys
import datetime
import copy

import Database
import SoccerData
import Schedule
import Tournament
import Match

database_path = "../../../share/DB/"
db = Database.get_db(database_path)

def print_usage():
    print "Usage: %s countryname tournamentname" % sys.argv[0]

def get_country_league(cname, tname, db):
    """Get a copy of a specific league in a country.
    
    Given country name and league name (stage name), returns a 
    Tournament which has the league as the copy of the given stage.
    Raises KeyError if stage not found."""
    for l in db.countries[cname].leaguesystem.levels:
        for b in l.branches:
            for s in b.stages:
                if s.name == tname:
                    t = Tournament.Tournament(tname)
                    t.stages = [copy.deepcopy(s)]
                    t.in_league_system = True
                    return t
    raise KeyError("Tournament '%s' not found" % tname)

def get_tournaments():
    tournaments = {}
    if len(sys.argv) == 2:          # Either DIY or a whole country
        try:                        # DIY
            tournamentname = sys.argv[1]
            tournaments[tournamentname] = copy.deepcopy(db.tournaments[tournamentname])
        except KeyError:            # whole country
            countryname = sys.argv[1]
            for k, v in db.countries[countryname].tournaments.items():
                tournaments[k] = copy.deepcopy(v)
            for l in db.countries[countryname].leaguesystem.levels:
                for b in l.branches:
                    for s in b.stages:
                        t = Tournament.Tournament(s.name)
                        t.stages = [copy.deepcopy(s)]
                        t.in_league_system = True
                        tournaments[s.name] = t
    else:                           # Specific tournament from a country
        countryname = sys.argv[1]
        tournamentname = sys.argv[2]
        try:                        # tournament
            tournaments[tournamentname] = copy.deepcopy(db.countries[countryname].tournaments[tournamentname])
        except KeyError:            # league system
            tournaments[tournamentname] = get_country_league(countryname, tournamentname, db)
    return tournaments

class world:
    def __init__(self, tournaments, db):
        self.tournaments = tournaments.values()
        self.db = db
        self.year = 2007
        self.num_finished_seasons = 0
        self.prepare_tournament_templates(tournaments)
        self.schedule = Schedule.Schedule([])
        self.schedule.add_season_to_schedule(get_startdate(self.year),
                get_enddate(self.year), self.tournaments, self.db)
        self.finished_tournaments = []

    def prepare_tournament_templates(self, ts):
        self.tournament_templates = copy.deepcopy(ts)
        for t in self.tournament_templates.values():
            t.clear_clubs()

    def next_round(self):
        for d, t in self.schedule.next_event():
            yield d, t, t.get_next_round()
            t.round_played(db) # return value ignored
            if t.finished():
                print "Tournament '%s' finished on %s" % (t.name, d)
                t.pretty_print()
                f = raw_input()
                self.finished_tournaments.append(t)

    def get_schedule(self, clubname):
        ret = []
        for t in self.tournaments:
            for stage in t.stages:
                for round in stage.rounds:
                    for match in round:
                        if match.has_club(clubname):
                            ret.append(match)
        return ret

    def new_schedule(self):
        self.schedule = Schedule.Schedule([])
        self.num_finished_seasons += 1
        self.tournaments = create_next_season(self.tournament_templates, self.finished_tournaments)
        self.finished_tournaments = []
        self.schedule.add_season_to_schedule(
                get_startdate(self.year + self.num_finished_seasons),
                get_enddate(self.year + self.num_finished_seasons), 
                self.tournaments, self.db)
        for newt in self.tournaments:
            newt.pretty_print()
        f = raw_input()

def main():
    """Entry point."""
    if len(sys.argv) == 1:
        print_usage()
        sys.exit(1)

    tournaments = get_tournaments()
    if len(tournaments) == 0:
        raise KeyError("Tournament/country not found")

    w = world(tournaments, db)
    create_xml = False
    show_each_round = False
    new_season = len(tournaments) > 1

    plclub = "Sunderland"
    
    while True:
        ms = w.get_schedule(plclub)
        for m in ms:
            print m
        f = raw_input()
        # f = raw_input()
        for d, t, round in w.next_round():
            for match in round:
                match.date = d     # TODO: move this somewhere else
                match.time = datetime.time(18, 00)
                # f = raw_input()
                if show_each_round:
                    t.pretty_print()
                    f = raw_input()
                if create_xml:
                    # t.pretty_print()
                    match.create_temp_xml(db)
                if match.has_club(plclub):
                    mr = players_match(match, t)
                else:
                    mr = match.play_match()
        if not new_season:
            break
        else:
            w.new_schedule()

def players_match(match, t):
    mr = match.play_tactical_match()
    t.pretty_print()
    print match.date, match
    f = raw_input()
    return mr

def create_next_season(templates, oldts):
    """Create the tournaments of the next season.
    
    Given finished tournaments, creates the same tournaments of the next
    season. New tournaments will have no matches played, and the promotions and
    relegations will be handled.
    """
    newts = copy.deepcopy(templates)
    for oldt in oldts:
        print oldt.name
        staying = oldt.get_staying()
        """
        for s in staying:
            print "%s -- %s" % (s, oldt.name)
            """
        newts[oldt.name].add_clubs(staying)
        exchanges = oldt.get_exchanges()
        # print "Tournament %s: %d" % (oldt.name, len(exchanges))
        for target_t, target_s, clubname in exchanges:
            print "%s -> %s" % (clubname, target_t)
            newts[target_t].add_clubs([clubname])

    att_ts = {}
    for newt in newts.values():
        attendances = newt.get_attendances()
        for attendance_t, attendance_s in attendances:
            if attendance_t in att_ts.keys():
                # print "Extending", attendance_t
                att_ts[attendance_t].extend(list(newt.get_clubs()))
            else:
                # print "Having new", attendance_t
                att_ts[attendance_t] = list(newt.get_clubs())
    for attk, attv in att_ts.items():
        newts[attk].add_preliminary_clubs(attv)
        # print "Tournament name: %s; Number of clubs: %d" % (attk, len(attv))
    f = raw_input()
    return newts.values()

def get_startdate(year):
    """Returns 1.9.year."""
    return datetime.date(year, 9, 1)

def get_enddate(year):
    """Returns 18.5.(year + 1)"""
    return datetime.date(year + 1, 5, 18)

if __name__ == '__main__':
    main()

