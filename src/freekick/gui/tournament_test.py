#!/usr/bin/python
"""Main test application for tournaments."""
import sys
import datetime
import copy

import Database
import Schedule
import Tournament

def print_usage():
    """Prints usage."""
    print "Usage: %s countryname tournamentname" % sys.argv[0]

def get_country_league(cname, tname, dbase):
    """Get a copy of a specific league in a country.
    
    Given country name and league name (stage name), returns a 
    Tournament which has the league as the copy of the given stage.
    Raises KeyError if stage not found."""
    for level in dbase.countries[cname].leaguesystem.levels:
        for branch in level.branches:
            for stage in branch.stages:
                if stage.name == tname:
                    tour = Tournament.Tournament(tname)
                    tour.stages = [copy.deepcopy(stage)]
                    tour.in_league_system = True
                    return tour
    raise KeyError("Tournament '%s' not found" % tname)

def get_tournaments(dbase):
    """Returns a list of tournaments based on command line parameters.
    
    The list consists either of a DIY tournament, tournaments in a country
    or a specific tournament in a country depending on parameters."""
    tournaments = {}
    if len(sys.argv) == 2:          # Either DIY or a whole country
        try:                        # DIY
            tournamentname = sys.argv[1]
            tournaments[tournamentname] = \
                    copy.deepcopy(dbase.tournaments[tournamentname])
        except KeyError:            # whole country
            countryname = sys.argv[1]
            for key, val in dbase.countries[countryname].tournaments.items():
                tournaments[key] = copy.deepcopy(val)
            for league in dbase.countries[countryname].leaguesystem.levels:
                for branch in league.branches:
                    for stage in branch.stages:
                        tour = Tournament.Tournament(stage.name)
                        tour.stages = [copy.deepcopy(stage)]
                        tour.in_league_system = True
                        tournaments[stage.name] = tour
    else:                           # Specific tournament from a country
        countryname = sys.argv[1]
        tournamentname = sys.argv[2]
        try:                        # tournament
            tournaments[tournamentname] = copy.deepcopy(
                    dbase.countries[countryname].tournaments[tournamentname])
        except KeyError:            # league system
            tournaments[tournamentname] = get_country_league(
                    countryname, tournamentname, dbase)
    return tournaments

class World:
    """world holds tournament and schedule data."""
    def __init__(self, tournaments, dbase):
        self.tournaments = tournaments.values()
        self.dbase = dbase
        self.year = 2007
        self.num_finished_seasons = 0
        self.prepare_tournament_templates(tournaments)
        self.schedule = Schedule.Schedule([])
        self.schedule.add_season_to_schedule(get_startdate(self.year),
                get_enddate(self.year), self.tournaments, self.dbase)
        self.finished_tournaments = []

    def prepare_tournament_templates(self, tours):
        """Helper function to prepare tournament templates.
        
        Tournament templates are used to store the persistent tournament
        data, i.e. everything except which clubs are playing in the
        tournament. Call this (internally) when constructing."""
        self.tournament_templates = copy.deepcopy(tours)
        for tour in self.tournament_templates.values():
            tour.clear_clubs()

    def next_round(self):
        """Generates a tuple of date, tournament, next round of the 
        tournament, round being a list of matches.
        
        If the tournament is finished, prints some stuff."""
        for date, tour in self.schedule.next_event():
            yield date, tour, tour.get_next_round()
            tour.round_played(self.dbase) # return value ignored
            if tour.finished():
                print "Tournament '%s' finished on %s" % (tour.name, date)
                tour.pretty_print()
                raw_input()
                self.finished_tournaments.append(tour)

    def get_schedule(self, clubname):
        """Returns the schedule of the given club."""
        ret = []
        for tour in self.tournaments:
            for stage in tour.stages:
                for mround in stage.rounds:
                    for match in mround:
                        if match.has_club(clubname):
                            ret.append(match)
        return ret

    def new_schedule(self):
        """Closes the current season and creates a new one.

        The tournaments should all be finished when calling this. Finally,
        all the new tournaments are output to screen."""
        self.schedule = Schedule.Schedule([])
        self.num_finished_seasons += 1
        self.tournaments = create_next_season(
                self.tournament_templates, self.finished_tournaments)
        self.finished_tournaments = []
        self.schedule.add_season_to_schedule(
                get_startdate(self.year + self.num_finished_seasons),
                get_enddate(self.year + self.num_finished_seasons), 
                self.tournaments, self.dbase)
        for newt in self.tournaments:
            newt.pretty_print()
        raw_input()

class MainApplication:
    """Main interactive application."""
    def __init__(self):
        self.database_path = "../../../share/DB/"
        self.dbase = Database.get_db(self.database_path)

    def run(self):
        """Starts the application."""
        if len(sys.argv) == 1:
            print_usage()
            sys.exit(1)

        tournaments = get_tournaments(self.dbase)
        if len(tournaments) == 0:
            raise KeyError("Tournament/country not found")

        world = World(tournaments, self.dbase)
        create_xml = False
        show_each_round = False
        new_season = len(tournaments) > 1

        plclub = "Sunderland"
    
        while True:
            matches = world.get_schedule(plclub)
            for match in matches:
                print match
            raw_input()
            for date, tour, mround in world.next_round():
                for match in mround:
                    match.date = date     # TODO: move this somewhere else
                    match.time = datetime.time(18, 00)
                    # raw_input()
                    if show_each_round:
                        tour.pretty_print()
                        raw_input()
                    if create_xml:
                        # tour.pretty_print()
                        match.create_temp_xml(self.dbase)
                        raw_input()
                    if match.has_club(plclub):
                        players_match(plclub, match, tour)
                    else:
                        match.play_match()
            if not new_season:
                break
            else:
                world.new_schedule()

def main():
    """Entry point."""
    appl = MainApplication()
    appl.run()

def players_match(plclub, match, tour):
    """Match which includes the player.

    Returns the match result."""
    mres = match.play_tactical_match()
    tour.pretty_print()
    print match.date, match
    raw_input()
    return mres

def handle_exchanges(templates, oldts):
    """Given templates and finished tournaments, promotes and relegates
    clubs.

    Returns list of new tournaments. Also prints stuff."""
    newts = copy.deepcopy(templates)
    for oldt in oldts:
        print oldt.name
        staying = oldt.get_staying()
        newts[oldt.name].add_clubs(staying)
        exchanges = oldt.get_exchanges()
        # print "Tournament %s: %d" % (oldt.name, len(exchanges))
        for target_t, target_s, clubname in exchanges:
            print "%s -> %s" % (clubname, target_t)
            newts[target_t].add_clubs([clubname])
    return newts

def create_next_season(templates, oldts):
    """Create the tournaments of the next season.
    
    Given finished tournaments, creates the same tournaments of the next
    season. New tournaments will have no matches played, and the promotions and
    relegations will be handled.
    """

    newts = handle_exchanges(templates, oldts)

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
    raw_input()
    return newts.values()

def get_startdate(year):
    """Returns 1.9.year."""
    return datetime.date(year, 9, 1)

def get_enddate(year):
    """Returns 18.5.(year + 1)"""
    return datetime.date(year + 1, 5, 18)

if __name__ == '__main__':
    main()

