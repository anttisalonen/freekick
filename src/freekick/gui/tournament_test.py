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

def create_event_schedule(startdate, enddate, tournament):
    """Create event schedule from start- and enddates and the tournament.
    
    Returns EventSchedule."""
    num_rounds = 0
    total_rounds = []
    for stage in reversed(tournament.stages):
        rounds = stage.to_rounds(db)
        for round in rounds:
            for match in round:
                match.tournament_name = tournament.name
        total_rounds.append(rounds)

    for stage in total_rounds:
        num_rounds += len(stage)

    s = Schedule.EventSchedule(startdate, enddate, tournament, num_rounds)
    return s

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

def main():
    """Entry point."""
    if len(sys.argv) == 1:
        print_usage()
        sys.exit(1)

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

    if len(tournaments) == 0:
        raise KeyError("Tournament/country not found")

    year = 2007
    new_season = len(tournaments) > 1
    create_xml = False
    show_each_round = False

    tournament_templates = copy.deepcopy(tournaments)
    for t in tournament_templates.values():
        t.clear_clubs()

    schedule = Schedule.Schedule([])

    add_season_to_schedule(schedule, get_startdate(year), get_enddate(year), tournaments.values())
    tournaments = {}

    while True:
        finished_tournaments = []
        # f = raw_input()
        for d, t in schedule.next_event():
            round = t.get_next_round()
            for match in round:
                match.date = d
                match.time = datetime.time(18, 00)
                # f = raw_input()
                if create_xml:
                    t.pretty_print()
                    match.create_temp_xml(db)
                mr = match.play_match()
                # print d, match
            expelled_clubs = t.round_played(db)
            if show_each_round:
                t.pretty_print()
                f = raw_input()
            if t.finished():
                print "Tournament '%s' finished on %s" % (t.name, d)
                t.pretty_print()
                f = raw_input()
                finished_tournaments.append(t)
        if not new_season:
            break
        else:
            schedule = Schedule.Schedule([])
            new_tournaments = create_next_season(tournament_templates, finished_tournaments)
            add_season_to_schedule(schedule, get_startdate(year),
                    get_enddate(year), new_tournaments)
            for newt in new_tournaments:
                newt.pretty_print()
            f = raw_input()

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
        for s in staying:
            print "%s -- %s" % (s, oldt.name)
        newts[oldt.name].add_clubs(staying)
        exchanges = oldt.get_exchanges()
        print "Tournament %s: %d" % (oldt.name, len(exchanges))
        for target_t, target_s, clubname in exchanges:
            print "%s -> %s" % (clubname, target_t)
            newts[target_t].add_clubs([clubname])
        f = raw_input()

    for newt in newts.values():
        attendances = newt.get_attendances()
        for attendance_t, attendance_s in attendances:
            newts[attendance_t].add_clubs(newt.get_clubs())
            print "%s %s %s %d" % (newt.name, attendance_t, attendance_s,
                    len(newt.get_clubs()))
    return newts.values()

def add_moved_clubs(moved_clubs, tgt_stage):
    """Adds moved (exchanged) clubs to the target stage.

    Doesn't work."""
    if tgt_stage in tournaments.keys():
        tgt_tournament = tournaments[tgt_stage]
    else:
        tgt_tournament = copy.deepcopy(get_country_league(coutryname, tgt_stage, db))
        tgt_tournament.clear_clubs()
        add_season_to_schedule(schedule, get_startdate(d.year), get_enddate(d.year), [tgt_tournament])
    for ptour, pstage, pclub in moved_clubs:
        tgt_tournament.add_clubs([pclub])

def level_to_stage(level):
    """Returns the name of the first stage of the first branch of the given
    level."""
    return level.branches[0].stages[0].name

def add_future_tournament(t, countryname, schedule, date):
    """Turns a finished tournament into the following tournaments.
    
    :param t: finished tournament
    :param countryname: name of the country of the tournament
    :param schedule: schedule where future tournaments will be added
    :param date: finishing date
    TODO: this function obviously has something wrong (just look at the
    needed parameters). Needs to be fixed.
    """
    try:
        newt = copy.deepcopy(db.countries[countryname].tournaments[t.name])
    except KeyError:
        newt = copy.deepcopy(get_country_league(countryname, t.name, db))

    if t.in_league_system:
        newt.clear_clubs()

    proms = t.get_promotions()
    if t.in_league_system and len(proms) > 0:
        higher_level = db.countries[countryname].leaguesystem.get_higher_level(t.name)
        add_moved_clubs(proms, level_to_stage(higher_level))

    stays = t.get_staying()
    newt.add_clubs(stays)

    rels = t.get_relegations()
    if t.in_league_system and len(rels) > 0:
        lower_level = db.countries[countryname].leaguesystem.get_lower_level(t.name)
        add_moved_clubs(proms, level_to_stage(lower_level))

    att = newt.get_attendances()
    if len(att) > 0:
        """ 
        TODO: add new tournament and to it all the clubs of this new tournament, also the clubs that will be
        added to it in the future(!)
        """
        pass    

    add_season_to_schedule(schedule, get_startdate(date.year), get_enddate(date.year), [newt])
    # print schedule
    f = raw_input()

def get_startdate(year):
    """Returns 1.9.year."""
    return datetime.date(year, 9, 1)

def get_enddate(year):
    """Returns 18.5.(year + 1)"""
    return datetime.date(year + 1, 5, 18)

def add_season_to_schedule(schedule, startdate, enddate, tournaments):
    """Adds all tournaments to the schedule.

    :param schedule: Schedule to be extended.
    :param startdate: startdate of the tournaments.
    :param enddate: enddate of the tournaments.
    :param tournaments: list of tournaments to add.
    """
    for t in tournaments:
        e = create_event_schedule(startdate, enddate, t)
        schedule.add_event_schedule(e)

if __name__ == '__main__':
    main()
