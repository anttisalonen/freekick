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
    print "Usage: %s tournamentname" % sys.argv[0]

def create_event_schedule(startdate, enddate, tournament):
    num_rounds = 0
    total_rounds = []
    for stage in reversed(tournament.stages):
        total_rounds.append(stage.to_rounds(db))

    for stage in total_rounds:
        num_rounds += len(stage)

    s = Schedule.EventSchedule(startdate, enddate, tournament, num_rounds)
    return s

def get_country_league(cname, tname, db):
    for l in db.countries[cname].leaguesystem.levels:
        for b in l.branches:
            for s in b.stages:
                if s.name == tname:
                    t = Tournament.Tournament(tname)
                    t.stages = [copy.deepcopy(s)]
                    return t
    raise KeyError("Tournament '%s' not found" % tname)

def main():
    if len(sys.argv) == 1:
        print_usage()
        sys.exit(1)

    tournaments = []
    if len(sys.argv) == 2:          # Either DIY or a whole country
        try:
            tournamentname = sys.argv[1]
            tournaments.append(copy.deepcopy(db.tournaments[tournamentname]))
        except KeyError:
            countryname = sys.argv[1]
            tournaments.extend(copy.deepcopy(db.countries[countryname].tournaments.values()))
            for l in db.countries[countryname].leaguesystem.levels:
                for b in l.branches:
                    for s in b.stages:
                        t = Tournament.Tournament(s.name)
                        t.stages = [copy.deepcopy(s)]
                        tournaments.append(t)
    else:                           # Specific tournament from a country
        countryname = sys.argv[1]
        tournamentname = sys.argv[2]
        try:
            tournaments.append(copy.deepcopy(db.countries[countryname].tournaments[tournamentname]))
        except KeyError:
            tournaments.append(get_country_league(countryname, tournamentname, db))

    if len(tournaments) == 0:
        raise KeyError("Tournament/country not found")

    year = 2007
    new_season = len(tournaments) > 1

    schedule = Schedule.Schedule([])

    add_season_to_schedule(schedule, get_startdate(year), get_enddate(year), tournaments)
    tournaments = []

    # f = raw_input()
    for d, t in schedule.next_event():
        round = t.get_next_round()
        for match in round:
            mr = match.play_match()
            # print d, match, "\n"
            # t.pretty_print()
            # f = raw_input()
        cont = t.round_played(db)
        # t.pretty_print()
        # f = raw_input()
        if t.finished() and new_season:
            print "Tournament '%s' finished on %s" % (t.name, d)
            t.pretty_print()
            try:
                newt = copy.deepcopy(db.countries[countryname].tournaments[t.name])
            except KeyError:
                newt = get_country_league(countryname, t.name, db)
            add_season_to_schedule(schedule, get_startdate(d.year), get_enddate(d.year), [newt])
            # print schedule
            f = raw_input()

def get_startdate(year):
    return datetime.date(year, 9, 1)

def get_enddate(year):
    return datetime.date(year + 1, 5, 18)

def add_season_to_schedule(schedule, startdate, enddate, tournaments):
    for t in tournaments:
        e = create_event_schedule(startdate, enddate, t)
        schedule.add_event_schedule(e)

if __name__ == '__main__':
    main()
