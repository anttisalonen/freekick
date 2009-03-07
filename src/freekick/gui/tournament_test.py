#!/usr/bin/python

import sys
import datetime

import Database
import SoccerData
import Schedule
import Tournament

db = SoccerData.DB()

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

if __name__ == '__main__':
    if len(sys.argv) == 1:
        print_usage()
        sys.exit(1)

    database_path = "../../../share/DB/"
    db = Database.get_db(database_path)

    tournaments = []
    if len(sys.argv) == 2:
        try:
            tournamentname = sys.argv[1]
            tournaments.append(db.tournaments[tournamentname])
        except KeyError:
            countryname = sys.argv[1]
            tournaments.extend(db.countries[countryname].tournaments.values())
            for l in db.countries[countryname].leaguesystem.levels:
                for b in l.branches:
                    for s in b.stages:
                        t = Tournament.Tournament(s.name)
                        t.stages = [s]
                        tournaments.append(t)
    else:
        countryname = sys.argv[1]
        tournamentname = sys.argv[2]
        try:
            tournaments.append(db.countries[countryname].tournaments[tournamentname])
        except KeyError:
            for l in db.countries[countryname].leaguesystem.levels:
                for b in l.branches:
                    for s in b.stages:
                        if s.name == tournamentname:
                            t = Tournament.Tournament(tournamentname)
                            t.stages = [s]
                            tournaments.append(t)
            if len(tournaments) == 0:
                raise KeyError("Tournament '%s' not found" % tournamentname)

    if len(tournaments) == 0:
        raise KeyError("Tournament/country not found")

    startdate = datetime.date(2007, 9, 1)
    enddate = datetime.date(2008, 5, 18)
    es = []
    for t in tournaments:
        es.append(create_event_schedule(startdate, enddate, t))
    schedule = Schedule.Schedule(es)

    # f = raw_input()
    for d, t in schedule.next_event():
        round = t.get_next_round()
        for match in round:
            mr = match.play_match()
            # print d, match, "\n"
            # t.pretty_print()
            # f = raw_input()
        cont = t.round_played(db)

    for t in tournaments:
        t.pretty_print()
