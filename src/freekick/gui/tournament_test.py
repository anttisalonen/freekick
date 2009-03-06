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

    if len(sys.argv) == 2:
        tournamentname = sys.argv[1]
        tournament = db.tournaments[tournamentname]
    else:
        countryname = sys.argv[1]
        tournamentname = sys.argv[2]
        try:
            tournament = db.countries[countryname].tournaments[tournamentname]
        except KeyError:
            tournament = None
            for l in db.countries[countryname].leaguesystem.levels:
                for b in l.branches:
                    for s in b.stages:
                        if s.name == tournamentname:
                            tournament = Tournament.Tournament(tournamentname)
                            tournament.stages = [s]
            if tournament == None:
                raise KeyError("Tournament '%s' not found" % tournamentname)

    startdate = datetime.date(2008, 8, 1)
    enddate = datetime.date(2008, 9, 18)
    es = create_event_schedule(startdate, enddate, tournament)
    schedule = Schedule.Schedule(es)

    for stage in reversed(tournament.stages):        
        stage.to_rounds(db)

    tournament.pretty_print()
    f = raw_input()
    for d, t in schedule.next_event():
        round = t.get_next_round()
        for match in round:
            mr = match.play_match()
            print d, match, mr, "\n"
            t.pretty_print()
            f = raw_input()
        cont = t.round_played(db)
