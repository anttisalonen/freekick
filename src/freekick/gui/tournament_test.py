#!/usr/bin/python

import sys
import datetime

import Database
import SoccerData
import Schedule

db = SoccerData.DB()

def print_usage():
    print "Usage: %s tournamentname" % sys.argv[0]

def create_event_schedule(startdate, enddate, tournament):
    num_rounds = 0
    total_rounds = []
    for stage in reversed(tournament.stages):
        total_rounds.append(stage.to_rounds())

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

    startdate = datetime.date(2008, 8, 1)
    enddate = datetime.date(2008, 9, 18)
    tournamentname = sys.argv[1]
    tournament = db.tournaments[tournamentname]

    es = create_event_schedule(startdate, enddate, tournament)
    schedule = Schedule.Schedule(es)

    for stage in reversed(tournament.stages):        
        stage.to_rounds()

    tournament.pretty_print()
    f = raw_input()
    for d, t in schedule.next_event():
        print d
        round = t.get_next_round()
        for match in round:
            match.play_random()
        cont = t.round_played()
        if cont == True:
            t.pretty_print()
            f = raw_input()
