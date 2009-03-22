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
    if len(sys.argv) == 1:
        print_usage()
        sys.exit(1)

    tournaments = {}
    if len(sys.argv) == 2:          # Either DIY or a whole country
        try:
            tournamentname = sys.argv[1]
            tournaments[tournamentname] = copy.deepcopy(db.tournaments[tournamentname])
        except KeyError:
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
        try:
            tournaments[tournamentname] = copy.deepcopy(db.countries[countryname].tournaments[tournamentname])
        except KeyError:
            tournaments[tournamentname] = get_country_league(countryname, tournamentname, db)

    if len(tournaments) == 0:
        raise KeyError("Tournament/country not found")

    year = 2007
    new_season = len(tournaments) > 1

    schedule = Schedule.Schedule([])

    add_season_to_schedule(schedule, get_startdate(year), get_enddate(year), tournaments.values())
    tournaments = {}

    # f = raw_input()
    for d, t in schedule.next_event():
        round = t.get_next_round()
        for match in round:
            match.date = d
            match.time = datetime.time(18, 00)
            # match.create_temp_xml(db)
            mr = match.play_match()
            print d, match
            # t.pretty_print()
            # f = raw_input()
        cont = t.round_played(db)
        t.pretty_print()
        f = raw_input()
        if t.finished() and new_season:
            print "Tournament '%s' finished on %s" % (t.name, d)
            t.pretty_print()
            break      # comment this line to start next season (doesn't work)
            if t.name not in tournaments:
                add_future_tournament(t, countryname, schedule, d)
            else:
                update_future_tournament(t, countryname, schedule, d)

def add_moved_clubs(moved_clubs, tgt_stage):
    if tgt_stage in tournaments.keys():
        tgt_tournament = tournaments[tgt_stage]
    else:
        tgt_tournament = copy.deepcopy(get_country_league(coutryname, tgt_stage, db))
        tgt_tournament.clear_clubs()
        add_season_to_schedule(schedule, get_startdate(d.year), get_enddate(d.year), [tgt_tournament])
    for ptour, pstage, pclub in moved_clubs:
        tgt_tournament.add_clubs([pclub])

def add_future_tournament(t, countryname, schedule, date):
    try:
        newt = copy.deepcopy(db.countries[countryname].tournaments[t.name])
    except KeyError:
        newt = copy.deepcopy(get_country_league(countryname, t.name, db))

    if t.in_league_system:
        newt.clear_clubs()

    proms = t.get_promotions()
    if t.in_league_system and len(proms) > 0:
        higher_stage = db.countries[countryname].leaguesystem.get_higher_stage(t.name)
        add_moved_clubs(proms, higher_stage)

    stays = t.get_staying()
    newt.add_clubs(stays)

    rels = t.get_relegations()
    if t.in_league_system and len(rels) > 0:
        lower_stage = db.countries[countryname].leaguesystem.get_lower_stage(t.name)
        add_moved_clubs(proms, lower_stage)

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
    return datetime.date(year, 9, 1)

def get_enddate(year):
    return datetime.date(year + 1, 5, 18)

def add_season_to_schedule(schedule, startdate, enddate, tournaments):
    for t in tournaments:
        e = create_event_schedule(startdate, enddate, t)
        schedule.add_event_schedule(e)

if __name__ == '__main__':
    main()
