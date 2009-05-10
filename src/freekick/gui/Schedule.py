#!/usr/bin/python

class Schedule:
    """Schedule class that holds a list of all the events (date + match)."""
    def __init__(self, es):
        """Construct Schedule.

        :param es: event schedule.
        """
        self.events = []
        for e in es:
            self.add_event_schedule(e)

    def add_event_schedule(self, es):
        """Add event schedule (es) to the schedule."""
        for date in es.dates:
            self.events.append((date, es.tournament))
        self.events.sort()

    def next_event(self):
        """Yield next event in the schedule."""
        for d, t in self.events:
            yield d, t

    def add_season_to_schedule(self, startdate, enddate, tournaments, db):
        """Adds all tournaments to the schedule.

        :param schedule: Schedule to be extended.
        :param startdate: startdate of the tournaments.
        :param enddate: enddate of the tournaments.
        :param tournaments: list of tournaments to add.
        """
        for t in tournaments:
            e = create_event_schedule(startdate, enddate, t, db)
            self.add_event_schedule(e)

        def __str__(self):
            ret = ""
            for d, t in self.events:
                ret += d.__str__() + " " + t.name + "\n"
            return ret

def create_event_schedule(startdate, enddate, tournament, db):
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

    s = EventSchedule(startdate, enddate, tournament, num_rounds)
    return s

class EventSchedule:
    """EventSchedule class that creates the dates 
    fitting to the tournament."""
    def __init__(self, startdate, enddate, tournament, num_rounds):
        """Construct EventSchedule.

        Given start- and enddate, the tournament and number of rounds,
        creates the match dates with correct intervals."""
        self.start_date = startdate
        self.end_date = enddate
        self.tournament = tournament
        self.total_rounds = num_rounds
        self.match_interval = (enddate - startdate) // (self.total_rounds - 1)
        self.dates = []
        for i in range(self.total_rounds):
            rounddate = self.start_date + self.match_interval * i
            self.dates.append(rounddate)

    def __str__(self):
        ret = ""
        for date in self.dates:
            ret += date.__str__() + "\n"
        return ret
