#!/usr/bin/python

class Schedule:
    def __init__(self, es):
        self.events = []
        for e in es:
            self.add_event_schedule(e)

    def add_event_schedule(self, es):
        for date in es.dates:
            self.events.append((date, es.tournament))
        self.events.sort()

    def next_event(self):
        for d, t in self.events:
            yield d, t

    def __str__(self):
        ret = ""
        for d, t in self.events:
            ret += d.__str__() + " " + t.name + "\n"
        return ret

class EventSchedule:
    def __init__(self, startdate, enddate, tournament, num_rounds):
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
