#!/usr/bin/python

import SoccerData
import Stage

class Tournament:
    """Tournament class."""
    def __init__(self, name):
        """Initialize an empty Tournament with given name."""
        self.name = name
        self.stages = []
        self.current_stage = 0
        self.in_league_system = False

    def add_stage(self, stage):
        """Add stage as the last stage to the tournament."""
        self.stages.append(stage)
        self.current_stage = len(self.stages) - 1

    def add_clubs(self, clubs):
        """Add clubs to the tournament.
        
        Clubs are added to the first stage that still has free slots, filling
        the stages as it goes."""
        for s in reversed(self.stages):
            s.feed_club_names(clubs)
            if len(clubs) == 0:
                break

    def add_preliminary_clubs(self, clubs):
        """Add clubs to the tournament.
        
        Clubs are only added to the first stage, and if there are too many
        clubs to add, an extra stage is automatically created with the same
        rules as the previous first stage, where the additional clubs are fit
        into.
        TODO: if number of clubs to add is more than twice the number of 
        clubs that the 'usual' first round would accept, the behavior is
        undefined (will probably crash). So use this only if you are
        adding one preliminary round until this is fixed.
        """
        num_add_clubs = len(clubs)
        num_free_slots = self.stages[-1].setup.participantnum -\
            len(self.stages[-1].club_names)
        if num_add_clubs > num_free_slots:
            diff_clubs = num_add_clubs - num_free_slots
            s = Stage.gen_preliminary_stage("Preliminary round", 
                    self.stages[-1], 
                    diff_clubs * 2)
            self.add_stage(s)
            self.add_clubs(clubs)
        else:
            self.stages[-1].feed_club_names(clubs)

    def get_clubs(self):
        clubs = []
        for s in self.stages:
            clubs.extend(s.club_names)
        return clubs

    def clear_clubs(self):
        """Clear all stages from clubs."""
        for s in self.stages:
            s.clear_clubs()
        self.current_stage = len(self.stages) - 1

    def get_next_round(self):
        """Return next round of the current stage, or an empty list if
        the tournament is finished."""
        if self.current_stage < 0:
            return []
        return self.stages[self.current_stage].get_next_round()

    def round_played(self, db):
        """Make note that the last round was played.
        
        Returns list of triples (tournamentname, stagename, clubname)
        designating clubs that are 'expelled' from this tournament into
        another one. If no clubs are expelled (the usual case, e.g. when the
        current stage is not yet finished), will return an empty list.

        Call this after playing a round.
        """
        if self.current_stage < 0:
            return []
        winners = self.stages[self.current_stage].round_played()
        if not winners:
            return []
        else:
            return self.update_stage(winners, db)

    def update_stage(self, winners, db):
        """Fills stages with winners.
        
        :param winners: list of club names to fill the next stage with.
        :param db: database.
        
        If tournament is finished, returns True, otherwise False.
        Call this function when moving clubs from stage n to stage n + 1.
        """
        if winners:
            self.current_stage -= 1
            if self.current_stage >= 0:
                self.stages[self.current_stage].update_club_names(winners, db)
                return []
            else:
                # TODO: tournament finished, return expelled clubs
                return []
        return []

    def get_attendances(self):
        """Return attendances of the last stage."""
        return self.stages[0].attendances

    def get_promotions(self):
        """Return promotions of the last stage."""
        return self.stages[0].get_promotions()

    def get_relegations(self):
        """Return relegations of the last stage."""
        return self.stages[0].get_relegations()

    def get_exchanges(self):
        return self.get_promotions() + self.get_relegations()

    def get_staying(self):
        """Return staying clubs of the last stage."""
        return self.stages[0].get_staying()

    def finished(self):
        """Return True if the tournament is finished."""
        if len(self.stages) > 1:
            return self.current_stage < 0
        elif len(self.stages) == 0:
            return False
        else:
            return self.stages[self.current_stage].finished()

    def pretty_print(self):
        for stage in reversed(self.stages):
            print "%s:" % stage.name
            stage.pretty_print()

