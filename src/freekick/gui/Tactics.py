#!/usr/bin/python
"""This module includes all the classes for formations and tactics.

There's PlayerTactic, PitchTactic, GeneralTactic, Lineup and Formation.

PlayerTactic includes the orders given to one specific player, e.g. position.

PitchTactic simply consists of all the PlayerTactics on the pitch.

GeneralTactic includes the clubwide tactics.

Lineup is simply two lists of player IDs: pitch players and substitutes.

Formation has PitchTactic, GeneralTactic and Lineup."""

from lxml import etree

total_players = 11

class PlayerTactic:
    """PlayerTactic class.

    Each PlayerTactic belongs to a specific player. PlayerTactic includes
    the general position on the pitch for the player as well as offensiveness
    rating. PlayerTactic is generic - it can be saved, loaded and reused."""
    def __init__(self, name = ""):
        self.name = name
        self.pos = 0.5, 0.5
        self.offensive = 0.5

    def to_xml(self):
        root = etree.Element("tactic", name = self.name)
        xcoord, ycoord = self.pos
        posnode = etree.SubElement(root, "pos", x = str(xcoord), y =
                str(ycoord))
        attrnode = etree.SubElement(root, "attributes", offensive =
                str(self.offensive))
        return root

class PitchTactic:
    """PitchTactic is a collection of all the player tactics of the players
    plus name."""
    def __init__(self, name, num_d, num_m, num_f, player_tactics):
        self.name = name
        self.player_tactics = player_tactics
        self.num_d = num_d
        self.num_m = num_m
        self.num_f = num_f

    def to_xml(self):
        root = etree.Element("pitchtactic")
        root.set("name", self.name)
        root.set("defenders", str(self.num_d))
        root.set("midfielders", str(self.num_m))
        root.set("forwards", str(self.num_f))
        for tacname, tac in self.player_tactics:
            root.append(tac.to_xml())
        return root

class GeneralTactic:
    """GeneralTactic class defines club-wide tactics.

    This consists of a few attributes that influence player decisions.
    Note that GeneralTactic is generic - it can be saved, loaded and 
    reused."""
    def __init__(self, long_ball = 0.5, width_attack = 0.5, 
            width_defense = 0.5, counter_attack = 0.5):
        self.long_ball = long_ball
        self.width_attack = width_attack
        self.width_defense = width_defense  
        self.counter_attack = counter_attack

    def __str__(self):
        return "Long balls: %1.3f\nWidth in attack: %1.3f\nWidth in defense: "\
            "%1.3f\nCounter attacks: %1.3f\n" % (self.long_ball, self.width_attack,
                    self.width_defense, self.counter_attack)

    def to_xml(self):
        root = etree.Element("generaltactic")
        lbnode = etree.SubElement(root, "longball", 
                value = str(self.long_ball))
        wanode = etree.SubElement(root, "widthatt", 
                value = str(self.width_attack))
        wdnode = etree.SubElement(root, "widthdef", 
                value = str(self.width_defense))
        canode = etree.SubElement(root, "countatt", 
                value = str(self.counter_attack))
        return root

class Lineup:
    """Lineup class.

    This is what you give to the referee: list of people that are allowed to 
    enter the pitch and the bench. Therefore it is also club specific."""
    def __init__(self):
        """Sets up an empty lineup with no players nor substitutes."""
        self.pitch_players = []
        self.substitutes = []

    def add_player(self, playerid, position):
        if position:
            self.pitch_players.append((playerid, position))
        else:
            self.substitutes.append(playerid)

    def is_valid(self, max_subs):
        return len(self.pitch_players) == total_players and \
               len(self.substitutes) <= max_subs

    def to_xml(self):
        root = etree.Element("lineup")
        for plid, ppos in self.pitch_players:
            etree.SubElement(root, "player", id = str(plid), position = ppos)
        for plid in self.substitutes:
            etree.SubElement(root, "substitute", id = str(plid))
        return root

class Formation:
    """Formation class is the main class a club takes with itself to a match.

    It includes a) the lineup, and b) club tactics. Lineup is the list of
    attending players and 'open' information. Club tactics include all the
    tactical decisions by the club. Formation maps the player specific, 
    generic tactics to the individual players in the lineup. While Lineup is 
    club specific, club tactics are generic.
    """
    def __init__(self, general_tactic, pitch_tactic):
        self.general_tactic = general_tactic
        self.pitch_tactic = pitch_tactic
        self.lineup = Lineup()

    def to_xml(self):
        root = etree.Element("formation")
        root.append(self.general_tactic.to_xml())
        root.append(self.pitch_tactic.to_xml())
        root.append(self.lineup.to_xml())
        return root

