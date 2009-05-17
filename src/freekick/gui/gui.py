#!/usr/bin/env python

import sys
import subprocess
import os
import signal
import time
import functools
import math
import copy

from PyQt4 import QtGui, QtCore
from PyQt4 import Qt as qt

from Primitives import sorted_dict_values
import Database
import SoccerData
import Stage
import Tournament

class Lineups(QtGui.QWidget):
    def __init__(self, clubnames, parent=None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle('Lineups')
        self.back = QtGui.QPushButton("Go Back", self)
        self.forward = QtGui.QPushButton("Play", self)

        clubs = []
        clubs.append(db.clubs[clubnames[0]])
        clubs.append(db.clubs[clubnames[1]])
        self.lineups = []
        for i in range(2):
            self.lineups.append(QtGui.QListWidget())
            self.lineups[i].setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
            for plnum in clubs[i].contracts:
                pl = db.players[plnum]
                clubs[i].players = pl
                self.lineups[i].addItem("%s" % pl)

        hbox1 = QtGui.QHBoxLayout()
        hbox1.addWidget(self.lineups[0])
        hbox1.addWidget(self.lineups[1])

        hbox2 = QtGui.QHBoxLayout()
        hbox2.addWidget(self.back)
        hbox2.addWidget(self.forward)

        vbox = QtGui.QVBoxLayout()
        vbox.addLayout(hbox1)
        vbox.addLayout(hbox2)

        self.setLayout(vbox)
        self.resize(800, 600)

        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.start)
        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))

    def start(self):
        cwd = os.getcwd()
        srv = subprocess.Popen(os.path.join(cwd, "fkserver"))
        time.sleep(1)
        cli = subprocess.Popen(os.path.join(cwd, "launch_client.sh"))
        time.sleep(0.1)
        aic = subprocess.Popen(os.path.join(cwd, "aiclient"))
        cli.communicate()
        time.sleep(0.5)
        os.kill(aic.pid, signal.SIGTERM)
        time.sleep(0.5)
        os.kill(srv.pid, signal.SIGTERM)
        self.close()

def get_tree_widget_item(text):
    item = QtGui.QTreeWidgetItem()
    item.setText(0, text)
    return item

def add_to_tree(widget):
    sorted_countries = sorted_dict_values(db.countries)
    for countryname, country in sorted_countries:
        item = get_tree_widget_item(countryname)
        widget.addTopLevelItem(item)
        stages = country.get_stages()
        if len(stages) == 1:
            for club in stages[0].club_names:
                club_item = get_tree_widget_item(club)
                item.addChild(club_item)
        elif len(stages) > 1:
            for stage in stages:
                st_item = get_tree_widget_item(stage.name)
                item.addChild(st_item)
                for club in stage.club_names:
                    club_item = get_tree_widget_item(club)
                    st_item.addChild(club_item)

class GeneralChooser(QtGui.QWidget):
    def __init__(self, options, chosen_func, parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle('Please choose')
        self.chosen_func = chosen_func

        highest_box = QtGui.QVBoxLayout()
        bottom_buttons_box = QtGui.QHBoxLayout()
        top_choosing_box = QtGui.QHBoxLayout()

        back = QtGui.QPushButton("Back")

        self.options = QtGui.QListWidget()
        for opt in options:
            self.options.addItem(opt)

        bottom_buttons_box.addWidget(back)
        top_choosing_box.addWidget(self.options)

        highest_box.addLayout(top_choosing_box)
        highest_box.addLayout(bottom_buttons_box)

        self.setLayout(highest_box)
        self.resize(800, 600)

        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        self.connect(self.options, QtCore.SIGNAL('itemClicked(QListWidgetItem *)'), self.goForward)

    def goForward(self):
        self.chosen_func(str(self.options.item(0).text()))

class GeneralClubChooser(QtGui.QWidget):
    def __init__(self, num_clubs_to_choose, chosen_func, parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle('Choose clubs')
        self.num_clubs_to_choose = num_clubs_to_choose
        self.chosen_func = chosen_func

        highest_box = QtGui.QVBoxLayout()
        bottom_buttons_box = QtGui.QHBoxLayout()
        top_choosing_box = QtGui.QHBoxLayout()

        tree_widget = QtGui.QTreeWidget()
        tree_widget.setUniformRowHeights(True)
        tree_widget.setHeaderLabel("Clubs")
        add_to_tree(tree_widget)

        back = QtGui.QPushButton("Back")
        self.forward = QtGui.QPushButton("Continue")
        self.num_label = QtGui.QLabel()

        self.chosen_clubs = QtGui.QListWidget()

        bottom_buttons_box.addWidget(back)
        bottom_buttons_box.addWidget(self.num_label)
        bottom_buttons_box.addWidget(self.forward)
        top_choosing_box.addWidget(tree_widget)
        top_choosing_box.addWidget(self.chosen_clubs)

        highest_box.addLayout(top_choosing_box)
        highest_box.addLayout(bottom_buttons_box)

        self.newSelection()
        self.setLayout(highest_box)
        self.resize(800, 600)

        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        self.connect(tree_widget, QtCore.SIGNAL('itemClicked(QTreeWidgetItem *, int)'), self.itemSelected)
        self.connect(self.chosen_clubs, QtCore.SIGNAL('itemClicked(QListWidgetItem *)'), self.itemDeselected)
        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.goForward)

    def itemSelected(self, widget, column):
        if widget.childCount() == 0:
            if len(self.chosen_clubs.findItems(widget.text(0), qt.Qt.MatchExactly)) == 0:
                self.chosen_clubs.addItem(widget.text(0))
                self.newSelection()

    def itemDeselected(self, widget):
        self.chosen_clubs.takeItem(self.chosen_clubs.currentRow())
        self.chosen_clubs.clearSelection()
        self.newSelection()

    def newSelection(self):
        num_choose = self.num_clubs_to_choose - self.chosen_clubs.count()
        if num_choose > 0:
            self.num_label.setText("Choose %d more clubs" % num_choose)
        elif num_choose < 0:
            self.num_label.setText("Choose %d clubs less" % -num_choose)
        else:
            self.num_label.setText("")
        if self.chosen_clubs.count() == self.num_clubs_to_choose:
            self.forward.setEnabled(True)
        else:
            self.forward.setEnabled(False)

    def goForward(self):
        names = []
        for index in range(self.chosen_clubs.count()):
            names.append(str(self.chosen_clubs.item(index).text()))
        self.chosen_func(names)

class Friendly:
    def __init__(self):
        cc = GeneralClubChooser(2, self.gotoLineups)
        cc.show()
        self.wins = [cc]

    def gotoLineups(self, clubs_selected):
        ln = Lineups(clubs_selected[:2])
        ln.show()
        self.wins.append(ln)

class Preset(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

class Season(QtGui.QWidget):
    def __init__(self, parent=None):
        countries = []
        sorted_countries = sorted_dict_values(db.countries)
        for countryname, country in sorted_countries:
            countries.append(countryname)
        cc = GeneralChooser(countries, self.start_season)
        cc.show()
        self.wins = [cc]

    def start_season(self, chosen_country):
        s = db.countries[chosen_country].leaguesystem.levels[0].branches[0].stages[0]
        t = Tournament.Tournament(s.name)
        t.stages = [copy.deepcopy(s)]
        t.in_league_system = True
        tournament_screen = TournamentScreen(t, chosen_country)
        tournament_screen.show()
        self.wins.append(tournament_screen)

class DIY(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.max_clubs = 128
        self.max_num_stages = 12

        self.setWindowTitle('DIY')

        self.forward = QtGui.QPushButton("Choose clubs", self)
        self.forward.setEnabled(False)
        self.back = QtGui.QPushButton("Back", self)
        self.bottom_hbox = QtGui.QHBoxLayout()
        self.bottom_hbox.addWidget(self.back)
        self.bottom_hbox.addWidget(self.forward)

        self.stageboxes = []
        self.additional_clubs_boxes = []
        self.num_clubs_boxes = []
        self.stage_type_boxes = []
        for stage in range(self.max_num_stages):
            self.stageboxes.append(QtGui.QHBoxLayout())
            self.num_clubs_boxes.append(QtGui.QSpinBox())
            self.stage_type_boxes.append(QtGui.QComboBox())
            self.additional_clubs_boxes.append(QtGui.QSpinBox())
            self.stageboxes[stage].addWidget(self.additional_clubs_boxes[stage])
            self.stageboxes[stage].addWidget(self.num_clubs_boxes[stage])
            self.stageboxes[stage].addWidget(self.stage_type_boxes[stage])

        # defaults
        self.num_clubs_boxes[0].setValue(32)
        self.num_clubs_boxes[1].setValue(16)
        self.num_clubs_boxes[2].setValue(8)
        self.num_clubs_boxes[3].setValue(4)
        self.num_clubs_boxes[4].setValue(2)

        for stage in range(5, len(self.num_clubs_boxes)):
            self.num_clubs_boxes[stage].setValue(1)

        self.num_clubs_boxes[0].setRange(self.num_clubs_boxes[1].value() + 1, self.max_clubs)

        vbox = QtGui.QVBoxLayout()
        for n in range(self.max_num_stages):
            vbox.addLayout(self.stageboxes[n])
        vbox.addLayout(self.bottom_hbox)

        self.setLayout(vbox)
        self.resize(800, 600)

        for stage in range(self.max_num_stages):
            self.connect(self.num_clubs_boxes[stage], QtCore.SIGNAL('valueChanged(int)'), self.update_stages)
            self.connect(self.additional_clubs_boxes[stage], QtCore.SIGNAL('valueChanged(int)'), self.update_stages)
        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.go_forward)

        self.update_stages()

    def update_stages(self):
        for stage in range(0, self.max_num_stages):
            if stage == 0:
                max_num = self.max_clubs
            else:
                max_num = self.num_clubs_boxes[stage - 1].value() + self.additional_clubs_boxes[stage - 1].value() - 1
            if stage == (self.max_num_stages - 1):
                min_num = 1
            else:
                min_num = self.num_clubs_boxes[stage + 1].value() + 1 - self.additional_clubs_boxes[stage].value()
                if min_num == 2:
                    min_num = 1
            if max_num - min_num < 0:
                self.num_clubs_boxes[stage].setRange(1, 1)
                self.num_clubs_boxes[stage].setEnabled(False)
            else:
                self.num_clubs_boxes[stage].setRange(min_num, max_num)
                self.num_clubs_boxes[stage].setEnabled(True)

            self.additional_clubs_boxes[stage].setRange(0, self.max_clubs - self.num_clubs_boxes[stage].value())

            total_clubs_here = self.num_clubs_boxes[stage].value() + self.additional_clubs_boxes[stage].value()
            if stage == (self.max_num_stages - 1):
                self.set_correct_stage_types(self.stage_type_boxes[stage], total_clubs_here, 1)
            else:
                self.set_correct_stage_types(self.stage_type_boxes[stage], total_clubs_here, self.num_clubs_boxes[stage + 1].value())
            everything_ok = all([b.count() > 0 or (not b.isEnabled()) for b in self.stage_type_boxes])
            self.forward.setEnabled(everything_ok)

    def set_correct_stage_types(self, combo, num_clubs, num_clubs_next):
        combo.clear()
        if num_clubs <= 1:
            combo.setEnabled(False)
            return
        else:
            combo.setEnabled(True)
        if num_clubs_next * 2 == num_clubs:
            combo.addItem("Knockout")
        for n in range(24, 2, -1):
            if num_clubs % n == 0:
                num_clubs_per_group = n
                num_groups = num_clubs // n
                if num_groups > 16:
                    continue
                if num_groups > 1:
                    combo.addItem("%d groups of %d" % (num_groups, num_clubs_per_group))
                else:
                    combo.addItem("%d group of %d" % (num_groups, num_clubs_per_group))

    def go_forward(self):
        total_clubs = self.num_clubs_boxes[0].value()
        for box in self.additional_clubs_boxes:
            total_clubs += box.value()
        t_stages = []
        for stage in range(self.max_num_stages):
            if self.num_clubs_boxes[stage].value() < 1:
                break
            s_type = Stage.StageType.League
            if str(self.stage_type_boxes[stage].currentText()) == "Knockout":
                s_type = Stage.StageType.Cup
            new_stage = Stage.Stage(SoccerData.stage_number_to_stage_name(stage, self.max_num_stages), s_type)
            new_stage.setup.participantnum = self.additional_clubs_boxes[stage].value() + self.num_clubs_boxes[stage].value()
            t_stages.append(new_stage)
        self.tournament = Tournament.Tournament("DIY Tournament")
        self.tournament.stages = t_stages
        cc = GeneralClubChooser(total_clubs, self.start_tournament)
        cc.show()
        self.wins = [cc]

    def start_tournament(self, chosen_clubs):
        tournament_screen = TournamentScreen(self.tournament, chosen_clubs)
        tournament_screen.show()
        self.wins += [tournament_screen]

class TournamentScreen(QtGui.QWidget):
    def __init__(self, tournament, additional_clubs = [], parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.tournament = tournament
        tournament.add_clubs(additional_clubs)

        self.highest_box = QtGui.QVBoxLayout()
        self.bottom_buttons_box = QtGui.QHBoxLayout()

        self.this_stage = tournament.stages[0]
        if self.this_stage.type == Stage.StageType.Cup:
            self.matches_box = QtGui.QHBoxLayout()
            self.left_matches_box = QtGui.QVBoxLayout()
            self.right_matches_box = QtGui.QVBoxLayout()
            index = 0
            for club in self.this_stage.club_names:
                label = QtGui.QLabel(club)
                if index % 2 == 0:
                    self.left_matches_box.addWidget(label)
                else:
                    self.right_matches_box.addWidget(label)
                index += 1
            self.matches_box.addLayout(self.left_matches_box)
            self.matches_box.addLayout(self.right_matches_box)
            self.highest_box.addLayout(self.matches_box)

        self.back = QtGui.QPushButton("Back", self)
        self.bottom_buttons_box.addWidget(self.back)
        self.highest_box.addLayout(self.bottom_buttons_box)

        self.setLayout(self.highest_box)
        self.resize(800, 600)

        self.connect(self.back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))

class MainMenu(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.wins = []
        self.setWindowTitle('Freekick menu')

        clubeditor = QtGui.QPushButton("Club Editor")
        playereditor = QtGui.QPushButton("Player Editor")
        orgeditor = QtGui.QPushButton("Organization Editor")
        pitcheditor = QtGui.QPushButton("Pitch Editor")
        options = QtGui.QPushButton("Options")
        quit = QtGui.QPushButton("Quit", self)
        friendly = QtGui.QPushButton("Friendly")
        diy = QtGui.QPushButton("DIY Tournament")
        preset = QtGui.QPushButton("Preset Tournament")
        season = QtGui.QPushButton("Preset Season")
        career = QtGui.QPushButton("Career")
        loadgame = QtGui.QPushButton("Load Game")

        vbox1 = QtGui.QVBoxLayout()
        vbox1.addWidget(clubeditor)
        vbox1.addWidget(playereditor)
        vbox1.addWidget(orgeditor)
        vbox1.addWidget(pitcheditor)
        vbox1.addWidget(options)
        vbox1.addWidget(quit)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(friendly)
        vbox2.addWidget(diy)
        vbox2.addWidget(preset)
        vbox2.addWidget(season)
        vbox2.addWidget(career)
        vbox2.addWidget(loadgame)

        hbox = QtGui.QHBoxLayout()
        hbox.addLayout(vbox1)
        hbox.addLayout(vbox2)

        self.setLayout(hbox)
        self.resize(800, 600)

        self.connect(friendly, QtCore.SIGNAL('clicked()'), self.doFriendly)
        self.connect(diy, QtCore.SIGNAL('clicked()'), self.doDIY)
        self.connect(preset, QtCore.SIGNAL('clicked()'), self.doPreset)
        self.connect(season, QtCore.SIGNAL('clicked()'), self.doSeason)
        self.connect(quit, QtCore.SIGNAL('clicked()'), QtGui.qApp, QtCore.SLOT('quit()'))

    def doFriendly(self):
        self.fr = Friendly()

    def doDIY(self):
        wn = DIY()
        wn.show()
        self.wins += [wn]

    def doPreset(self):
        wn = Preset()
        wn.show()
        self.wins += [wn]

    def doSeason(self):
        wn = Season()
        wn.show()
        self.wins += [wn]

db = SoccerData.DB()

if __name__ == '__main__':
    database_path = "../../../share/DB/"
    db = Database.get_db(database_path)
    app = QtGui.QApplication(sys.argv)
    mm = MainMenu()
    mm.show()
    sys.exit(app.exec_())
