#!/usr/bin/env python

import sys
import subprocess
import os
import signal
import time
import functools
import math

from PyQt4 import QtGui, QtCore
from PyQt4 import Qt as qt

import Database

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

class ChooseClubs(QtGui.QWidget):
    def __init__(self, num_needed_clubs, clubs, selected_clubs, continue_handler, title="Clubs", parent = None):
        QtGui.QWidget.__init__(self, parent)

        self.setWindowTitle(title)
        self.num_needed_clubs = num_needed_clubs
        self.continue_handler = continue_handler

        self.forward = QtGui.QPushButton("Continue", self)
        back = QtGui.QPushButton("Back", self)

        self.list = QtGui.QListWidget()
        for clubname in clubs:
            self.list.addItem(clubname)
        self.list.setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
        self.list.sortItems()
        for clubname in selected_clubs:
            print clubname
            self.list.setItemSelected(self.list.findItems(clubname, qt.Qt.MatchExactly)[0], True)
        self.newSelection()

        vbox1 = QtGui.QVBoxLayout()
        vbox1.addWidget(self.list)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(self.forward)
        vbox2.addWidget(back)

        hbox = QtGui.QHBoxLayout()
        hbox.addLayout(vbox1)
        hbox.addLayout(vbox2)

        self.setLayout(hbox)
        self.resize(800, 600)

        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.goForward)
        self.connect(back, QtCore.SIGNAL('clicked()'), self.backClicked)
        self.connect(self.list, QtCore.SIGNAL('itemSelectionChanged()'), self.newSelection)

    def newSelection(self):
        if len(self.list.selectedItems()) == self.num_needed_clubs:
            self.forward.setEnabled(True)
        else:
            self.forward.setEnabled(False)

    def backClicked(self):
        self.continue_handler([str(item.text()) for item in self.list.selectedItems()], False)
        self.close()

    def goForward(self):
        self.continue_handler([str(item.text()) for item in self.list.selectedItems()], True)

class Event:
    def receiveClubsHandler(self, clubs):
        pass

def sorted_dict_values(adict):
    items = adict.items()
    items.sort()
    return [(key, value) for key, value in items]

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
            for club in stages[0].clubs:
                club_item = get_tree_widget_item(club)
                item.addChild(club_item)
        elif len(stages) > 1:
            for stage in stages:
                st_item = get_tree_widget_item(stage.name)
                item.addChild(st_item)
                for club in stage.clubs:
                    club_item = get_tree_widget_item(club)
                    st_item.addChild(club_item)
            
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
        add_to_tree(tree_widget)

        back = QtGui.QPushButton("Back")
        self.forward = QtGui.QPushButton("Continue")

        self.chosen_clubs = QtGui.QListWidget()

        bottom_buttons_box.addWidget(back)
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
            self.chosen_clubs.addItem(widget.text(0))
            self.newSelection()

    def itemDeselected(self, widget):
        self.chosen_clubs.takeItem(self.chosen_clubs.currentRow())
        self.chosen_clubs.clearSelection()
        self.newSelection()

    def newSelection(self):
        if self.chosen_clubs.count() == self.num_clubs_to_choose:
            self.forward.setEnabled(True)
        else:
            self.forward.setEnabled(False)

    def goForward(self):
        names = []
        for index in range(self.chosen_clubs.count()):
            names.append(str(self.chosen_clubs.item(index).text()))
        self.chosen_func(names)


    """
    def __init__(self, num_clubs_to_choose, chosen_func):
        countrynames = db.countries.keys()
        countrynames.sort()
        cc = ChooseX(countrynames, self.countryChosenHandler, "Choose Country", 4)
        cc.show()
        self.wins = [cc]
        self.clubs_selected = set()
        self.chosen_func = chosen_func
        self.num_clubs_to_choose = num_clubs_to_choose

    def countryChosenHandler(self, country_name):
        stages = db.countries[country_name].get_stages()
        num_stages = len(stages)
        if num_stages > 0:
            if num_stages == 1:
                cd = self.get_club_chooser(stages[0].clubs)
            else:
                cd = ChooseX([s.name for s in stages], functools.partial(self.divisionChosenHandler, stages), "Choose Division")
            cd.show()
            self.wins.append(cd)

    def divisionChosenHandler(self, stages, stage_name):
        for stage in stages:
            if stage.name == stage_name:
                cd = self.get_club_chooser(stage.clubs)
                cd.show()
                self.wins.append(cd)

    def get_club_chooser(self, clubs):
        already_here_chosen_clubs = self.clubs_selected.intersection(clubs)
        return ChooseClubs(num_clubs_to_choose - len(self.clubs_selected) + len(already_here_chosen_clubs), clubs, already_here_chosen_clubs, functools.partial(self.receiveClubsHandler, clubs))

    def receiveClubsHandler(self, total_clubs, these_clubs, cont):
        self.clubs_selected = self.clubs_selected - set(total_clubs)
        self.clubs_selected = self.clubs_selected.union(these_clubs)
        if cont:
            self.chosen_func(list(self.clubs_selected))
    """

class Friendly(Event):
    def __init__(self):
        cc = GeneralClubChooser(2, self.gotoLineups)
        cc.show()
        self.wins = [cc]

    def gotoLineups(self, clubs_selected):
        ln = Lineups(clubs_selected[:2])
        ln.show()
        self.wins.append(ln)

class ChooseX(QtGui.QWidget):
    def __init__(self, button_names, continue_handler, title, num_columns = 1, parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle(title)
        self.continue_handler = continue_handler

        back = QtGui.QPushButton("Back", self)
        self.select_buttons = []

        for button_name in button_names:
            self.select_buttons.append(QtGui.QPushButton(button_name, self))

        button_box = QtGui.QGridLayout()

        num_rows = math.ceil(len(self.select_buttons) / float(num_columns))
        for index in range(len(self.select_buttons)):
            button_box.addWidget(self.select_buttons[index], index % num_rows, index / num_rows)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(back)

        topbox = QtGui.QVBoxLayout()
        topbox.addLayout(button_box)
        topbox.addLayout(vbox2)

        self.setLayout(topbox)
        self.resize(800, 600)

        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        for i in range(0, len(self.select_buttons)):
            self.connect(self.select_buttons[i], QtCore.SIGNAL('clicked()'), functools.partial(continue_handler, str(self.select_buttons[i].text())))

class ChooseDivision(QtGui.QWidget):
    def __init__(self, divisions, num_needed_clubs, clubs_handler, title="Divisions", parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle(title)

        self.setWindowTitle(title)
        self.num_needed_clubs = num_needed_clubs
        self.clubs_handler = clubs_handler

        back = QtGui.QPushButton("Back", self)
        self.countrybuttons = []
        self.countrybuttonhandlers = []

        countrynames = db.countries.keys()
        countrynames.sort()

        for countryname in countrynames:
            self.countrybuttons.append(QtGui.QPushButton(countryname, self))
            self.countrybuttonhandlers.append(self.get_button_handler(countryname))

        clubbox = QtGui.QGridLayout()

        num_columns = 4
        num_rows = math.ceil(len(self.countrybuttons) / float(num_columns))
        for index in range(len(self.countrybuttons)):
            clubbox.addWidget(self.countrybuttons[index], index % num_rows, index / num_rows)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(back)

        topbox = QtGui.QVBoxLayout()
        topbox.addLayout(clubbox)
        topbox.addLayout(vbox2)

        self.setLayout(topbox)
        self.resize(800, 600)

        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        for i in range(0, len(self.countrybuttons)):
            self.connect(self.countrybuttons[i], QtCore.SIGNAL('clicked()'), self.countrybuttonhandlers[i])

    def get_button_handler(self, country_label):
        def button_handler(self, country_name):
            stages = db.countries[country_name].get_stages()
            num_stages = len(stages)
            if num_stages > 0:
                if num_stages == 1:
                    cd = ChooseClubs(stages[0].clubs, self.num_needed_clubs, self.clubs_handler)
                else:
                    cd = ChooseDivision(stages, self.num_needed_clubs, self.clubs_handler)
                cd.show()
                self.wins = [cd]
        return functools.partial(button_handler, self, country_label)

        for div in divisions:
            print div.name
            for clubname in div.clubs:
                print clubname

class ChooseCountry(QtGui.QWidget):
    def __init__(self, num_needed_clubs, clubs_handler, title = "Countries", parent = None):
        QtGui.QWidget.__init__(self, parent)
        self.setWindowTitle(title)
        self.num_needed_clubs = num_needed_clubs
        self.clubs_handler = clubs_handler

        back = QtGui.QPushButton("Back", self)
        self.countrybuttons = []
        self.countrybuttonhandlers = []

        countrynames = db.countries.keys()
        countrynames.sort()

        for countryname in countrynames:
            self.countrybuttons.append(QtGui.QPushButton(countryname, self))
            self.countrybuttonhandlers.append(self.get_button_handler(countryname))

        clubbox = QtGui.QGridLayout()

        num_columns = 4
        num_rows = math.ceil(len(self.countrybuttons) / float(num_columns))
        for index in range(len(self.countrybuttons)):
            clubbox.addWidget(self.countrybuttons[index], index % num_rows, index / num_rows)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(back)

        topbox = QtGui.QVBoxLayout()
        topbox.addLayout(clubbox)
        topbox.addLayout(vbox2)

        self.setLayout(topbox)
        self.resize(800, 600)

        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        for i in range(0, len(self.countrybuttons)):
            self.connect(self.countrybuttons[i], QtCore.SIGNAL('clicked()'), self.countrybuttonhandlers[i])

    def get_button_handler(self, country_label):
        def button_handler(self, country_name):
            stages = db.countries[country_name].get_stages()
            num_stages = len(stages)
            if num_stages > 0:
                if num_stages == 1:
                    cd = ChooseClubs(stages[0].clubs, self.num_needed_clubs, self.clubs_handler)
                else:
                    cd = ChooseDivision(stages, self.num_needed_clubs, self.clubs_handler)
                cd.show()
                self.wins = [cd]
        return functools.partial(button_handler, self, country_label)

class ShowClubs(QtGui.QWidget):
    def __init__(self, clubs, num_needed_clubs, forward_handler, title="Clubs", parent=None):
        QtGui.QWidget.__init__(self, parent)

        self.setWindowTitle(title)
        self.num_needed_clubs = num_needed_clubs
        self.forward_handler = forward_handler

        self.forward = QtGui.QPushButton("Continue", self)
        self.forward.setEnabled(False)
        back = QtGui.QPushButton("Back", self)

        self.list = QtGui.QListWidget()
#        for clubname in db.clubs.keys():
        for clubname in clubs:
            self.list.addItem(clubname)
        self.list.setSelectionMode(QtGui.QAbstractItemView.MultiSelection)
        self.list.sortItems()

        vbox1 = QtGui.QVBoxLayout()
        vbox1.addWidget(self.list)

        vbox2 = QtGui.QVBoxLayout()
        vbox2.addWidget(self.forward)
        vbox2.addWidget(back)

        hbox = QtGui.QHBoxLayout()
        hbox.addLayout(vbox1)
        hbox.addLayout(vbox2)

        self.setLayout(hbox)
        self.resize(800, 600)

        self.connect(self.forward, QtCore.SIGNAL('clicked()'), self.doForward)
        self.connect(back, QtCore.SIGNAL('clicked()'), self, QtCore.SLOT('close()'))
        self.connect(self.list, QtCore.SIGNAL('itemSelectionChanged()'), self.newSelection)

    def newSelection(self):
        if len(self.list.selectedItems()) == self.num_needed_clubs:
            self.forward.setEnabled(True)
        else:
            self.forward.setEnabled(False)

    def doForward(self):
        self.forward_handler.receiveClubsHandler(self.list.selectedItems())

class Preset(QtGui.QWidget):
    def __init__(self, parent=None):
        QtGui.QWidget.__init__(self, parent)


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

database_path = "../share/DB/"
db = Database.get_db(database_path)

if __name__ == '__main__':
    app = QtGui.QApplication(sys.argv)
    mm = MainMenu()
    mm.show()
    sys.exit(app.exec_())
