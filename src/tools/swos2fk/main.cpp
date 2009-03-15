/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU Affero General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/

/**
  TODO:
  1. SWOS mode not finished, needs e.g. player skills
*/

#include <iostream>
#include <cstdlib>
#include <cctype>
#include <fstream>
#include <vector>
#include <cstring>
#include <string>
#include <map>
#include <cmath>

#include "addutil/XML.h"
#include "addutil/General.h"

using namespace std;

void usage(const char* prog_name)
{
    cerr << prog_name << " -s           input_file country_name output_file\n";
    cerr << prog_name << "    first_pid input_file country_name country_adjective\n";
    cerr << "                           club_output_file players_output_file\n";
    cerr << "                           country_output_file\n";
    cerr << "-s: SWOS mode (no additional Freekick tags included)\n";
    cerr << "first_pid: player IDs will be given starting from this number.\n";
    cerr << "input_file: SWOS team data file\n";
    cerr << "Will convert a SWOS team data file into a Freekick club file, country file and players file.\n";
}

static const int team_size = 684;
static const int player_size = 38;
static const int player_name_len = 23;

typedef struct
{
    int id;
    int nationality;
    int number;
    char player_name[player_name_len];
    /* unsigned char multi_purpose; */
    int field_position;
    int head_type;
    int passing;
    int shooting;
    int heading;
    int tackling;
    int ball_control;
    int speed;
    int finishing;
    int value;
} s_player;

typedef struct
{
    int type;
    int first_color;
    int second_color;
    int short_color;
    int socks_color;
} s_kit;

static const int team_name_len = 19;
static const int coach_name_len = 25;
static const int player_position_in_file_len = 15;
static const int number_of_players = 16;

typedef struct
{
    int nation;
    int team_number;
    int swos_team_number;
    char team_name[team_name_len];
    int tactics;
    int division;
    s_kit primary_kit;
    s_kit secondary_kit;
    char coach_name[coach_name_len];
    char player_position_in_file[player_position_in_file_len];
    s_player players[number_of_players];
    int value;
} s_team;

typedef vector<s_team> team_list;

void correct_name(char* n)
{
    char* iter = n;
    while(*iter != 0)
    {
        if(iter != n)
        {
            if (*(iter - 1) != ' ')
            {
                *iter = tolower(*iter);
            }
        }
        iter++;
    }
}

int rand_personality_value()
{
    return addutil::general::rand_normal_distribution(0, 1000);
}

const char* nationality_to_string(int nat)
{
    switch(nat)
    {
        case 0: return "Albania";
        case 1: return "Austria";
        case 2: return "Belgium";
        case 3: return "Bulgaria";
        case 4: return "Croatia";
        case 5: return "Cyprus";
        case 6: return "Czech Republic";
        case 7: return "Denmark";
        case 8: return "England";
        case 9: return "Estonia";
        case 10: return "Faroe Islands";
        case 11: return "Finland";
        case 12: return "France";
        case 13: return "Germany";
        case 14: return "Greece";
        case 15: return "Hungary";
        case 16: return "Iceland";
        case 17: return "Israel";
        case 18: return "Italy";
        case 19: return "Latvia";
        case 20: return "Lithuania";
        case 21: return "Luxembourg";
        case 22: return "Malta";
        case 23: return "The Netherlands";
        case 24: return "Northern Ireland";
        case 25: return "Norway";
        case 26: return "Poland";
        case 27: return "Portugal";
        case 28: return "Romania";
        case 29: return "Russia";
        case 30: return "San Marino";
        case 31: return "Scotland";
        case 32: return "Slovenia";
        case 33: return "Sweden";
        case 34: return "Turkey";
        case 35: return "Ukraine";
        case 36: return "Wales";
        case 37: return "Serbia";  // check for montenegrin players
        case 38: return "Belarus";
        case 39: return "Slovakia";
        case 40: return "Spain";
        case 41: return "Armenia";
        case 42: return "Bosnia-Herzegovina";
        case 43: return "Azerbaijan";
        case 44: return "Georgia";
        case 45: return "Switzerland";
        case 46: return "Ireland";
        case 47: return "FYR Macedonia";
        case 48: return "Turkmenistan";
        case 49: return "Liechtenstein";
        case 50: return "Moldova";
        case 51: return "Costa Rica";
        case 52: return "El Salvador";
        case 53: return "Guatemala";
        case 54: return "Honduras";
        case 55: return "Bahamas";
        case 56: return "Mexico";
        case 57: return "Panama";
        case 58: return "U.S.A.";
        case 59: return "Bahrain";
        case 60: return "Nicaragua";
        case 61: return "Bermuda";
        case 62: return "Jamaica";
        case 63: return "Trinidad and Tobago";
        case 64: return "Canada";
        case 65: return "Barbados";
        case 66: return "El Salvador";
        case 67: return "Saint Vincent and the Grenadines";
        case 68: return "Argentina";
        case 69: return "Bolivia";
        case 70: return "Brazil";
        case 71: return "Chile";
        case 72: return "Colombia";
        case 73: return "Ecuador";
        case 74: return "Paraguay";
        case 75: return "Surinam";
        case 76: return "Uruguay";
        case 77: return "Venezuela";
        case 78: return "Guyana";
        case 79: return "Peru";
        case 80: return "Algeria";
        case 81: return "South Africa";
        case 82: return "Botswana";
        case 83: return "Burkina Faso";
        case 84: return "Burundi";
        case 85: return "Lesotho";
        case 86: return "Congo";
        case 87: return "Zambia";
        case 88: return "Ghana";
        case 89: return "Senegal";
        case 90: return "Ivory Coast";
        case 91: return "Tunisia";
        case 92: return "Mali";
        case 93: return "Madagascar";
        case 94: return "Cameroon";
        case 95: return "Chad";
        case 96: return "Uganda";
        case 97: return "Liberia";
        case 98: return "Mozambique";
        case 99: return "Kenia";
        case 100: return "Sudan";
        case 101: return "Swaziland";
        case 102: return "Angola";
        case 103: return "Togo";
        case 104: return "Zimbabwe";
        case 105: return "Egypt";
        case 106: return "Tanzania";
        case 107: return "Nigeria";
        case 108: return "Ethiopia";
        case 109: return "Gabon";
        case 110: return "Sierra Leone";
        case 111: return "Benin";
        case 112: return "Congo";
        case 113: return "Guinea";
        case 114: return "Sri Lanka";
        case 115: return "Morocco";
        case 116: return "Gambia";
        case 117: return "Malawi";
        case 118: return "Japan";
        case 119: return "Taiwan";
        case 120: return "India";
        case 121: return "Bangladesh";
        case 122: return "Brunei";
        case 123: return "Iraq";
        case 124: return "Jordan";
        case 125: return "Sri Lanka";
        case 126: return "Syria";
        case 127: return "South Korea";
        case 128: return "Iran";
        case 129: return "Vietnam";
        case 130: return "Malaysia";
        case 131: return "Saudi Arabia";
        case 132: return "Yemen";
        case 133: return "Kuwait";
        case 134: return "Laos";
        case 135: return "North Korea";
        case 136: return "Oman";
        case 137: return "Pakistan";
        case 138: return "Philippines";
        case 139: return "China";
        case 140: return "Singapore";
        case 141: return "Mauritius";
        case 142: return "Burma";
        case 143: return "Papua New Guinea";
        case 144: return "Thailand";
        case 145: return "Uzbekistan";
        case 146: return "Qatar";
        case 147: return "United Arab Emirates";
        case 148: return "Australia";
        case 149: return "New Zealand";
        case 150: return "Fiji";
        case 151: return "Solomon Islands";
        case 152: default: cerr << "Unknown nationality " << nat << " found.\n"; return "Unknown";
    }
}

void swos_position_to_freekick_num(int n, int& pos, int& left, int& wing)
{
    int ran_left = (addutil::general::rand_float() < 0.2f) ? 1 : 0;
    switch(n)
    {
        case 0: // gk
            pos = 0;
            left = ran_left;
            wing = 0;
            break;
        case 1: // rb
            pos = 1;
            left = 0;
            wing = 1;
            break;
        case 2: // lb
            pos = 1;
            left = 1;
            wing = 1;
            break;
        case 3: // d
            pos = 1;
            left = ran_left;
            wing = 0;
            break;
        case 4: // rw
            pos = 2;
            left = 0;
            wing = 1;
            break;
        case 5: // lw
            pos = 2;
            left = 1;
            wing = 1;
            break;
        case 6: // m
            pos = 2;
            left = ran_left;
            wing = 0;
            break;
        case 7: // f
            pos = 3;
            left = ran_left;
            wing = 0;
        default:
            break;
    }
}

int head_type_to_appearance(int h)
{
    switch(h)
    {
        case 1: // white skin, blonde hair
            return 8;
        case 2: // black skin, black hair
            return 1;
        case 0: // white skin, black hair
            return 0;
        default:
            cerr << "Unknown head type: " << h << endl;
            return 0;
    }
}

void color_variate(int& val, int max_var, int min_val, int max_val)
{
    int orig_val = val;
    do
    {
        int diff = rand() % (2 * max_var) - max_var;
        val = orig_val + diff;
    } while (val < min_val || val > max_val);
}

void color_std_variate(int& val)
{
    color_variate(val, 10, 0, 255);
}

void add_physical_traits(xmlNodePtr node, int head_type)
{
    int sr, sg, sb, hr, hg, hb, er, eg, eb;
    switch(head_type)
    {
        case 1:   // white skin, blonde hair
            sr = 239;
            sg = 190;
            sb = 160;
            hr = 252;
            hg = 211;
            hb = 142;
            er = 173;
            eg = 216;
            eb = 230;
            break;
        case 2:   // black skin, black hair
            sr = 100;
            sg = 50;
            sb = 0;
            hr = 20;
            hg = 10;
            hb = 0;
            er = 150;
            eg = 75;
            eb = 0;
            break;
        case 0:   // white skin, black hair
        default:
            sr = 239;
            sg = 190;
            sb = 160;
            hr = 20;
            hg = 10;
            hb = 0;
            er = 150;
            eg = 75;
            eb = 0;
            break;
    }
    color_std_variate(sr); color_std_variate(sg); color_std_variate(sb);
    color_std_variate(hr); color_std_variate(hg); color_std_variate(hb);
    color_std_variate(er); color_std_variate(eg); color_std_variate(eb);
    xmlNodePtr skin, hair, eyes;
    skin = addutil::xml::add_child(node, "skin");
    addutil::xml::add_attribute(skin, "r", sr); addutil::xml::add_attribute(skin, "g", sg); addutil::xml::add_attribute(skin, "b", sb);
    hair = addutil::xml::add_child(node, "hair");
    addutil::xml::add_attribute(hair, "r", hr); addutil::xml::add_attribute(hair, "g", hg); addutil::xml::add_attribute(hair, "b", hb);
    eyes = addutil::xml::add_child(node, "eyes");
    addutil::xml::add_attribute(eyes, "r", er); addutil::xml::add_attribute(eyes, "g", eg); addutil::xml::add_attribute(eyes, "b", eb);
}

string team_name_to_stadium_name(const char* team_name)
{
    return string(team_name) + " Stadium";
}

int parse_player(const unsigned char* player_block, int pid, s_player* player)
{
    const unsigned char* iter = player_block;
    player->id = pid;
    player->nationality = *iter++;
    int pos_1 = *iter++;
    if(pos_1 != 0)
    {
        cerr << "parse_player: pos_1 failed: " << pos_1 << ".\n";
        return 1;
    }
    player->number = *iter++;
    memcpy(&player->player_name, (const void*)iter, player_name_len); iter += player_name_len;
    correct_name(player->player_name);
    // cerr << player->player_name << endl;
    unsigned char multi_purpose = *iter++;
    player->field_position = multi_purpose >> 5;
    player->head_type      = (multi_purpose >> 3) & 0x03;

    unsigned char skill_byte = *iter++;
    player->passing      = skill_byte & 0x07;
    skill_byte = *iter++;
    player->shooting     = skill_byte >> 4;
    player->heading      = skill_byte & 0x07;
    skill_byte = *iter++;
    player->tackling     = skill_byte >> 4;
    player->ball_control = skill_byte & 0x07;
    skill_byte = *iter++;
    player->speed        = skill_byte >> 4;
    player->finishing    = skill_byte & 0x07;
    iter++;
    player->value = *iter++;
    // cout << player->value << endl;
    // cout << player->tackling << endl;
    // cout << player->finishing << endl;
    return 0;
}

int parse_kit(const unsigned char* kit_block, s_kit* kit)
{
    const unsigned char* iter = kit_block;
    kit->type = *iter++;
    kit->first_color = *iter++;
    kit->second_color = *iter++;
    kit->short_color = *iter++;
    kit->socks_color = *iter++;
    return 0;
}

int parse_team(const unsigned char* team_block, int first_pid, s_team* team)
{
    int retval = 0;
    int pid = first_pid;
    const unsigned char* iter = team_block;
    team->nation = *iter++;
    team->team_number = *iter++;
    memcpy(&team->swos_team_number, (const void*)iter, 2); iter += 2;
    int pos_4 = *iter++;
    if(pos_4 != 0)
    {
        cerr << "parse_team: failed.\n";
        return 1;
    }
    memcpy(&team->team_name, (const void*)iter, team_name_len); iter += team_name_len;
    correct_name(team->team_name);
    // cerr << team->team_name << endl;
    team->tactics = *iter++;
    team->division = *iter++;
    for(int i = 0; i < 2; i++)
    {
        if(!i)
            retval = parse_kit(iter, &team->primary_kit);
        else
            retval = parse_kit(iter, &team->secondary_kit);
        if(retval != 0)
        {
            cerr << "parse_team: failed kit parse.\n";
            return 1;
        }
        iter += 5;
    }
    memcpy(&team->coach_name, (const void*)iter, coach_name_len); iter += coach_name_len;
    correct_name(team->coach_name);
    // cerr << team->coach_name << endl;
    memcpy(&team->player_position_in_file, (const void*)iter, player_position_in_file_len); iter += player_position_in_file_len;
    team->value = 0;
    for(int pnum = 0; pnum < number_of_players; pnum++)
    {
        retval = parse_player(iter, pid, &team->players[pnum]);
        if(retval != 0)
        {
            cerr << "parse_team: failed player parse.\n";
            return 1;
        }
        iter += player_size;
        pid++;
        team->value += team->players[pnum].value;
    }
    // cerr << team->value << endl;
    return 0;
}

void parse_input_file(istream& in, int first_pid, team_list& teams)
{
    unsigned char c;
    int num_teams;
    int curr_pid = first_pid;

    c = in.get();
    if(c != 0)
    {
        cerr << "General: position 0 failed.\n";
        goto cleanup;
    }

    c = in.get();
    num_teams = c;
    cerr << num_teams << " teams to parse.\n";

    for(int i = 0; i < num_teams; i++)
    {
        unsigned char teamblock[team_size];
        in.read((char*)teamblock, team_size);
        if(in.gcount() != team_size)
        {
            cerr << "General: team " << (i + 1) << " could not be parsed: EOF.\n";
            break;
        }
        s_team team;
        // cerr << "Parsing team " << (i + 1) << endl;
        int retval = parse_team(teamblock, curr_pid, &team);
        if(retval != 0)
        {
            cerr << "General: team " << (i + 1) << " failed.\n";
            break;
        }
        // cerr << "Parsed team " << (i + 1) << endl;
        curr_pid += number_of_players;
        teams.push_back(team);
    }

cleanup:
    return;
}

void create_swos_xml(const team_list& teams, const char* swos_out_filename)
{
    xmlDocPtr doc = NULL;       /* document pointer */
    xmlNodePtr root_node = NULL;
    xmlNodePtr sub_node, sub_node2 = NULL;

    doc = xmlNewDoc(BAD_CAST "1.0");
    root_node = xmlNewNode(NULL, BAD_CAST "Clubs");
    xmlDocSetRootElement(doc, root_node);
    int num_teams = teams.size();
    for(int i = 0; i < num_teams; i++)
    {
        sub_node = addutil::xml::add_child(root_node, "players");
        for (int j = 0; j < number_of_players; j++)
        {
            sub_node2 = addutil::xml::add_child(sub_node, "player");
            addutil::xml::add_attribute(sub_node2, "name", teams[i].players[j].player_name);
            addutil::xml::add_attribute(sub_node2, "position", teams[i].players[j].field_position);
        }
    }

    xmlSaveFormatFileEnc(swos_out_filename, doc, "UTF-8", 1);
    xmlFreeDoc(doc);
}

void create_freekick_player_xml(const team_list& teams, const char* player_out_filename)
{
    xmlDocPtr doc = NULL;       /* document pointer */
    xmlNodePtr root_node = NULL;
    xmlNodePtr sub_node = NULL;
    xmlNodePtr player_node = NULL; /* node pointers */
    xmlNodePtr personal_node = NULL;

    doc = xmlNewDoc(BAD_CAST "1.0");
    root_node = xmlNewNode(NULL, BAD_CAST "Players");
    xmlDocSetRootElement(doc, root_node);
    int num_teams = teams.size();
    for(int i = 0; i < num_teams; i++)
    {
        for (int j = 0; j < number_of_players; j++)
        {
            player_node = addutil::xml::add_child(root_node, "player");
            addutil::xml::add_attribute(player_node, "id", teams[i].players[j].id);
            personal_node = addutil::xml::add_child(player_node, "personal");
            addutil::xml::add_attribute(personal_node, "name", teams[i].players[j].player_name);

/*
            sub_node = addutil::xml::add_child(personal_node, "birth");
            addutil::xml::add_attribute(sub_node, "year", "1980");
            addutil::xml::add_attribute(sub_node, "month", "1");
            addutil::xml::add_attribute(sub_node, "day", "1");
            add_physical_traits(personal_node, teams[i].players[j].head_type);
            sub_node = addutil::xml::add_child(personal_node, "height");
            addutil::xml::add_attribute(sub_node, "value", "178");
*/
            sub_node = addutil::xml::add_child(personal_node, "appearance");
            addutil::xml::add_attribute(sub_node, "value", head_type_to_appearance(teams[i].players[j].head_type));
            sub_node = addutil::xml::add_child(personal_node, "nationality");
            addutil::xml::add_attribute(sub_node, "value", nationality_to_string(teams[i].players[j].nationality));

            sub_node = addutil::xml::add_child(player_node, "personality");
            addutil::xml::add_attribute(sub_node, "active", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "risktaking", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "offensive", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "aggressive", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "consistent", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "creative", rand_personality_value());
            addutil::xml::add_attribute(sub_node, "experienced", rand_personality_value());

            sub_node = addutil::xml::add_child(player_node, "skills");
            // 1. 0 <= r < 200
            /* 2. price defines general level: 50 price levels
               e.g. price level  0: all skills 200 < s < 400
                    price level  1: all skills  10 < s < 210
                    ...
                    price level 50: all skills 700 < s < 900
               all skills = all except goalkeeping
             */
            /* 3. position adds rand(0..200) to skills
               pos/skill sta dex spe tac pas sho con acc goa hea tot
               gk        -   -   -   -   -   -   -   -   -   -   -
               back      50  50  0   50  50  0   50  0   -   0   250
               def       0   50  0   100 50  0   50  0   -   0   250
               wing      50  50  50  0   50  0   50  0   -   0   250
               midf      50  50  0   50  50  0   50  0   -   0   250
               att       0   0   0   0   0   100 0   100 -   50  250
             */
            // 4. goalkeeping is set down for field players
            // 5. all other skills are set down for goalkeepers

            int abs_min = 200 + teams[i].players[j].value * 10;
            static const int std_price_level_range = 200;
            static const int field_player_goalkeeping_divisor = 6;
            int sta = abs_min * 0.5f;
            int dex = abs_min * 0.5f;
            int spe = abs_min * 0.5f;
            int tac = abs_min + addutil::general::rand_int(std_price_level_range);
            int pas = abs_min + addutil::general::rand_int(std_price_level_range);
            int sho = abs_min + addutil::general::rand_int(std_price_level_range);
            int con = abs_min + addutil::general::rand_int(std_price_level_range);
            int acc = abs_min + addutil::general::rand_int(std_price_level_range);
            int goa = abs_min + addutil::general::rand_int(std_price_level_range);
            int hea = abs_min + addutil::general::rand_int(std_price_level_range);

            sta += addutil::general::rand_int(550);
            dex += addutil::general::rand_int(550);
            spe += addutil::general::rand_int(550);
            switch(teams[i].players[j].field_position)
            {
                case 0:  // gk
                    tac *= 0.5f;
                    con *= 0.8f;
                    acc *= 0.5f;
                    hea *= 0.5f;
                    goa += 50 + addutil::general::rand_int(50);
                    break;
                case 1: case 2: // back
                    sta += addutil::general::rand_int(50);
                    dex += addutil::general::rand_int(50);
                    tac += addutil::general::rand_int(50);
                    pas += addutil::general::rand_int(50);
                    con += addutil::general::rand_int(50);
                    goa /= field_player_goalkeeping_divisor;
                    break;
                case 3:  // def
                    dex += addutil::general::rand_int(50);
                    tac += addutil::general::rand_int(100);
                    pas += addutil::general::rand_int(50);
                    con += addutil::general::rand_int(50);
                    goa /= field_player_goalkeeping_divisor;
                    break;
                case 4: case 5: // wing
                    sta += addutil::general::rand_int(50);
                    dex += addutil::general::rand_int(50);
                    spe += addutil::general::rand_int(50);
                    pas += addutil::general::rand_int(50);
                    con += addutil::general::rand_int(50);
                    goa /= field_player_goalkeeping_divisor;
                    break;
                case 6:  // mf
                    sta += addutil::general::rand_int(50);
                    dex += addutil::general::rand_int(50);
                    tac += addutil::general::rand_int(50);
                    pas += addutil::general::rand_int(50);
                    con += addutil::general::rand_int(50);
                    goa /= field_player_goalkeeping_divisor;
                    break;
                case 7: default: // forw
                    sho += addutil::general::rand_int(100);
                    acc += addutil::general::rand_int(100);
                    hea += addutil::general::rand_int(50);
                    goa /= field_player_goalkeeping_divisor;
                    break;
            }
            addutil::xml::add_attribute(sub_node, "stamina", sta);
            addutil::xml::add_attribute(sub_node, "dexterity", dex);
            addutil::xml::add_attribute(sub_node, "speed", spe);
            addutil::xml::add_attribute(sub_node, "tackling", tac);
            addutil::xml::add_attribute(sub_node, "passing", pas);
            addutil::xml::add_attribute(sub_node, "shooting", sho);
            addutil::xml::add_attribute(sub_node, "control", con);
            addutil::xml::add_attribute(sub_node, "accuracy", acc);
            addutil::xml::add_attribute(sub_node, "goalkeeping", goa);
            addutil::xml::add_attribute(sub_node, "heading", hea);

            sub_node = addutil::xml::add_child(player_node, "position");
            int pos, left, wing;
            swos_position_to_freekick_num(teams[i].players[j].field_position, pos, left, wing);
            addutil::xml::add_attribute(sub_node, "pos", pos);
            addutil::xml::add_attribute(sub_node, "left", left);
            addutil::xml::add_attribute(sub_node, "wing", wing);
        }
    }
    xmlSaveFormatFileEnc(player_out_filename, doc, "UTF-8", 1);
    xmlFreeDoc(doc);
}

void create_freekick_club_xml(const team_list& teams, const char* country_name, const char* club_out_filename)
{
    xmlDocPtr doc = NULL;       /* document pointer */
    xmlNodePtr root_node = NULL;
    xmlNodePtr team_node = NULL;
    xmlNodePtr sub_node = NULL;
    xmlNodePtr sub_node2, sub_node3 = NULL;

    doc = xmlNewDoc(BAD_CAST "1.0");
    root_node = xmlNewNode(NULL, BAD_CAST "Clubs");
    xmlDocSetRootElement(doc, root_node);

    int num_teams = teams.size();
    for(int i = 0; i < num_teams; i++)
    {
        team_node = addutil::xml::add_child(root_node, "club");
        addutil::xml::add_attribute(team_node, "name", teams[i].team_name);
        sub_node = addutil::xml::add_child(team_node, "coach");
        addutil::xml::add_attribute(sub_node, "name", teams[i].coach_name);
        sub_node = addutil::xml::add_child(team_node, "kits");
        for(int j = 0; j < 2; j++)
        {
            const s_kit* kit = (j == 0) ? &teams[i].primary_kit : &teams[i].secondary_kit;
            sub_node2 = addutil::xml::add_child(sub_node, "kit");
                sub_node3 = addutil::xml::add_child(sub_node2, "jersey");
                    addutil::xml::add_attribute(sub_node3, "type", kit->type);
                    addutil::xml::color_to_xml(sub_node3, kit->first_color, "color");
                    addutil::xml::color_to_xml(sub_node3, kit->second_color, "color");
                sub_node3 = addutil::xml::add_child(sub_node2, "shorts");
                addutil::xml::color_to_xml(sub_node3, kit->short_color, "color");
                sub_node3 = addutil::xml::add_child(sub_node2, "socks");
                addutil::xml::color_to_xml(sub_node3, kit->socks_color, "color");
        }
        sub_node = addutil::xml::add_child(team_node, "country");
        addutil::xml::add_attribute(sub_node, "name", country_name);

/*
        sub_node = addutil::xml::add_child(team_node, "region");
        addutil::xml::add_attribute(sub_node, "name", country_name);
*/

        sub_node = addutil::xml::add_child(team_node, "stadium");
        addutil::xml::add_attribute(sub_node, "name", team_name_to_stadium_name(teams[i].team_name).c_str());
        sub_node = addutil::xml::add_child(team_node, "contracts");
        for (int j = 0; j < number_of_players; j++)
        {
            sub_node2 = addutil::xml::add_child(sub_node, "contract");
            addutil::xml::add_attribute(sub_node2, "player", teams[i].players[j].id);
        }
    }
    xmlSaveFormatFileEnc(club_out_filename, doc, "UTF-8", 1);

    xmlFreeDoc(doc);

    return;
}

const char* stage_number_to_stage_name(int s, int num_cup_stages)
{
    if(s < 1 || num_cup_stages < 1) return "";
    if(s < 4)
    {
        switch(s)
        {
            case 1:  return "Final";
            case 2:  return "Semifinals";
            default: return "Quarterfinals";
        }
    }

    if(s == num_cup_stages)
        return "First round";
    else if (s == num_cup_stages - 1)
        return "Second round";
    else if (s == num_cup_stages - 2)
        return "Third round";
    else if (s == num_cup_stages - 3)
        return "Fourth round";
    else if (s == num_cup_stages - 4)
        return "Fifth round";
    else if (s == num_cup_stages - 5)
        return "Sixth round";
    else if (s == num_cup_stages - 6)
        return "Seventh round";
    else if (s == num_cup_stages - 7)
        return "Eighth round";
    return "";
}

void create_freekick_country_xml(const team_list& teams, const char* country_name, const char* country_adjective, const char* country_out_filename)
{
    xmlDocPtr doc = NULL;       /* document pointer */
    xmlNodePtr root_node = NULL;
    xmlNodePtr country_node = NULL;
    xmlNodePtr regions_node, region_node, stadium_node = NULL;
    xmlNodePtr league_system_node, levels_node = NULL;
    int num_divisions = 4;
    int clubs_in_divisions[num_divisions];
    vector<const s_team*> divisions[num_divisions];

    int i;
    for(i = 0; i < num_divisions; i++)
        clubs_in_divisions[i] = 0;

    doc = xmlNewDoc(BAD_CAST "1.0");
    root_node = xmlNewNode(NULL, BAD_CAST "Countries");
    xmlDocSetRootElement(doc, root_node);
    country_node = addutil::xml::add_child(root_node, "Country");
    addutil::xml::add_attribute(country_node, "name", country_name);
    regions_node = addutil::xml::add_child(country_node, "Regions");
    region_node = addutil::xml::add_child(regions_node, "Region");
    addutil::xml::add_attribute(region_node, "name", country_name);
    league_system_node = addutil::xml::add_child(country_node, "leaguesystem");
    addutil::xml::add_attribute(league_system_node, "name", (string(country_adjective) + " league").c_str());
    levels_node = addutil::xml::add_child(league_system_node, "Levels");

    team_list::const_iterator it;
    for(it = teams.begin(); it != teams.end(); it++)
    {
        stadium_node = addutil::xml::add_child(region_node, "stadium");
        addutil::xml::add_attribute(stadium_node, "name", team_name_to_stadium_name(it->team_name).c_str());
        int capacity = pow(it->value, 1.7) / 100;
        if(capacity >= 4)
            capacity = capacity + addutil::general::rand_int(capacity / 2) - (capacity / 4);
        addutil::xml::add_attribute(stadium_node, "capacity", capacity * 100);
        if(it->division < num_divisions)
        {
            clubs_in_divisions[it->division]++;
            divisions[it->division].push_back(&(*it));
        }
    }

    int num_teams = teams.size();
    int num_cup_stages = 0;

    for(i = 8; i > 1; i--)
    {
        if(num_teams >= pow(2, i))
        {
            num_cup_stages = i;
            break;
        }
    }
    string tournament_name = string(country_adjective) + " Cup";
    if(num_cup_stages > 0)
    {
        int s;
        xmlNodePtr tournaments, tournament, t_stage, t_setup, t_trophy, t_cuppr = NULL;
        tournaments = addutil::xml::add_child(country_node, "Tournaments");
        tournament = addutil::xml::add_child(tournaments, "tournament");
        addutil::xml::add_attribute(tournament, "name", tournament_name.c_str());
        for(s = 1; s <= num_cup_stages; s++)
        {
            t_stage = addutil::xml::add_child(tournament, "stage");
            const char* stage_name = stage_number_to_stage_name(s, num_cup_stages);
            if(strlen(stage_name) < 1)
                break;
            addutil::xml::add_attribute(t_stage, "name", stage_name);
            addutil::xml::add_attribute(t_stage, "type", "1");

            t_setup = addutil::xml::add_child(t_stage, "setup");
            addutil::xml::add_attribute(t_setup, "seeded", "0");
            addutil::xml::add_attribute(t_setup, "participantnum", pow(2, s));
            addutil::xml::add_attribute(t_setup, "rounds", "1");
            addutil::xml::add_attribute(t_setup, "extratime", "1");
            addutil::xml::add_attribute(t_setup, "penalties", "1");
            addutil::xml::add_attribute(t_setup, "replays", "0");
            addutil::xml::add_attribute(t_setup, "awaygoals", "0");

            if(s == 1)
            {
                t_trophy = addutil::xml::add_child(t_stage, "trophy");
                addutil::xml::add_attribute(t_trophy, "name", (string(country_adjective) + " Cup champion").c_str());
            }
            else
            {
                t_cuppr = addutil::xml::add_child(t_stage, "cuppr");
                addutil::xml::add_attribute(t_cuppr, "stage", stage_number_to_stage_name(s - 1, num_cup_stages));
            }

            if(s == num_cup_stages)
            {
                int added_clubs = 0;
                int total_num_clubs = pow(2, num_cup_stages);

                xmlNodePtr t_preset, t_club = NULL;
                vector<const s_team*>::const_iterator it2;
                t_preset = addutil::xml::add_child(t_stage, "preset");

                for(i = 0; i < num_divisions && added_clubs < total_num_clubs; i++)
                {
                    for(it2 = divisions[i].begin(); it2 != divisions[i].end() && added_clubs < total_num_clubs; it2++)
                    {
                        added_clubs++;
                        t_club = addutil::xml::add_child(t_preset, "club");
                        addutil::xml::add_attribute(t_club, "name", (*it2)->team_name);
                    }
                }
            }
        }
    }
    const char* bottom_tournament_stage = stage_number_to_stage_name(num_cup_stages, num_cup_stages);

    for(i = 0; i < num_divisions; i++)
    {
        if(clubs_in_divisions[i] <= 0)
            continue;
        xmlNodePtr level_node, branch_node, stage_node, setup_node = NULL;
        xmlNodePtr leagueprs, leaguepr, leaguerls, leaguerl, attendances, attendance, trophy = NULL;
        level_node = addutil::xml::add_child(levels_node, "level");
        branch_node = addutil::xml::add_child(level_node, "branch");
        stage_node = addutil::xml::add_child(branch_node, "stage");
        const char* stage_name;
        switch(i)
        {
            case 0:  stage_name = "Premier League"; break;
            case 1:  stage_name = "First League"; break;
            case 2:  stage_name = "Second League"; break;
            default: stage_name = "Third League"; break;
        }
        addutil::xml::add_attribute(stage_node, "name", stage_name);
        addutil::xml::add_attribute(stage_node, "type", "0");
        setup_node = addutil::xml::add_child(stage_node, "setup");
        addutil::xml::add_attribute(setup_node, "seeded", "0");
        addutil::xml::add_attribute(setup_node, "participantnum", clubs_in_divisions[i]);
        addutil::xml::add_attribute(setup_node, "groups", "1");
        addutil::xml::add_attribute(setup_node, "rounds", "2");
        addutil::xml::add_attribute(setup_node, "pointsperwin", "3");
        trophy = addutil::xml::add_child(stage_node, "trophy");
        addutil::xml::add_attribute(trophy, "name", (string(country_adjective) + " " + stage_name + " champion").c_str());

        if(i > 0)
        {
            leaguepr = addutil::xml::add_child(stage_node, "leaguepr");
            addutil::xml::add_attribute(leaguepr, "num", "3");
        }
        if(i < num_divisions - 1 && clubs_in_divisions[i + 1] > 0)
        {
            leaguerl = addutil::xml::add_child(stage_node, "leaguerl");
            addutil::xml::add_attribute(leaguerl, "num", "3");
        }
        attendances = addutil::xml::add_child(stage_node, "attendances");
        if(strlen(bottom_tournament_stage) > 0)
        {
            attendance = addutil::xml::add_child(attendances, "attendance");
            addutil::xml::add_attribute(attendance, "tournament", tournament_name.c_str());
            addutil::xml::add_attribute(attendance, "stage", bottom_tournament_stage);
        }

        xmlNodePtr preset, preset_club = NULL;
        vector<const s_team*>::const_iterator it2;
        preset = addutil::xml::add_child(stage_node, "preset");

        for(it2 = divisions[i].begin(); it2 != divisions[i].end(); it2++)
        {
            preset_club = addutil::xml::add_child(preset, "club");
            addutil::xml::add_attribute(preset_club, "name", (*it2)->team_name);
        }
    }

    xmlSaveFormatFileEnc(country_out_filename, doc, "UTF-8", 1);

    xmlFreeDoc(doc);

    return;
}

int main(int argc, char** argv)
{
    LIBXML_TEST_VERSION;
    srand(time(NULL));

    bool swos_mode = false;
    int argstart = 1;
    char c;

    const char* input_filename = argv[2];

    while((c = getopt(argc, argv, "hs")) != -1)
    {
        switch(c)
        {
            case 's':
                swos_mode = true;
                argstart++;
                break;
            case 'h':
            case '?':
            default:
                usage(argv[0]);
                exit(1);
        }
    }

    if((!swos_mode && argc < 8) || (swos_mode && argc < 5))
    {
        usage(argv[0]);
        exit(1);
    }

    int first_pid;
    if(!swos_mode)
    {
        first_pid = atoi(argv[argstart]);
        if(first_pid <= 0)
        {
            cerr << "First PID has to be greater than 0.\n";
            usage(argv[0]);
            exit(1);
        }
    }
    else first_pid = 1;

    ifstream input_file(input_filename);
    if(!input_file.is_open())
    {
        cerr << "Error: could not open file " << argv[argstart] << std::endl;
        exit(2);
    }

    team_list teams;

    parse_input_file(input_file, first_pid, teams);
    input_file.close();

    char* country_name = argv[3];
    if(swos_mode)
    {
        char* swos_out_filename = argv[4];
        create_swos_xml(teams, swos_out_filename);
    }
    else
    {
        char* country_adjective = argv[4];
        char* club_out_filename = argv[5];
        char* player_out_filename = argv[6];
        char* country_out_filename = argv[7];
        create_freekick_player_xml(teams, player_out_filename);
        create_freekick_club_xml(teams, country_name, club_out_filename);
        create_freekick_country_xml(teams, country_name, country_adjective, country_out_filename);
    }

    xmlCleanupParser();
}
