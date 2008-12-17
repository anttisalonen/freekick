/************************************************************************
  This file is part of Freekick.

  Freekick is free software: you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Freekick is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with Freekick.  If not, see <http://www.gnu.org/licenses/>.

  Copyright Antti Salonen, 2008
**************************************************************************/


#ifndef FREEKICK_MATCH_MESSAGES_MESSAGE_H
#define FREEKICK_MATCH_MESSAGES_MESSAGE_H

#include <string>
#include <iostream>
#include <sstream>
#include <set>
#include <cctype>
#include <cstdlib>

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            const static std::string freekick_server_id 	= "FREEKICK_SERVER";
            const static std::string freekick_client_id 	= "FREEKICK_CLIENT";
            const static std::string serialization_delim	= "+++";
            const static unsigned int initialdata_id		= 1;
            const static unsigned int initialdata_club_id	= 100;
            const static unsigned int initialdata_kit_id	= 110;
            const static unsigned int initialdata_player_id	= 120;
            const static unsigned int initialdata_pitch_id	= 130;
            const static unsigned int initialdata_tournament_id	= 140;
            const static unsigned int initialdata_stadium_id	= 160;
            const static unsigned int initialdata_time_id	= 180;
            const static unsigned int initialdata_weather_id	= 190;
            const static unsigned int initial_ball_id		= 200;
            const static std::string msg_start                  = "(";
            const static std::string msg_end                    = ")";
            const static std::string list_start                 = "[";
            const static std::string list_delim                 = ",";
            const static std::string list_end                   = "]";
            const static std::string s_list_of_players          = "Z";
            const static std::string s_username_ack             = "Y";
            const static std::string s_set_gen_upd_ack          = "T";
            const static std::string s_set_const_upd_ack        = "S";
            const static std::string s_gen_pause_upd            = "A";
            const static std::string s_gen_time_upd             = "B";
            const static std::string s_gen_score_upd            = "C";
            const static std::string s_gen_status_upd           = "D";
            const static std::string s_const_upd                = "";
            const static std::string s_gen_err                  = "H";
            const static std::string s_srv_info                 = "I";
            const static std::string s_give_gen_upd_int         = "V";
            const static std::string s_give_const_upd_int       = "U";
            const static std::string c_pl_cont_req              = "z";
            const static std::string c_username_req             = "y";
            const static std::string c_initial_data_req         = "x";
            const static std::string c_gen_upd_req              = "w";
            const static std::string c_pl_ctl_move              = "a";
            const static std::string c_pl_ctl_kick              = "b";
            const static std::string c_pl_ctl_tackle            = "c";
            const static std::string c_pl_ctl_hold              = "d";
            const static std::string c_pl_ctl_head              = "e";
            const static std::string c_pl_ctl_status            = "f";
            const static std::string c_set_gen_upd_int          = "t";
            const static std::string c_set_const_upd_int        = "s";
            const static std::string c_get_gen_upd_int          = "v";
            const static std::string c_get_const_upd_int        = "u";
            typedef int PlayerID;    // also used for e.g. ball -> signed int
            class Message
            {
            public:
                virtual ~Message() { }
                virtual const std::string toString() const = 0;
            };

            std::string setToMessageList(const std::set<PlayerID>& c);
            bool messageListToSet(const std::string& m, std::set<PlayerID>& s);
            bool isValidMessage(const std::string& s);
            const std::string getMessageType(const std::string& s);
        }
    }
}

#endif // FREEKICK_MATCH_MESSAGES_MESSAGE_H
