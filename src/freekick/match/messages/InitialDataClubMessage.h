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


#ifndef FREEKICK_MATCH_MESSAGES_INITIALDATACLUBMESSAGE_H
#define FREEKICK_MATCH_MESSAGES_INITIALDATACLUBMESSAGE_H

#include <string>

#include <boost/shared_ptr.hpp>
#include <boost/array.hpp>
#include <boost/foreach.hpp>

#include "Lineup.h"

#include "SerializationDataMessage.h"

namespace freekick
{
    namespace match
    {
        namespace messages
        {
            class InitialDataClubMessage : public SerializationDataMessage
            {
            public:
            InitialDataClubMessage(const boost::shared_ptr<Club>& c1, 
                                   const boost::shared_ptr<Club>& c2)
                : SerializationDataMessage(initialdata_club_id)
                {
                    clubs[0] = c1;
                    clubs[1] = c2;
                }

            InitialDataClubMessage(std::string& msg)
                : SerializationDataMessage(msg, initialdata_club_id)
                {
                    using namespace boost;
                    regex expr("\"([[:print:]]+?)\" \"([[:print:]]+?)\"(.*)");
                    cmatch what;
                    // std::cerr << "IDCM: phase 1\n";
                    // std::cerr << "Full string: " << msg << std::endl;
                    if(regex_match(msg.c_str(), what, expr))
                    {
                        std::string club1name, club2name;
                        club1name.assign(what[1].first, what[1].second);
                        club2name.assign(what[2].first, what[2].second);
                        clubs[0].reset(new Club(club1name));
                        clubs[1].reset(new Club(club2name));

                        std::string ps;
                        ps.assign(what[3].first, what[3].second);
                        regex expr2(" *\"([[:print:]]+?)\" +([0-9]+?) +([0-9]+?) +([01]) +([0123]) +([01])(.*)");
                        cmatch what2;
                        int plcounter = 0;

                        // std::cerr << "IDCM: phase 2\n";
                        while(regex_match(ps.c_str(), what2, expr2))
                        {
                            // std::cerr << "IDCM: parsing player\n";
                            std::string name;
                            std::string sidn, snum, sclub, spos, ssub;
                            int idnum, number, club, sub;
                            int pos;
                            name.assign (what2[1].first, what2[1].second);
                            sidn.assign (what2[2].first, what2[2].second);
                            snum.assign (what2[3].first, what2[3].second);
                            sclub.assign(what2[4].first, what2[4].second);
                            spos.assign (what2[5].first, what2[5].second);
                            ssub.assign (what2[6].first, what2[6].second);
                            ps.assign   (what2[7].first, what2[7].second);

                            // std::cerr << "1. string: " << name << std::endl;
                            // std::cerr << "2. string: " << sidn << std::endl;
                            // std::cerr << "3. string: " << snum << std::endl;
                            // std::cerr << "4. string: " << sclub << std::endl;
                            // std::cerr << "5. string: " << spos << std::endl;
                            // std::cerr << "6. string: " << ssub << std::endl;
                            // std::cerr << "Next string: " << ps << std::endl;

                            idnum  = atoi(sidn.c_str());
                            number = atoi(snum.c_str());
                            club   = atoi(sclub.c_str());
                            sub    = atoi(ssub.c_str());
                            pos    = atoi(spos.c_str());
                            PlayerPosition ppos = IntToPlayerPosition(pos);
                            boost::shared_ptr<Player> pp(new Player(name, number, idnum, ppos));
                            PlayerInLineup pll;
                            if(sub == 1) pll = Substitute; else pll = Playing;
                            clubs[club]->addPlayer(pp, pll);
                            plcounter++;
                        }
                        std::cerr << "---------------------------------\nSuccessfully parsed " << plcounter << " players.\n-----------------------------\n";
                    }
                    else
                    {
                        // std::cerr << "IDCM: failed\n";
                        throw "InitialDataClubMessage: failed parse";
                    }
                }

                virtual ~InitialDataClubMessage() { }

                const std::string toString () const
                {
                    std::ostringstream oss(std::ostringstream::out);
                    typedef std::vector<boost::shared_ptr<Player> > pllist;
                    boost::array<pllist, 2> pllists;

                    oss << "\"" << clubs[0]->getName() << "\" \"" << clubs[1]->getName() << "\" ";
                    clubs[0]->getPlayers(pllists[0]);
                    clubs[1]->getPlayers(pllists[1]);

                    for(int i = 0; i < 2; i++)
                    {
                        boost::shared_ptr<Lineup> l = clubs[i]->getLineup();
                        BOOST_FOREACH(boost::shared_ptr<Player> pp, pllists[i])
                        {
                            PlayerInLineup st = l->playerInLineup(pp->getID());
                            if(st == NotPlaying)
                                continue;

                            oss << "\"" << pp->getName() << "\" " << pp->getID() << " " << pp->getNumber() << " " << i << " " << pp->getPlayerPosition();
                            if(st == Playing)
                                oss << " 0 ";
                            else
                                oss << " 1 ";
                        }
                    }

                    return serString(oss.str());
                }

                void getClub(bool home, boost::shared_ptr<Club>& c) const
                {
                    if (home) c = clubs[0];
                    else c = clubs[1];
                }

            private:
                boost::array<boost::shared_ptr<Club>, 2> clubs;
            };
        }
    }
}

#endif
