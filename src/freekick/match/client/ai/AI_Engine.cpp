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

#include "AI_Engine.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                AI_Engine::AI_Engine(boost::shared_ptr<MatchStatus> st, Network* netw)
                    : mMatchStatus(st),
                      mNetwork(netw),
                      aiplayers_locked(false)
                {
                    boost::array<std::vector<int>, 4> pls;
                    std::set<int> allplids;
                    pls[0] = mMatchStatus->getMatchData()->getHomeClub()->getLineup()->getPitchPlayerIDs();
                    pls[1] = mMatchStatus->getMatchData()->getHomeClub()->getLineup()->getSubstituteIDs();
                    pls[2] = mMatchStatus->getMatchData()->getAwayClub()->getLineup()->getPitchPlayerIDs();
                    pls[3] = mMatchStatus->getMatchData()->getAwayClub()->getLineup()->getSubstituteIDs();
                    clubais[0].reset(new ClubAI(Home, mMatchStatus));
                    clubais[1].reset(new ClubAI(Away, mMatchStatus));
                    for(int s = 0; s < 4; s++)
                    {
                        BOOST_FOREACH(int i, pls[s])
                        {
                            int club_i = (s < 2) ? 0 : 1;
                            aiplayers.push_back(AIPlayer(mMatchStatus, mMatchStatus->getPlayer(i), clubais[club_i]));
                            allplids.insert(i);
                        }
                    }
                    mNetwork->sendMessage(messages::PlayerControlRequestMessage(allplids));
                }

                void AI_Engine::run()
                {
                    using namespace boost::posix_time;
                    unsigned long sleep_time = 1000000/20;

                    std::vector<ClubAI>::iterator club_it;
                    std::vector<AIPlayer>::iterator it;
                    std::vector<boost::shared_ptr<messages::Message> > msgs;
                    std::cerr << "AI_Engine running\n";
                    while(1)
                    {
                        msgs.clear();
                        ptime before_time(microsec_clock::local_time());
                        for(int i = 0; i < 2; i++)
                        {
                            clubais[i]->updateAll();
                        }
                        for(it = aiplayers.begin(); it != aiplayers.end(); it++)
                        {
                            if(aiplayers_locked)
                            {
                                std::cout << "AI_Engine::run: pause in acting due to locked player list\n";
                                break;
                            }
                            try
                            {
                                boost::shared_ptr<messages::Message> m = it->act();
                                msgs.push_back(m);
                            }
                            catch(const char* e)
                            {
                                std::cerr << "AI_Engine::run: Error when acting: " << e << std::endl;
                                // No action from AI -> ignore
                            }
                        }
                        mNetwork->sendMessages(msgs);

                        ptime after_time(microsec_clock::local_time());

                        time_period diff_time(before_time, after_time);
                        time_duration diff_dur = diff_time.length();
                        long us_diff = diff_dur.total_microseconds();
                        long time_left = sleep_time - us_diff;
                        if(time_left > 0)
                            boost::this_thread::sleep(microseconds(time_left));
                    }
                }

                void AI_Engine::newListOfPlayers(const std::set<int>& ids)
                {
                    std::set<int> lost;
                    std::set<int> aiids;
                    BOOST_FOREACH(AIPlayer p, aiplayers)
                    {
                        aiids.insert(p.getID());
                    }
                    addutil::general::set_inverse_union(aiids, ids, lost);
                    aiplayers_locked = true;
                    std::cout << "AI_Engine::newListOfPlayers: lost " << lost.size() << " players.\n";
                    BOOST_FOREACH(int i, lost)
                    {
                        std::vector<AIPlayer>::iterator it = aiplayers.begin();
                        while(it != aiplayers.end())
                        {
                            if(it->getID() == i)
                            {
                                aiplayers.erase(it);
                                break;
                            }
                            it++;
                        }
                    }
                    aiplayers_locked = false;
                }
            }
        }
    }
}
