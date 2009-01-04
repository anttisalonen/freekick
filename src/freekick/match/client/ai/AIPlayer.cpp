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

#include "AIPlayer.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                AIPlayer::AIPlayer(boost::shared_ptr<MatchStatus> ms, int id, bool active)
                    : mMatchStatus(ms),
                      mPlayerID(id),
                      mActive(active)
                {
                    try
                    {
                        mPlayer = mMatchStatus->getPlayer(id);
                    }
                    catch (...)
                    {
                        throw "AIPlayer::AIPlayer: player not found in matchstatus!";
                    }

                    boost::shared_ptr<Club> c1 = mMatchStatus->getMatchData()->getHomeClub();
                    boost::shared_ptr<Club> c2 = mMatchStatus->getMatchData()->getAwayClub();
                    std::set<int> ids1;
                    c1->getPlayerIDs(ids1);
                    std::set<int> ids2;
                    c2->getPlayerIDs(ids2);
                    bool in_home = false;
                    bool in_away = false;

                    std::set<int>::const_iterator idsit;
                    idsit = ids1.find(id);
                    if(idsit != ids1.end())
                        in_home = true;
                    else
                    {
                        idsit = ids2.find(id);
                        if(idsit != ids2.end())
                            in_away = true;
                    }
                    if(!in_home && !in_away) throw "AIPlayer::AIPlayer: player ID not found in matchstatus!";
                    if(in_home)
                    {
                        mClub = c1;
                        mOpponentClub = c2;
                    }
                    else
                    {
                        mClub = c2;
                        mOpponentClub = c1;
                    }

                    if(in_home)
                    {
                        fillPlayers(mTeammates, ids1);
                        fillPlayers(mOpponents, ids2);
                    }
                    else
                    {
                        fillPlayers(mTeammates, ids2);
                        fillPlayers(mOpponents, ids1);
                    }

                    boost::shared_ptr<tasks::Soccer> t(new tasks::Soccer(mMatchStatus, mPlayerID));
                    mTaskManager.addTask(t);
                }

                void AIPlayer::fillPlayers(std::vector<boost::shared_ptr<MatchPlayer> >& pl, const std::set<int>& plids)
                {
                    BOOST_FOREACH(int i, plids)
                    {
                        try
                        {
                            boost::shared_ptr<MatchPlayer> d = mMatchStatus->getPlayer(i);
                            pl.push_back(d);
                        }
                        catch (...)
                        {
                            // Player not playing -> ignore
                        }
                    }
                }

                AIPlayer::~AIPlayer()
                {
                }

                boost::shared_ptr<messages::PlayerControlMessage> AIPlayer::act()
                {
                    return mTaskManager.think();
                }
            }
        }
    }
}
