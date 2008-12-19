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

#include "MatchStatus.h"

namespace freekick
{
    namespace match
    {
        MatchStatus::MatchStatus(boost::shared_ptr<MatchData> md)
            : mMatchData(md)
        {
            mEntities[BallID] = boost::shared_ptr<MatchBall>(new MatchBall(*mMatchData->getBall()));

            boost::shared_ptr<Club> c1 = mMatchData->getHomeClub();
            boost::shared_ptr<Club> c2 = mMatchData->getAwayClub();
            std::vector<boost::shared_ptr<Player> > c1pls;
            std::vector<boost::shared_ptr<Player> > c2pls;
            c1->getPlayers(c1pls);
            c2->getPlayers(c2pls);
            BOOST_FOREACH(boost::shared_ptr<Player> p, c1pls)
            {
                int i = p->getID();
                mEntities[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p));
            }
            BOOST_FOREACH(boost::shared_ptr<Player> p, c2pls)
            {
                int i = p->getID();
                mEntities[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p));
            }
            // TODO: add referee + others (if any)

            typedef std::pair<int, boost::shared_ptr<DynamicEntity> > pair_de;
            BOOST_FOREACH(pair_de p, mEntities)
            {
                mEntityVector.push_back(p.second);
            }
        }

/*
        void MatchStatus::updatePlayers()
        {
            boost::array<PlayerList, 6> pllist;
            boost::array<Lineup, 2> lineups;
            mMatchData->getHomeClub()->getPlayers(pllist[0]);
            mMatchData->getAwayClub()->getPlayers(pllist[1]);
            mMatchData->getHomeLineup(lineups[0]);
            mMatchData->getAwayLineup(lineups[1]);
            pllist[2] = lineups[0]->getPitchPlayers();
            pllist[3] = lineups[0]->getSubstitutes();
            pllist[4] = lineups[1]->getPitchPlayers();
            pllist[5] = lineups[1]->getSubstitutes();
            BOOST_FOREACH(PlayerList l, pllist)
            {
                BOOST_FOREACH(boost::shared_ptr<Player> p, l)
                {
                    int i = p->getID();
                    mEntities[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p));
                }
            }
            // TODO: add referee + others (if any)
            // TODO: make the whole thing with lineups + clubs cleaner and remove duplicate code
            typedef std::pair<int, boost::shared_ptr<DynamicEntity> > pair_de;
            BOOST_FOREACH(pair_de p, mEntities)
            {
                mEntityVector.push_back(p.second);
            }
        }
*/

        boost::shared_ptr<MatchData> MatchStatus::getMatchData() const
        {
            return mMatchData;
        }

/*
        void MatchStatus::newEvent (const std::string& evt ) 
        {
            int n, v;
            float x, y, z, qw, qx, qy, qz;
            std::istringstream ist(evt);
            ist >> n >> v >> x >> y >> z >> qw >> qx >> qy >> qz;
            if(n == BallID)
            {
                ball->update(v, x, y, z);
                return;
            }
            BOOST_FOREACH(boost::shared_ptr<Club> c, clubs)
            {
                if (c->updatePlayer(n, v, x, y, z, qw, qx, qy, qz))
                    return;
            }
        }

        void MatchStatus::newEvents (std::vector <std::string>& events ) 
        {
            BOOST_FOREACH(std::string s, events)
                newEvent(s);
        }
*/

        void MatchStatus::getEntities (std::vector <boost::shared_ptr<DynamicEntity> >& v)
        {
            v = mEntityVector;
        }

        void MatchStatus::update(const messages::ConstantUpdateMessage& m)
        {
            int n, v;
            addutil::Vector3 vec;
            addutil::Quaternion q;
            n = m.getPlayerID();
            v = m.getDerivative();
            m.getVector(vec);
            m.getQuaternion(q);
            std::map<int, boost::shared_ptr<DynamicEntity> >::iterator it;
            it = mEntities.find(n);
            if (it == mEntities.end())
            {
                return;
            }

            (*it).second->update(v, vec.x, vec.y, vec.z);
            (*it).second->updateOrientation(v, q.w, q.x, q.y, q.z);
            return;
        }

        void MatchStatus::update(const std::vector<messages::ConstantUpdateMessage>& ms)
        {
            typedef std::pair<std::string, boost::shared_ptr<MatchClub> > pair_cl;
            BOOST_FOREACH(messages::ConstantUpdateMessage m, ms)
            {
                update(m);
            }
        }
    }
}
