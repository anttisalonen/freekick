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
            : mMatchData(md),
              mBall(new MatchBall(*mMatchData->getBall()))
        {
            float pwidth = mMatchData->getStadium()->getPitch()->getWidth();
            float plength = mMatchData->getStadium()->getPitch()->getLength();
            mBall->setPosition(pwidth / 2.0f, 1.0f, plength / 2.0f);

            boost::shared_ptr<Club> c1 = mMatchData->getHomeClub();
            boost::shared_ptr<Club> c2 = mMatchData->getAwayClub();
            std::vector<boost::shared_ptr<Player> > c1pls;
            std::vector<boost::shared_ptr<Player> > c2pls;
            c1->getPlayers(c1pls);
            c2->getPlayers(c2pls);
            BOOST_FOREACH(boost::shared_ptr<Player> p, c1pls)
            {
                int i = p->getID();
                PlayerInLineup pil = c1->getLineup().playerInLineup(i);
                bool subst = (pil == Substitute);
                mPlayers[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p, subst));
            }
            BOOST_FOREACH(boost::shared_ptr<Player> p, c2pls)
            {
                int i = p->getID();
                PlayerInLineup pil = c2->getLineup().playerInLineup(i);
                bool subst = (pil == Substitute);
                mPlayers[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p, subst));
            }
            // TODO: add referee + others (if any)
        }

        const boost::shared_ptr<MatchData>& MatchStatus::getMatchData() const
        {
            return mMatchData;
        }

        void MatchStatus::getPlayers (std::map<int, boost::shared_ptr<MatchPlayer> >& v) const
        {
            v = mPlayers;
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
            std::map<int, boost::shared_ptr<MatchPlayer> >::iterator it;
            it = mPlayers.find(n);
            if (it == mPlayers.end())
            {
                if(n == BallID)
                {
                    mBall->update(v, vec.x, vec.y, vec.z);
                    mBall->updateOrientation(v, q.w, q.x, q.y, q.z);
                }
                // add updateable entities HERE
            }
            else
            {
                (*it).second->update(v, vec.x, vec.y, vec.z);
                (*it).second->updateOrientation(v, q.w, q.x, q.y, q.z);
            }
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

        void MatchStatus::update(const messages::GeneralUpdateStatusMessage& m)
        {
            mBallState = m.getBallState();
        }

        void MatchStatus::update(const std::vector<messages::GeneralUpdateStatusMessage>& ms)
        {
            typedef std::pair<std::string, boost::shared_ptr<MatchClub> > pair_cl;
            BOOST_FOREACH(messages::GeneralUpdateStatusMessage m, ms)
            {
                update(m);
            }
        }

        boost::shared_ptr<MatchPlayer> MatchStatus::getPlayer(int id) const
        {
            std::map<int, boost::shared_ptr<MatchPlayer> >::const_iterator it;
            it = mPlayers.find(id);
            if (it == mPlayers.end())
            {
                throw "MatchStatus::getPlayer: player not found";
            }

            return it->second;            
        }

        boost::shared_ptr<MatchBall> MatchStatus::getBall() const
        {
            return mBall;
        }

        const BallState& MatchStatus::getBallState() const
        {
            return mBallState;
        }

        BallOwner MatchStatus::getPlayerClub(int id) const
        {
            boost::shared_ptr<Club> c1 = mMatchData->getHomeClub();
            boost::shared_ptr<Club> c2 = mMatchData->getAwayClub();
            std::set<int> ids1;
            c1->getPlayerIDs(ids1);
            std::set<int> ids2;
            c2->getPlayerIDs(ids2);

            std::set<int>::const_iterator idsit;
            idsit = ids1.find(id);
            if(idsit != ids1.end())
                return Home;
            else
            {
                idsit = ids2.find(id);
                if(idsit != ids2.end())
                    return Away;
            }
            throw "MatchStatus::getPlayerClub: player ID not found in matchstatus!";
        }
    }
}
