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
            std::cout << "Adding ball to matchstatus\n";
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
        }

        const boost::shared_ptr<MatchData>& MatchStatus::getMatchData() const
        {
            return mMatchData;
        }

        void MatchStatus::getEntities (std::map<int, boost::shared_ptr<DynamicEntity> >& v)
        {
            v = mEntities;
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
    }
}
