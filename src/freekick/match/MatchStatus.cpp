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
                PlayerInLineup pil = c1->getLineup()->playerInLineup(i);
                bool subst = (pil == Substitute);
                mPlayers[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p, subst));
            }
            BOOST_FOREACH(boost::shared_ptr<Player> p, c2pls)
            {
                int i = p->getID();
                PlayerInLineup pil = c2->getLineup()->playerInLineup(i);
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

        soccer::BallOwner MatchStatus::getPlayerSide(int id) const
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
            throw "MatchStatus::getPlayerSide: player ID not found in matchstatus!";
        }

        boost::shared_ptr<Club> MatchStatus::getPlayerClub(int id) const
        {
            if(getPlayerSide(id) == Home)
            {
                return mMatchData->getHomeClub();
            }
            return mMatchData->getAwayClub();
        }

        addutil::Vector3 MatchStatus::getCentreSpot() const
        {
            float pwidth = mMatchData->getStadium()->getPitch()->getWidth();
            float plength = mMatchData->getStadium()->getPitch()->getLength();
            float middle_x = pwidth / 2.0f;
            float middle_z = plength / 2.0f;
            return addutil::Vector3(middle_x, 0.0f, middle_z);
        }

        boost::tuple<int, float> MatchStatus::nearestPlayerToBall() const
        {
            MatchPlayerMap::const_iterator it;
            float min_length = 100000.0f;
            int plid = 0;
            for(it = mPlayers.begin(); it != mPlayers.end(); it++)
            {
                if(it->second->isSubstitute()) continue;
                Vector3 v = it->second->getPosition() - mBall->getPosition();
                float this_length = v.length();
                if(this_length < min_length)
                {
                    min_length = this_length;
                    plid = it->first;
                }
            }
            return boost::tuple<int, float>(plid, min_length);
        }

        boost::tuple<int, float> MatchStatus::nearestPlayerFromClubToBall(soccer::BallOwner b) const
        {
            boost::shared_ptr<Club> c1 = mMatchData->getHomeClub();
            boost::shared_ptr<Club> c2 = mMatchData->getAwayClub();
            std::set<int> ids1;
            c1->getPlayerIDs(ids1);
            std::set<int> ids2;
            c2->getPlayerIDs(ids2);

            MatchPlayerMap::const_iterator it;
            float min_length = 100000.0f;
            int plid = 0;
            for(it = mPlayers.begin(); it != mPlayers.end(); it++)
            {
                if(it->second->isSubstitute()) continue;

                std::set<int>::const_iterator idsit;
                if(b == Home)
                {
                    idsit = ids1.find(it->first);
                    if(idsit == ids1.end())
                        continue;
                }
                else
                {
                    idsit = ids2.find(it->first);
                    if(idsit == ids2.end())
                        continue;
                }

                float this_length = (it->second->getPosition() - mBall->getPosition()).length();
                if(this_length < min_length)
                {
                    min_length = this_length;
                    plid = it->first;
                }
            }
            return boost::tuple<int, float>(plid, min_length);
        }

        float MatchStatus::getPitchWidth() const
        {
            return mMatchData->getStadium()->getPitch()->getWidth();
        }

        float MatchStatus::getPitchLength() const
        {
            return mMatchData->getStadium()->getPitch()->getLength();
        }

        addutil::Vector3 MatchStatus::getGoal(soccer::BallOwner b) const
        {
            float x = getPitchWidth() / 2.0f;
            float z = (b == Home) ? 0.0f : getPitchLength();
            return addutil::Vector3(x, 0, z);
        }

        int MatchStatus::getPlayerPositions(std::vector<addutil::Vector3>& ret, soccer::BallOwner b) const
        {
            int num = 0;
            ret.clear();

            std::vector<int> ids1 = mMatchData->getClub(b)->getLineup()->getPitchPlayerIDs();

            BOOST_FOREACH(int i, ids1)
            {
                ret.push_back(getPlayer(i)->getPosition());
                num++;
            }
            return num;
        }

        bool MatchStatus::playerAllowedToKick(int id) const
        {
            if(mBallState.blocked_play) return false;
            soccer::BallOwner b = getPlayerSide(id);
            if(mBallState.bio_type == BallIn || mBallState.owner == b)
            {
                return true;
            }
            return false;
        }
    }
}
