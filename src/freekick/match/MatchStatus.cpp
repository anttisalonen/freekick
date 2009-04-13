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
              mContinue(true),
              mBall(new MatchBall(*mMatchData->getBall())),
              score_home(0),
              score_away(0),
              secondhalf(false)
        {
            float pwidth = mMatchData->getStadium()->getPitch()->getWidth();
            float plength = mMatchData->getStadium()->getPitch()->getLength();
            float start_x = pwidth / 2.0f;
            float start_z = plength / 2.0f;
            mBall->setPosition(start_x, 1.0f, start_z);
            mBallState.restart_point.x = start_x;
            mBallState.restart_point.z = start_z;

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
                mPlayers[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p, Home, subst));
            }
            BOOST_FOREACH(boost::shared_ptr<Player> p, c2pls)
            {
                int i = p->getID();
                PlayerInLineup pil = c2->getLineup()->playerInLineup(i);
                bool subst = (pil == Substitute);
                mPlayers[i] = boost::shared_ptr<MatchPlayer>(new MatchPlayer(*p, Away, subst));
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

        std::vector<boost::shared_ptr<MatchPlayer> > MatchStatus::getPlayers() const
        {
            std::vector<boost::shared_ptr<MatchPlayer> > retval;
            typedef const std::pair<const int, boost::shared_ptr<MatchPlayer> > pair_cl;
            BOOST_FOREACH(pair_cl m, mPlayers)
            {
                retval.push_back(m.second);
            }
            return retval;
        }

        void MatchStatus::getPlayers(std::vector<boost::shared_ptr<MatchPlayer> >& retval, soccer::BallOwner b) const
        {
            retval.clear();
            typedef const std::pair<const int, boost::shared_ptr<MatchPlayer> > pair_cl;
            BOOST_FOREACH(pair_cl m, mPlayers)
            {
                soccer::BallOwner bo = getPlayerSide(m.second->getID());
                if(bo == b)
                    retval.push_back(m.second);
            }
        }

        std::vector<boost::shared_ptr<MatchPlayer> > MatchStatus::getPlayers(soccer::BallOwner b) const
        {
            std::vector<boost::shared_ptr<MatchPlayer> > retval;
            typedef const std::pair<const int, boost::shared_ptr<MatchPlayer> > pair_cl;
            BOOST_FOREACH(pair_cl m, mPlayers)
            {
                soccer::BallOwner bo = getPlayerSide(m.second->getID());
                if(bo == b)
                    retval.push_back(m.second);
            }
            return retval;
        }

        void MatchStatus::update(const messages::ConstantUpdateMessage& m, float time_interval, bool update_offside_pos)
        {
            int n = m.getPlayerID();
            std::map<int, boost::shared_ptr<MatchPlayer> >::iterator it;
            it = mPlayers.find(n);
            if (it == mPlayers.end())
            {
                if(n == BallID)
                {
                    updateEntity(mBall.get(), m, time_interval);
                }
                // add updateable entities HERE
            }
            else
            {
                updateEntity(it->second.get(), m, time_interval);
                if(update_offside_pos)
                {
                    update_offside_positions(getPlayerSide(n));
                }
            }
        }

        void MatchStatus::updateEntity(addutil::DynamicEntity* e, const messages::ConstantUpdateMessage& m, float time_interval)
        {
            int v = m.getDerivative();
            addutil::Vector3 vec;
            addutil::Quaternion q;
            m.getVector(vec);
            m.getQuaternion(q);

            if(time_interval > 0.0f)
            {
                if(v == 0)
                {
                    addutil::Vector3 oldpos = e->getPosition();
                    addutil::Vector3 newvel = (vec - oldpos) * (1.0f / time_interval);
                    e->update(1, newvel);
                }
                else if (v == 1)
                {
                    addutil::Vector3 oldvel = e->getVelocity();
                    addutil::Vector3 newacc = (vec - oldvel) * (1.0f / time_interval);
                    e->update(2, newacc);
                }
            }

            e->update(v, vec.x, vec.y, vec.z);
            e->updateOrientation(v, q.w, q.x, q.y, q.z);

            int holder = holdingBall();
            int thisid = m.getPlayerID();
            ControlledStatus cons;
            m.getControlledStatus(cons);
            if(holder == thisid)
            {
                if(!cons.holding_ball)
                    setBallHolder(0);
            }
            if(cons.holding_ball)
            {
                setBallHolder(thisid);
            }
        }

        void MatchStatus::update(const std::vector<messages::ConstantUpdateMessage>& ms, float time_interval)
        {
            typedef std::pair<std::string, boost::shared_ptr<MatchClub> > pair_cl;
            BOOST_FOREACH(messages::ConstantUpdateMessage m, ms)
            {
                update(m, time_interval, false);
            }
            update_offside_positions(Home);
            update_offside_positions(Away);
        }

        void MatchStatus::update(const messages::GeneralUpdateStatusMessage& m)
        {
            mBallState = m.getBallState();
        }

        void MatchStatus::update(const messages::GeneralUpdateScoreMessage& m)
        {
            score_home = m.goals(true, false);
            score_away = m.goals(false, false);
            // TODO: also use penalty kick score; also see rules.cpp
            std::cout << "MatchStatus::update::GeneralUpdateScoreMessage: " << score_home << " : " << score_away << std::endl;
        }

        void MatchStatus::update(const messages::GeneralUpdateTimeMessage& m)
        {
            currtime.m = m.m_min;
            currtime.s = m.m_sec;
            if(m.m_half == 2)
                secondhalf = true;
            else if(m.m_half == 1)
                secondhalf = false;
            // TODO: parse extra time
            std::cout << "MatchStatus::update::GeneralUpdateTimeMessage: " << currtime.m << ":" << currtime.s << " (" << secondhalf << " half)\n";
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

        soccer::PlayerTarget MatchStatus::getPlayerTarget(int id) const
        {
            soccer::BallOwner own = getPlayerSide(id);
            if(!secondhalf)
            {
                if(own == Away)
                    return UpTarget;
                return DownTarget;
            }
            if(own == Away)
                return DownTarget;
            return UpTarget;
        }

        soccer::BallOwner MatchStatus::getPlayerSide(int id) const
        {
            return getPlayer(id)->getSide();
        }

        boost::shared_ptr<Club> MatchStatus::getPlayerClub(int id) const
        {
            if(getPlayerSide(id) == Home)
            {
                return mMatchData->getHomeClub();
            }
            return mMatchData->getAwayClub();
        }

        boost::shared_ptr<Formation> MatchStatus::getPlayerFormation(int id) const
        {
            return getPlayerClub(id)->getFormation();
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

        boost::tuple<int, float> MatchStatus::nearestPlayerFromClubToEntity(soccer::BallOwner b, const boost::shared_ptr<addutil::Entity>& e) const
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

                float this_length = (it->second->getPosition() - e->getPosition()).length();
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
            return nearestPlayerFromClubToEntity(b, mBall);
        }

        boost::tuple<int, float> MatchStatus::nearestPlayerFromClubToPlayer(soccer::BallOwner b, int id) const
        {
            return nearestPlayerFromClubToEntity(b, getPlayer(id));
        }

        float MatchStatus::getPitchWidth() const
        {
            return getPitch()->getWidth();
        }

        float MatchStatus::getPitchLength() const
        {
            return getPitch()->getLength();
        }

        const boost::shared_ptr<Pitch> MatchStatus::getPitch() const
        {
            return mMatchData->getStadium()->getPitch();
        }

        addutil::Vector3 MatchStatus::getGoalPosition(soccer::PlayerTarget b) const
        {
            float x = getPitchWidth() / 2.0f;
            float z = (b == UpTarget) ? 0.0f : getPitchLength();
            return addutil::Vector3(x, 0, z);
        }

        int MatchStatus::getPlayerPositions(std::vector<addutil::Vector3>& ret, int pid) const
        {
            return getPlayerPositions(ret, getPlayerSide(pid));
        }

        int MatchStatus::getPlayerPositions(std::vector<addutil::Vector3>& ret, soccer::PlayerTarget t) const
        {
            soccer::BallOwner b = playerTargetToBallOwner(t, secondhalf);
            return getPlayerPositions(ret, b);
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
            // TODO: add "player can't touch the ball after giving throwin/goal kick/etc."

            // 1. before/after the match kick as much as you want
            if(mBallState.bio_type == PreKickoff || mBallState.bio_type == HalfFullTime) return true;

            // 2. blocked -> deny
            // if(mBallState.blocked_play) return false;

            // 3. ball held in someone's hands -> deny
            int h = mBall->getHolder();
            if(h != 0 && h != id) return false;

            // 4. ball in play or we're supposed to get it in -> accept
            soccer::BallOwner b = getPlayerSide(id);
            if(mBallState.bio_type == BallIn || mBallState.owner == b)
            {
                return true;
            }
            return false;
        }

        bool MatchStatus::continuing() const
        {
            return mContinue;
        }

        void MatchStatus::setContinue(bool c)
        {
            mContinue = c;
        }

        void MatchStatus::addHomeScore()
        {
            score_home++;
        }

        void MatchStatus::addAwayScore()
        {
            score_away++;
        }

        int MatchStatus::getHomeScore() const
        {
            return score_home;
        }

        int MatchStatus::getAwayScore() const
        {
            return score_away;
        }

        GoalQuery MatchStatus::ballInGoalArea() const
        {
            GoalQuery q = mMatchData->getStadium()->getPitch()->inGoalArea(mBall->getPosition());
            if(q == NoGoal) return q;
            if(secondhalf) return other(q);
            return q;
        }

        int MatchStatus::holdingBall() const
        {
            return mBall->getHolder();
        }

        void MatchStatus::setBallHolder(int h)
        {
            mBall->setHolder(h);
        }

        BallOwner MatchStatus::nearestClubToBall() const
        {
            return getPlayerSide(nearestPlayerToBall().get<0>());
        }

        int MatchStatus::ownPlayersInOwnPenaltyBox(BallOwner b) const
        {
            boost::shared_ptr<Pitch> p = getPitch();
            int retval = 0;
            typedef const std::pair<const int, boost::shared_ptr<MatchPlayer> > pair_cl;
            BOOST_FOREACH(pair_cl m, mPlayers)
            {
                if(getPlayerSide(m.second->getID()) == b)
                {
                    if(p->inPenaltyBoxArea(m.second->getPosition()))
                        retval++;
                }
            }
            return retval;
        }

        float MatchStatus::playerDistanceToBall(int id) const
        {
            return (getPlayer(id)->getPosition() - mBall->getPosition()).length();
        }

        float MatchStatus::distanceToNearestOpponent(int id) const
        {
            return nearestPlayerFromClubToEntity(other(getPlayerSide(id)), getPlayer(id)).get<0>();
        }

        bool MatchStatus::ballInOwnPenaltyBox(BallOwner b) const
        {
            GoalQuery g = getPitch()->inPenaltyBoxArea(mBall->getPosition());
            if(g == NoGoal) return false;
            bool retval = false;
            if(g == HomeGoal && b == Home)
                retval = true;
            else if (g == AwayGoal && b == Away)
                retval = true;
            if(secondhalf)
                retval = !retval;
            return retval;
        }

        addutil::Vector3 MatchStatus::percent_pitch_position_to_absolute(const addutil::Vector3& perc) const
        {
            addutil::Vector3 retval = perc;
            retval.x *= getPitchWidth();
            retval.z *= getPitchLength();
            return retval;
        }

        addutil::Vector3 MatchStatus::percent_pitch_position_to_absolute(const addutil::Vector3& perc, BallOwner side) const
        {
            addutil::Vector3 retval = perc;
            if(sides_flipped(side))
            {
                retval.x = 1.0f - retval.x;
                retval.z = 1.0f - retval.z;
            }
            retval.x *= getPitchWidth();
            retval.z *= getPitchLength();
            return retval;
        }

        addutil::Vector3 MatchStatus::absolute_pitch_position_to_percent(const addutil::Vector3& abs) const
        {
            addutil::Vector3 retval = abs;
            retval.x /= getPitchWidth();
            retval.z /= getPitchLength();
            return retval;
        }

        addutil::Vector3 MatchStatus::absolute_pitch_position_to_percent(const addutil::Vector3& abs, BallOwner side) const
        {
            addutil::Vector3 retval = abs;
            if(sides_flipped(side))
            {
                retval.x = getPitchWidth() - retval.x;
                retval.z = getPitchLength() - retval.z;
            }
            retval.x /= getPitchWidth();
            retval.z /= getPitchLength();
            return retval;
        }

        void MatchStatus::update_offside_positions(BallOwner b)
        {
            int index = (b == Home) ? 0 : 1;
            float second_lowest_opponent = 100000.0f;
            float lowest_opponent = 200000.0f;
            MatchPlayerMap::const_iterator it;
            for(it = mPlayers.begin(); it != mPlayers.end(); it++)
            {
                if(it->second->getSide() == b)
                    continue;
                if(it->second->isSubstitute())
                    continue;
                float this_pos = it->second->getPosition().z;
                if(ballOwnerToPlayerTarget(other(b), secondhalf)) // they have uptarget
                {
                    if(this_pos < lowest_opponent)
                    {
                        second_lowest_opponent = lowest_opponent;
                        lowest_opponent = this_pos;
                    }
                    else if(this_pos < second_lowest_opponent)
                    {
                        second_lowest_opponent = this_pos;
                    }
                }
                else
                {
                    if(this_pos > lowest_opponent)
                    {
                        second_lowest_opponent = lowest_opponent;
                        lowest_opponent = this_pos;
                    }
                    else if(this_pos > second_lowest_opponent)
                    {
                        second_lowest_opponent = this_pos;
                    }
                }
            }
            offside_lines[index] = second_lowest_opponent;      // TODO: take body size into account (currently measured from middle of body)
        }

        bool MatchStatus::inOffsidePosition(int id) const
        {
            return inOffsidePosition(getPlayerTarget(id), getPlayer(id)->getPosition());
        }

        bool MatchStatus::sides_flipped(BallOwner side) const
        {
            return ((side == Away && !secondhalf) || (side == Home && secondhalf));
        }

        float MatchStatus::getOffsideLine(BallOwner b) const
        {
            int index = (b == Home) ? 0 : 1;
            float ball_pos_z = mBall->getPosition().z;
            if(ballOwnerToPlayerTarget(b, secondhalf) == UpTarget)
            {
                if(ball_pos_z < offside_lines[index])
                    return 0.0f;
            }
            else
            {
                if(ball_pos_z > offside_lines[index])
                    return getPitch()->getLength();
            }
            return offside_lines[index];
        }

        float MatchStatus::getOffsideLine(PlayerTarget b) const
        {
            if((b == DownTarget && !secondhalf) || (b == UpTarget && secondhalf))
                return getOffsideLine(Home);
            return getOffsideLine(Away);
        }

        bool MatchStatus::onPitch(const addutil::Vector3& pos) const
        {
            return getPitch()->onPitch(pos.x, pos.z);
        }

        bool MatchStatus::onOwnSide(const addutil::Vector3& pos, BallOwner side) const
        {
            bool on_top_side = getPitch()->onSide(true, pos.x, pos.z);
            if((side == Home && !secondhalf) || (side == Away && secondhalf))
                return on_top_side;
            return !on_top_side;
        }

        bool MatchStatus::inOffsidePosition(soccer::PlayerTarget b, const addutil::Vector3& v) const
        {
            float offside_line = getOffsideLine(b);

            if(b == UpTarget)
            {
                return (v.z < offside_line);
            }
            else
            {
                return (v.z > offside_line);
            }
        }

        void MatchStatus::resetTime()
        {
            currtime.reset();
        }

        void MatchStatus::addTime(float sec)
        {
            currtime.add_ms(sec * 1000.0f);
        }

        void MatchStatus::updateTimers(float sec)
        {
            std::map<int, boost::shared_ptr<MatchPlayer> >::iterator it;
            for(it = mPlayers.begin(); it != mPlayers.end(); ++it)
            {
                it->second->updateTimer(sec);
            }
        }

        void MatchStatus::getTime(int& m, int& s) const
        {
            m = currtime.m;
            s = currtime.s;
        }

        int MatchStatus::getTimeSec() const
        {
            return currtime.m * 60 + currtime.s;
        }

        bool MatchStatus::isSecondHalf() const
        {
            return secondhalf;
        }

        void MatchStatus::setSecondHalf(int val)
        {
            if(val == 2)
                secondhalf = true;
            else if(val == 1)
                secondhalf = false;
        }

        bool MatchStatus::createMatchResultFile() const
        {
            if(!mMatchData->getFilename())
                return false;
            std::string filename(mMatchData->getFilename());
            if(filename.empty())
                return false;
            filename += "_res";

            xmlDocPtr doc = NULL;       /* document pointer */
            xmlNodePtr root_node = NULL;
            xmlNodePtr sub_node = NULL;

            doc = xmlNewDoc(BAD_CAST "1.0");
            root_node = xmlNewNode(NULL, BAD_CAST "MatchResult");
            xmlDocSetRootElement(doc, root_node);
            sub_node = addutil::xml::add_child(root_node, "home");
            addutil::xml::add_attribute(sub_node, "score", score_home);
            sub_node = addutil::xml::add_child(root_node, "away");
            addutil::xml::add_attribute(sub_node, "score", score_away);
            xmlSaveFormatFileEnc(filename.c_str(), doc, "UTF-8", 1);
            xmlFreeDoc(doc);
            return true;
        }
    }
}
