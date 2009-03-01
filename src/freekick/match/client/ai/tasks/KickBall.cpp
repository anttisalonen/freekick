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

#include "tasks/KickBall.h"

namespace freekick 
{ 
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                namespace tasks
                {
                    KickBall::KickBall (const boost::shared_ptr<MatchStatus>& ms, int id)
                        : mMatchStatus(ms),
                          mPlayerID(id)
                    {
                        mPlayer = mMatchStatus->getPlayer(mPlayerID);
                    }

                    bool KickBall::finished() const
                    {
                        // return true;
                        // float len = (mPlayer->getPosition() - mTarget).length();
                        // if(len < 5.0f) return true;
                        return false;
                    }

                    optimal_kick KickBall::getOptimalPass() const
                    {
                        float plength = mMatchStatus->getPitchLength();
                        const float max_pass_length = 25.0f;
                        float opt_val = 0.0f;
                        const float defensive_area_threshold = 25.0f;
                        const float defensive_area_coefficient = 0.15f;

                        float best_z    = 0.0f;
                        float own_z     = (t == UpTarget) ? plength - ownpos.z : ownpos.z;
                        addutil::Vector3 bestpass;

                        // TODO: take opponents near target into account
                        std::vector<boost::shared_ptr<MatchPlayer> > ownclub;
                        std::vector<boost::shared_ptr<MatchPlayer> >::const_iterator clubit;

                        mMatchStatus->getPlayers(ownclub, mMatchStatus->getPlayerSide(mPlayerID));
                        for(clubit = ownclub.begin(); clubit != ownclub.end(); clubit++)
                        {
                            addutil::Vector3 clubitpos = (*clubit)->getPosition();
                            // TODO: passing ahead of the player
                            float diff = (clubitpos - ownpos).length();
                            float this_z = clubitpos.z;
                            if(this_z == ownpos.z)
                                continue;
                            if(diff < 10.0f || diff > max_pass_length)
                                continue;
                            if(!mMatchStatus->onPitch(clubitpos))
                                continue;
                            if(mMatchStatus->inOffsidePosition((*clubit)->getID()))
                                continue;

                            if(t == UpTarget) 
                                this_z = plength - this_z;
                            if (this_z < own_z - 10.0f)
                                continue;

                            if(this_z > best_z)
                            {
                                bestpass = clubitpos;
                                best_z = this_z;
                            }
                        }
                        if(bestpass.length2() == 0.0f)
                            return optimal_kick(0.0f, bestpass);
                        addutil::Vector3 target = bestpass - ownpos;

                        std::cerr << "best_z: " << best_z << std::endl;
                        opt_val = 1.0f - (target.length() / max_pass_length);
                        if(best_z < defensive_area_threshold) 
                        {
                            opt_val *= defensive_area_coefficient;
                            std::cerr << "pass target in defensive area\n";
                        }

                        Helpers::correctPassVector(target);
                        // std::cout << "Pass: Kick target: " << (target + ownpos) << std::endl;

                        return optimal_kick(opt_val, target);
                    }

                    optimal_kick KickBall::getOptimalShot() const
                    {
                        addutil::Vector3 target = tgtgoal - ownpos;
                        Helpers::correctShootVector(target);
                        float gvlen = goalvec.length();
                        float max_shoot_len = 35.0f;
                        float min_opt_shoot_len = 10.0f;
                        float val;
                        // TODO: take angle into account
                        if(gvlen < min_opt_shoot_len)
                            val = 1.0f;
                        if(gvlen > max_shoot_len)
                            val = 0.0f;
                        else 
                            val = 1.0f - ((gvlen - min_opt_shoot_len) / (max_shoot_len - min_opt_shoot_len));
                        return optimal_kick(val, target);
                    }

                    optimal_kick KickBall::getOptimalDribble() const
                    {
                        addutil::Vector3 target = tgtgoal - ownpos;
                        soccer::BallOwner our_club = mMatchStatus->getPlayerSide(mPlayerID);
                        soccer::BallOwner other_club = soccer::other(our_club);
                        float nop_len = mMatchStatus->nearestPlayerFromClubToPlayer(other_club, mPlayerID).get<1>();
                        const float nop_max_len = 10.0f;
                        if(nop_len > nop_max_len) nop_len = nop_max_len;
                        target.normalize();
                        const float dribble_strength = 10.0f;
                        target *= dribble_strength;
                        const float dribble_coefficient = 0.2f;
                        float opt_val = (nop_len / nop_max_len) * dribble_coefficient;

                        const float offensive_area_multiplier = 2.0f;
                        const float offensive_area_threshold = 0.65f;
                        const float defensive_area_multiplier = 0.5f;
                        const float defensive_area_threshold = 0.20f;
                        addutil::Vector3 ownpos = mMatchStatus->getPlayer(mPlayerID)->getPosition();
                        addutil::Vector3 ownpos_rel = mMatchStatus->absolute_pitch_position_to_percent(ownpos, our_club);

                        if(ownpos_rel.z > offensive_area_threshold)
                            opt_val *= offensive_area_multiplier;
                        else if(ownpos_rel.z < defensive_area_threshold)
                            opt_val *= defensive_area_multiplier;

                        return optimal_kick(opt_val, target);
                    }

                    optimal_kick KickBall::getOptimalLongBall() const
                    {
                        float plength = mMatchStatus->getPitchLength();
                        float opt_val = 0.0f;

                        float best_z    = 0.0f;
                        float own_z     = (t == UpTarget) ? plength - ownpos.z : ownpos.z;
                        addutil::Vector3 bestpass;

                        std::vector<boost::shared_ptr<MatchPlayer> > ownclub;
                        std::vector<boost::shared_ptr<MatchPlayer> >::const_iterator clubit;

                        mMatchStatus->getPlayers(ownclub, mMatchStatus->getPlayerSide(mPlayerID));
                        for(clubit = ownclub.begin(); clubit != ownclub.end(); clubit++)
                        {
                            addutil::Vector3 clubitpos = (*clubit)->getPosition();
                            float diff = (clubitpos - ownpos).length();
                            float this_z = clubitpos.z;
                            if(this_z == ownpos.z)
                                continue;
                            if(t == UpTarget) this_z = plength - this_z;

                            if(diff < 5.0f)
                                continue;
                            if (this_z < own_z + 25.0f)
                                continue;
                            if(mMatchStatus->inOffsidePosition((*clubit)->getID()))
                                continue;

                            if(this_z > best_z)
                            {
                                bestpass = clubitpos;
                                best_z = this_z;
                            }
                        }
                        if(bestpass.length2() == 0.0f)
                            return optimal_kick(0.0f, bestpass);
                        addutil::Vector3 target = bestpass - ownpos;

                        float tlen = target.length();
                        const float max_long_pass = 70.0f;
                        const float long_ball_coefficient = 0.2f;
                        if(tlen > max_long_pass) tlen = max_long_pass;
                        opt_val = (tlen / max_long_pass) * long_ball_coefficient;

                        Helpers::correctLongBallVector(target);
                        return optimal_kick(opt_val, target);
                    }

                    boost::shared_ptr<messages::PlayerControlMessage> KickBall::process()
                    {
                        clearTasks();
                        ownpos = mPlayer->getPosition();
                        t = mMatchStatus->getPlayerTarget(mPlayerID);
                        tgtgoal = mMatchStatus->getGoalPosition(t);
                        goalvec = tgtgoal - ownpos;

                        optimal_kick optimalpass = getOptimalPass();
                        optimal_kick optimalshot = getOptimalShot();
                        optimal_kick optimaldribble = getOptimalDribble();
                        optimal_kick optimallong = getOptimalLongBall();
                        std::cerr << "Pass value: " << optimalpass.get<0>() << std::endl;
                        std::cerr << "Shot value: " << optimalshot.get<0>() << std::endl;
                        std::cerr << "Dribble value: " << optimaldribble.get<0>() << std::endl;
                        std::cerr << "Long ball value: " << optimallong.get<0>() << std::endl;
                        std::cerr << std::endl;

                        using namespace messages;
                        if(optimalpass.get<0>() >= optimalshot.get<0>() &&
                           optimalpass.get<0>() >= optimaldribble.get<0>() &&
                           optimalpass.get<0>() >= optimallong.get<0>())
                        {
                            return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimalpass.get<1>()));
                        }
                        else if (optimalshot.get<0>() >= optimaldribble.get<0>() &&
                                 optimalshot.get<0>() >= optimallong.get<0>())
                        {
                            return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimalshot.get<1>()));
                        }
                        else if (optimaldribble.get<0>() >= optimallong.get<0>())
                        {
                            return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimaldribble.get<1>()));
                        }
                        else
                        {
                            return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimallong.get<1>()));
                        }
                    }
                }
            }
        }
    }
}
