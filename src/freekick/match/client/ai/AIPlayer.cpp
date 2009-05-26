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

#include "AIPlayer.h"

namespace freekick
{
    namespace match
    {
        namespace client
        {
            namespace ai_client
            {
                using namespace messages;

                AIPlayer::AIPlayer(boost::shared_ptr<MatchStatus>ms, boost::shared_ptr<MatchPlayer> mp, boost::shared_ptr<ClubAI> clubai, bool active)
                    : mMatchStatus(ms),
                      mPlayer(mp),
                      mClubAI(clubai),
                      mPlayerID(mp->getID()),
                      mActive(active)
                {
                    plpos = "";
                }

                AIPlayer::~AIPlayer()
                {
                }

                CtrlMsg AIPlayer::act()
                {
                    allowed_to_kick = mMatchStatus->playerAllowedToKick(mPlayerID) || (mClubAI->isBlocked() && mClubAI->weAreOwner());
                    isnearestplayer = mClubAI->nearestOwn().get<0>() == mPlayerID;
                    abletokick = isnearestplayer && mClubAI->nearestOwn().get<1>() < constants::max_kick_distance;
                    issub = mPlayer->isSubstitute();
                    if(!issub)
                    {
                        plpos = mMatchStatus->getPlayerClub(mPlayerID)->getPlayerPosition(mPlayerID);
                        mGoalie = plpos == "Goalkeeper";
                    }
                    holdingtheball = mMatchStatus->holdingBall() == mPlayerID;
                    startplay = (mClubAI->isKickoff() && !mClubAI->isBlocked() && isnearestplayer && mClubAI->weAreOwner());
                    ownpos = mPlayer->getPosition();
                    goalvec = mClubAI->getTargetGoal() - ownpos;
                    disttoball = (ownpos - mClubAI->getBallPos()).length();
                    dangerlevel = std::max(
                            mClubAI->getBallDesire(), 
                            addutil::general::normalize(max_danger_dist, mMatchStatus->distanceToNearestOpponent(mPlayerID)));
                    futpos = mClubAI->ballFuturePos();
                    futpos.y = 0.0f;
                    futpos_dist = (ownpos - futpos).length();

                    return think();
                }

                CtrlMsg AIPlayer::think()
                {
                    if((mClubAI->getBio() == PreKickoff && mClubAI->isBlocked()) || 
                            (mClubAI->isKickoff() && !startplay))
                    {   // before kickoff
                        return kickoffpos();
                    }
                    if(issub || mClubAI->getBio() == HalfFullTime)
                    {   // to the bench
                        return gotocabins();
                    }
                    if(mGoalie && mClubAI->getBio() == BallIn)
                    {   // goalkeeper
                        return goalkeeper();
                    }
                    if(allowed_to_kick)
                    {   // match time
                        if(abletokick)
                        {   // on the ball
                            return kickball();
                        }
                        if(isnearestplayer || 
                                futpos_dist < AIConfig::getInstance()->max_future_fetch_distance)
                        {   // near the ball
                            return fetchball();
                        }
                        // idling
                        return idleinformation();
                    }
                    return idleinformation();
                }

                CtrlMsg AIPlayer::idle()
                {
                    return boost::shared_ptr<MovePlayerControlMessage>(
                            new MovePlayerControlMessage(mPlayerID, addutil::Vector3()));
                }

                CtrlMsg AIPlayer::idleinformation()
                {
                    if(mClubAI->ourClubHasBall())
                    {
                        return support();
                    }
                    return defensive();
                }

                CtrlMsg AIPlayer::support()
                {
                    // float bz = mClubAI->ballPosCorrected().z;
                    return inownarea();
                }

                CtrlMsg AIPlayer::defensive()
                {
                    return inownarea();
                }

                CtrlMsg AIPlayer::inownarea()
                {
                    const boost::shared_ptr<Formation> f = mMatchStatus->getPlayerClub(mPlayerID)->getFormation();
                    addutil::Vector3 ballpos = mClubAI->getBallPos();

                    using namespace messages;
                    /*
                    if(f->inTacticArea(mClubAI->ourClubHasBall(), mClubAI->ballOnOurSide(), plpos,
                                       mMatchStatus->absolute_pitch_position_to_percent(ownpos, mClubAI->ourClub())) &&
                       !(mMatchStatus->inOffsidePosition(mPlayerID)))
                    {
                        addutil::Vector3 gotovec = ballpos - ownpos;
                        gotovec.normalize();
                        gotovec *= 0.05f;
                        return boost::shared_ptr<MovePlayerControlMessage>(new MovePlayerControlMessage(mPlayerID, gotovec));
                    }
                    */

                    addutil::Vector3 formationpoint = f->getPitchPoint(plpos);
                    addutil::Vector3 movedFormationpoint(formationpoint);
                    float plength = mMatchStatus->getPitchLength();
                    float pwidth = mMatchStatus->getPitchWidth();
                    float bheight = ballpos.z / plength;
                    float bwidth = ballpos.x / pwidth;
                    if(mClubAI->getTarget() == UpTarget)
                    {
                        bheight = 1.0f - bheight;
                    }
                    // 0.0f: own goal; 1.0f: opponent's goal

                    float target_z_modifier = 
                        (1.0f - formationpoint.z) * 
                        (bheight - formationpoint.z) * 
                        (mGoalie ? 0.04f : 0.2f);
                    movedFormationpoint.z += target_z_modifier;

                    float target_x_modifier = (bwidth - 0.5f) * 0.2f;
                    if(mClubAI->getTarget() == UpTarget)
                    {
                        movedFormationpoint.x -= target_x_modifier;
                    }
                    else
                    {
                        movedFormationpoint.x += target_x_modifier;
                    }
                    addutil::general::clamp(movedFormationpoint.x, 0.0f, 1.0f);

                    addutil::Vector3 tgtvec;
                    if((movedFormationpoint - ownpos).length() > 5.0f)
                    {
                        tgtvec = movedFormationpoint;
                    }
                    else
                    {
                        tgtvec = formationpoint;
                    }

                    if(mClubAI->getTarget() == UpTarget)
                    {
                        tgtvec.z = 1.0f - tgtvec.z;
                        tgtvec.x = 1.0f - tgtvec.x;
                    }

                    tgtvec.x *= pwidth;
                    tgtvec.z *= plength;

                    if(mMatchStatus->inOffsidePosition(mClubAI->getTarget(), tgtvec))
                    {
                        tgtvec.z = mMatchStatus->getOffsideLine(mClubAI->getTarget());
                    }

                    return runTo(tgtvec, gen_jog_vel);
                }

                CtrlMsg AIPlayer::gotocabins()
                {
                    return runTo(cabins_pos(), min_walk_vel);
                }

                addutil::Vector3 AIPlayer::cabins_pos() const
                {
                    addutil::Vector3 retval;
                    retval.x = -0.1f;
                    retval.z = 0.4f;
                    if(mClubAI->ourClub() == Away)
                    {
                        retval.z = 1.0f - retval.z;
                    }
                    float pitchw = mMatchStatus->getMatchData()->getStadium()->getPitch()->getWidth();
                    float pitchl = mMatchStatus->getMatchData()->getStadium()->getPitch()->getLength();
                    retval.x *= pitchw;
                    retval.z *= pitchl;
                    return retval;
                }

                CtrlMsg AIPlayer::kickoffpos()
                {
                    set_own_kickoff_pos();
                    return runTo(ownkickoffpos, gen_jog_vel);
                }

                CtrlMsg AIPlayer::goalkeeper()
                {
                    if(isnearestplayer && mClubAI->ballInOurGoalArea())
                    {
                        if(abletokick)
                        {
                            return kickball();
                        }
                        return gkgetball();
                    }
                    return idleinformation();
                }

                CtrlMsg AIPlayer::gkgetball()
                {
                    addutil::Vector3 ballpos = mClubAI->getBallPos();
                    addutil::Vector3 ballvel = mMatchStatus->getBall()->getVelocity();
                    float balldist = (ownpos - ballpos).length();

                    ballpos.y = 0.0f;
                    ballvel.y = 0.0f;
                    ownpos.y = 0.0f;

                    addutil::Vector3 gotovec = steering::pursuit(ownpos, ballpos, ballvel, max_velocity);

                    if(balldist < 1.5f)      // TODO: get length from somewhere else
                    {
                        using namespace messages;
                        return boost::shared_ptr<HoldPlayerControlMessage>(new HoldPlayerControlMessage(mPlayerID, gotovec));
                    }
                    return fetchball();
                }

                optimal_kick AIPlayer::getOptimalPass() const
                {
                    float plength = mMatchStatus->getPitchLength();
                    const float max_pass_length = 25.0f;
                    float opt_val = 0.0f;
                    const float defensive_area_threshold = 0.25f;
                    const float defensive_area_coefficient = 0.15f;
                    const float offensive_area_threshold = 0.75f;
                    const float offensive_area_coefficient = 0.15f;

                    float best_z    = 0.0f;
                    float own_z     = (mMatchStatus->getPlayerTarget(mPlayerID) == UpTarget) ? plength - ownpos.z : ownpos.z;
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

                        if(mMatchStatus->getPlayerTarget(mPlayerID) == UpTarget) 
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
                    soccer::BallOwner our_club = mMatchStatus->getPlayerSide(mPlayerID);
                    float best_z_rel = mMatchStatus->absolute_pitch_position_to_percent(addutil::Vector3(0.0f, 0.0f, best_z), our_club).z;
                    if(best_z_rel < defensive_area_threshold) 
                    {
                        opt_val *= defensive_area_coefficient;
                        std::cerr << "pass target in defensive area\n";
                    }
                    if(best_z_rel > offensive_area_threshold) 
                    {
                        opt_val *= offensive_area_coefficient;
                    }

                    Helpers::correctPassVector(target);
                    // std::cout << "Pass: Kick target: " << (target + ownpos) << std::endl;

                    return optimal_kick(opt_val, target);
                }

                optimal_kick AIPlayer::getOptimalShot() const
                {
                    addutil::Vector3 target(goalvec);
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

                optimal_kick AIPlayer::getOptimalDribble() const
                {
                    addutil::Vector3 target(goalvec);
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
                    addutil::Vector3 ownpos_rel = mMatchStatus->absolute_pitch_position_to_percent(ownpos, our_club);

                    if(ownpos_rel.z > offensive_area_threshold)
                        opt_val *= offensive_area_multiplier;
                    else if(ownpos_rel.z < defensive_area_threshold)
                        opt_val *= defensive_area_multiplier;

                    return optimal_kick(opt_val, target);
                }

                optimal_kick AIPlayer::getOptimalLongBall() const
                {
                    float plength = mMatchStatus->getPitchLength();
                    float opt_val = 0.0f;

                    float best_z    = 0.0f;
                    float own_z     = (mMatchStatus->getPlayerTarget(mPlayerID) == UpTarget) ? plength - ownpos.z : ownpos.z;
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
                        if(mClubAI->getTarget() == UpTarget) this_z = plength - this_z;

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

                void AIPlayer::maybePrepareForKick(addutil::Vector3& kickvec) const
                {
                    // maybe prepare for kick: potentially stop the ball first for easier kick
                    if(kickvec.length2() < 8.0f) 
                        return;
                    const addutil::Vector3& ballvec = mMatchStatus->getBall()->getVelocity();
                    if(ballvec.length2() < 8.0f)
                        return;
                    kickvec.reset();
                    return;
                    /*
                    float ang = kickvec.angleBetweenXZ(ballvec);
                    if (abs(ang) < addutil::pi_4 || abs(ang) > addutil::pi_3_4)
                    {
                        kickvec.reset();
                    }
                    */
                }

                CtrlMsg AIPlayer::kickball()
                {
                    using namespace messages;
                    if(holdingtheball)
                    {
                        std::cerr << "AIPlayer: held the ball, now letting go.\n";
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, getOptimalLongBall().get<1>()));
                    }

                    optimal_kick optimalpass = getOptimalPass();
                    optimal_kick optimalshot = getOptimalShot();
                    optimal_kick optimaldribble = getOptimalDribble();
                    optimal_kick optimallong = getOptimalLongBall();
                    if(AIConfig::getInstance()->verbose >= 1)
                    {
                        std::cerr << "Pass value: " << optimalpass.get<0>() << std::endl;
                        std::cerr << "Shot value: " << optimalshot.get<0>() << std::endl;
                        std::cerr << "Dribble value: " << optimaldribble.get<0>() << std::endl;
                        std::cerr << "Long ball value: " << optimallong.get<0>() << std::endl;
                        std::cerr << std::endl;
                    }

                    if(optimalpass.get<0>() >= optimalshot.get<0>() &&
                       optimalpass.get<0>() >= optimaldribble.get<0>() &&
                       optimalpass.get<0>() >= optimallong.get<0>())
                    {
                        maybePrepareForKick(optimalpass.get<1>());
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimalpass.get<1>()));
                    }
                    else if (optimalshot.get<0>() >= optimaldribble.get<0>() &&
                             optimalshot.get<0>() >= optimallong.get<0>())
                    {
                        maybePrepareForKick(optimalpass.get<1>());
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimalshot.get<1>()));
                    }
                    else if (optimaldribble.get<0>() >= optimallong.get<0>())
                    {
                        maybePrepareForKick(optimalpass.get<1>());
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimaldribble.get<1>()));
                    }
                    else
                    {
                        maybePrepareForKick(optimalpass.get<1>());
                        return boost::shared_ptr<KickPlayerControlMessage>(new KickPlayerControlMessage(mPlayerID, optimallong.get<1>()));
                    }
                }

                CtrlMsg AIPlayer::fetchball()
                {
                    addutil::Vector3 ballpos = mClubAI->getBallPos();
                    addutil::Vector3 ballvel = mMatchStatus->getBall()->getVelocity();

                    ballpos.y = 0.0f;
                    ballvel.y = 0.0f;
                    ownpos.y = 0.0f;
                    if(disttoball < 2.0f)
                        return seekTo(ballpos, dangerlevel * max_velocity);

                    addutil::Vector3 gotovec = steering::pursuit(ownpos, ballpos, ballvel * 2.0f, max_velocity);
                    return seekTo(gotovec + ownpos, dangerlevel * max_velocity);

                    // using namespace messages;
                    // return boost::shared_ptr<MovePlayerControlMessage>(new MovePlayerControlMessage(mPlayerID, gotovec));
                }

                CtrlMsg AIPlayer::seekTo(const addutil::Vector3& tgt, float vel) const
                {
                    using namespace addutil::steering;
                    return boost::shared_ptr<MovePlayerControlMessage>(
                            new MovePlayerControlMessage(mPlayerID, seek(ownpos, tgt, std::max(min_walk_vel, vel))));
                }

                CtrlMsg AIPlayer::runTo(const addutil::Vector3& tgt, float vel) const
                {
                    using namespace addutil::steering;
                    return boost::shared_ptr<MovePlayerControlMessage>(
                            new MovePlayerControlMessage(mPlayerID, arrive(ownpos, tgt, std::max(min_walk_vel, vel))));
                }

                void AIPlayer::set_own_kickoff_pos()
                {
                    if(!issub)
                    {
                        addutil::Vector3 formationpoint;
                        try
                        {
                            formationpoint = mMatchStatus->getPlayerFormation(mPlayerID)->getPitchPoint(plpos);
                            ownkickoffpos.x = formationpoint.x;
                            ownkickoffpos.z = formationpoint.z * 0.5f;
                        }
                        catch(addutil::Exception& e)
                        {
                            std::cerr << "Could not find position: " << plpos << std::endl;
                            ownkickoffpos.x = 0.5f;
                        }
                    }
                    else
                    {
                        ownkickoffpos = cabins_pos();
                        return;
                    }
                    if((mClubAI->ourClub() == Away && !mMatchStatus->isSecondHalf()) || (mClubAI->ourClub() == Home && mMatchStatus->isSecondHalf()))
                    {
                        if(!issub)
                        {
                            ownkickoffpos.x = 1.0f - ownkickoffpos.x;
                            ownkickoffpos.z = 1.0f - ownkickoffpos.z;
                        }
                    }
                    float pitchw = mMatchStatus->getPitchWidth();
                    float pitchl = mMatchStatus->getPitchLength();
                    ownkickoffpos.x *= pitchw;
                    ownkickoffpos.z *= pitchl;
                }

                int AIPlayer::getID() const
                {
                    return mPlayerID;
                }
            }
        }
    }
}
