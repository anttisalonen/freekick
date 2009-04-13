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


#ifndef FREEKICKTASKSKICKBALL_H
#define FREEKICKTASKSKICKBALL_H

#include <boost/shared_ptr.hpp>
#include <boost/tuple/tuple.hpp>

#include "addutil/Vector3.h"

#include "MatchStatus.h"
#include "Helpers.h"
#include "Primitives.h"

#include "AIConfig.h"
#include "tasks/CompositeTask.h"
#include "messages/KickPlayerControlMessage.h"

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
                    typedef boost::tuple<float, addutil::Vector3> optimal_kick;
                    class KickBall : public CompositeTask
                    {
                    public:
                        KickBall(const boost::shared_ptr<MatchStatus>& ms, int id);
                        bool finished() const;
                        boost::shared_ptr<messages::PlayerControlMessage> process();
                    private:
                        optimal_kick getOptimalPass() const;
                        optimal_kick getOptimalShot() const;
                        optimal_kick getOptimalDribble() const;
                        optimal_kick getOptimalLongBall() const;
                        void maybePrepareForKick(addutil::Vector3& kickvec) const;
                        boost::shared_ptr<MatchStatus> mMatchStatus;
                        int mPlayerID;
                        boost::shared_ptr<MatchPlayer> mPlayer;
                        addutil::Vector3 ownpos;
                        soccer::PlayerTarget t;
                        addutil::Vector3 tgtgoal;
                        addutil::Vector3 goalvec;
                    };
                }
            }
        }
    }
}

#endif
