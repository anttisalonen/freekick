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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/


#ifndef STATUS_H
#define STATUS_H

#include <vector>
#include <set>
#include <string>
#include <iostream>
#include <sstream>
#include <exception>

#include <boost/foreach.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/thread.hpp>

#include "Stadium.h"
#include "Ball.h"
#include "Referee.h"
#include "MatchStatus.h"
#include "Club.h"
#include "Drawable.h"

/**
  * class Status
  */

class Status
{
public:

    // Constructors/Destructors
    //  

    /**
     */
     Status ( );
     virtual ~Status();

    // Public member functions

    /**
     * @param  evt
     */
     void newEvent (const std::string& evt );

    /**
     * @param  events
     */
     void newEvents (std::vector <std::string>& events );

     std::set <boost::shared_ptr<Drawable> >* getDrawables ( );

     void addClub(const std::string& name);
     void addPlayer(const std::string& clubname, int idnum, const Color& col);
     void addBall();
     void updateAll(float interval);
     void interpolateAll(boost::posix_time::ptime pt);

     bool run();
//     const std::string& getHomeClubName() const;
     template <typename ContT> void getHomePlayerIDs(ContT& ids);
     template <typename ContT> void getAwayPlayerIDs(ContT& ids);

private:

    // Private attributes
    //  

    Stadium* stadium;
    boost::shared_ptr<Ball> ball;
    std::map <std::string, boost::shared_ptr<Club> > clubs;
    Referee* referee;
    MatchStatus* matchstatus;
    std::set <boost::shared_ptr<Drawable> > drawables;
    boost::shared_ptr<Club> homeclub;
    boost::shared_ptr<Club> awayclub;
    boost::posix_time::ptime update_time;

    template <typename ContT> void getClubPlayerIDs(const boost::shared_ptr<Club> c, ContT& ids);

};

#endif // STATUS_H
