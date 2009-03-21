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

#include <cstdlib>

#include <iostream>
#include <exception>
#include <fstream>
#include <csignal>

#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include <boost/archive/text_oarchive.hpp>
#include <boost/exception.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>

#include "addutil/Exception.h"

#include "MatchStatus.h"
#include "Network.h"

// https://svn.boost.org/trac/boost/ticket/2156: first boost/asio.hpp (Network.h), then curses.h
#include <curses.h>



#include "messages/InitialDataRequest.h"

using namespace freekick::match;
using namespace freekick::match::client;

// TODO: make this less ugly
void run_network(Network* n)
{
    n->run();
}

namespace freekick { namespace match { namespace client { namespace curses {
static void cleanup(int sig)
{
    nl();
    endwin();
    exit(0);
}

using namespace std;

class Curses_Engine
{
public:
    Curses_Engine(boost::shared_ptr<MatchStatus> ms, Network* n)
        : m_matchstatus(ms),
          m_network(n)
    {
        initscr();
        use_default_colors();
        keypad(stdscr, TRUE);
        nonl();
        cbreak();
        noecho();
        start_color();

        init_pair(1, COLOR_GREEN,   COLOR_BLACK);
        init_pair(2, COLOR_CYAN,    COLOR_BLACK);
        init_pair(3, COLOR_WHITE,   COLOR_BLACK);

        getmaxyx (stdscr, m_max_y, m_max_x);

        set_fps(10.0f);
        m_max_pitch_x = m_max_x - 1;
        m_max_pitch_y = m_max_y - 3 - 1;
        m_pitch_width = m_matchstatus->getPitchWidth();
        m_pitch_length = m_matchstatus->getPitchLength();
        if (m_pitch_width == 0.0f) throw std::runtime_error("pitch width = 0.0f");
        if (m_pitch_length == 0.0f) throw std::runtime_error("pitch length = 0.0f");

        m_playermap_home = m_matchstatus->getPlayers(Home);
        m_playermap_away = m_matchstatus->getPlayers(Away);
    }

    void set_fps(float f)
    {
        if(f <= 0.0f) return;
        m_fps = f;
        m_frametime = 1.0f / m_fps;
        m_sleep_time = m_frametime * 1000000;
    }

    bool render()
    {
        clear();
        attrset(COLOR_PAIR(1));
        BOOST_FOREACH(boost::shared_ptr<MatchPlayer> p, m_playermap_home)
        {
            int y = pitch_x_coord_to_screen(p->getPosition().x);
            int x = pitch_z_coord_to_screen(p->getPosition().z);
            mvaddstr(y, x, p->getNumberString().c_str());
        }
        attrset(COLOR_PAIR(2));
        BOOST_FOREACH(boost::shared_ptr<MatchPlayer> p, m_playermap_away)
        {
            int y = pitch_x_coord_to_screen(p->getPosition().x);
            int x = pitch_z_coord_to_screen(p->getPosition().z);
            mvaddstr(y, x, p->getNumberString().c_str());
        }

        attrset(COLOR_PAIR(3));
        {
            int y = pitch_x_coord_to_screen(m_matchstatus->getBall()->getPosition().x);
            int x = pitch_z_coord_to_screen(m_matchstatus->getBall()->getPosition().z);
            mvaddch(y, x, 'o');
        }

        attrset(COLOR_PAIR(0));
        mvaddch(0, 0, '1');
        mvaddch(0, m_max_pitch_x, '2');
        mvaddch(m_max_pitch_y, 0, '3');
        mvaddch(m_max_pitch_y, m_max_pitch_x, '4');
        refresh();
        return true;
    }

    void render_loop()
    {
        using namespace boost::posix_time;
        bool cont = true;
        float time_left;
        while (cont)
        {
            ptime before_time(microsec_clock::local_time());
            cont = render();
            ptime after_time(microsec_clock::local_time());

            time_period diff_time(before_time, after_time);
            time_duration diff_dur = diff_time.length();
            long us_diff = diff_dur.total_microseconds();
            time_left = m_sleep_time - us_diff;
            if(time_left > 0)
                boost::this_thread::sleep(microseconds(time_left));
            else
            {
                std::cerr << "render_loop: overrun by " << -time_left << " microseconds\n";
            }
        }
    }

    void run()
    {
        render_loop();
    }

    int pitch_x_coord_to_screen(float x)
    {
        return /*m_max_pitch_y -*/ ((x / m_pitch_width) * m_max_pitch_y);
    }

    int pitch_z_coord_to_screen(float z)
    {
        return (z / m_pitch_length) * m_max_pitch_x;
    }

private:
    boost::shared_ptr<MatchStatus> m_matchstatus;
    Network* m_network;

    std::vector <boost::shared_ptr<MatchPlayer> > m_playermap_home;
    std::vector <boost::shared_ptr<MatchPlayer> > m_playermap_away;

    int m_max_y;
    int m_max_x;
    float m_fps;
    float m_frametime;
    float m_sleep_time;

    int m_max_pitch_y;
    int m_max_pitch_x;
    float m_pitch_width;
    float m_pitch_length;
};

void run_game(boost::shared_ptr<MatchStatus> ms, Network* n)
{
    using namespace std;
    
    signal(SIGINT, cleanup);
    Curses_Engine ce(ms, n);
    ce.run();
    cleanup(0);
}
            } } } }

using namespace freekick::match::client::curses;

int main(int argc, char** argv)
{
    try
    {
        addutil::network::IP_Connection conn;
        conn.ip_address = "127.0.0.1";
        conn.port = "32105";
        Network* network;
        boost::shared_ptr<MatchStatus> status;
        std::cerr << "Freekick Curses client starting" << std::endl;
        try
        {
            network = new Network(conn, false);
            boost::thread network_thread(boost::bind(&run_network, network));
            boost::this_thread::sleep(boost::posix_time::milliseconds(1000));  // TODO: make timeouts configurable?
            if(!network->is_connected())
            {
                std::cerr << "Network::Network: timeout while connecting";
                return 1;
            }
            network->sendMessage(freekick::match::messages::InitialDataRequest());
            boost::this_thread::sleep(boost::posix_time::milliseconds(1000));
            status = network->getMatchStatus();
            if(status.get() == 0)
            {
                std::string err("Network::Network: no match status created.\n");
                std::cerr << err;
                return 1;
            }
            else std::cerr << "Network::Network: Success.\n";
        }
        catch(...)
        {
            std::cerr << "Network connection failed; exiting.\n";
            return 1;
        }
        if(status.get() == 0) { std::cerr << "Received invalid match status?\n"; return 1; }

        run_game(status, network);
        std::cout << "Shutting down client.\n";
        cleanup(0);

        delete network;
    }
    catch (boost::exception& e)
    {
	addutil::output_boost_exception(e);
    }
    catch (std::exception& e)
    {
        std::cerr << "A std::exception has occurred: " << e.what() << std::endl;
    }
    catch (...)
    {
        std::cerr << "Unknown exception has occurred." << std::endl;
    }
    cleanup(0);

    return 0;
}
