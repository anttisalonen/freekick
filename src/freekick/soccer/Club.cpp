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
  This file was generated on So Okt 26 2008 at 12:09:20
**************************************************************************/

#include "Club.h"

namespace freekick
{
    namespace soccer
    {
/**
 * @param  name
 */
        Club::Club (const std::string& _name ) 
            : name(_name),
              lineup(new Lineup()),
              mFormation(new Formation())
        {
        }

        void Club::from_xml(xmlNodePtr root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            for (node = root->next; node; node = node->next)
            {
                if(node_is_node(node, "coach"))
                {
                    // TODO
                }
                else if(node_is_node(node, "kits"))
                {
                    kits.clear();
                    xmlNodePtr chnode = NULL;
                    for (chnode = node->children; chnode; chnode = chnode->next)
                    {
                        if (chnode->type == XML_ELEMENT_NODE)
                        {
                            if(node_is_node(chnode, "kit"))
                            {
                                kits.push_back(Kit(chnode->children));
                            }
                        }
                    }
                }
                else if(node_is_node(node, "country"))
                {
                    // TODO
                }
                else if(node_is_node(node, "stadium"))
                {
                    // TODO
                }
                else if(node_is_node(node, "contracts"))
                {
                    contracts.clear();
                    xmlNodePtr chnode = NULL;
                    for (chnode = node->children; chnode; chnode = chnode->next)
                    {
                        if (chnode->type == XML_ELEMENT_NODE)
                        {
                            if(node_is_node(chnode, "contract"))
                            {
                                int plnum;
                                get_attribute(chnode, "player", plnum);
                                contracts.insert(plnum);
                            }
                        }
                    }
                }
            }
        }

        const std::string& Club::getName() const
        {
            return name;
        }

        int Club::getNumberOfPlayers() const
        {
            return players.size();
        }

        void Club::addPlayer(boost::shared_ptr<Player>& p, PlayerInLineup st)
        {
            int id = p->getID();
            players[id] = p;
            if(st == Playing)
            {
                lineup->addPlayer(id, "", false);
            }
            else if(st == Substitute)
            {
                lineup->addPlayer(id, "", true);
            }
        }

        bool Club::hasPlayer(int i) const
        {
            return (players.find(i) != players.end());
        }

        const Player& Club::getPlayer(int i) const
        {
            std::map <int, boost::shared_ptr<Player> >::const_iterator it = players.find(i);
            if(it == players.end())
                throw "Club::getPlayer: No player found";
            return *(it->second);
        }

        void Club::getPlayers(std::vector<boost::shared_ptr<Player> >& pls) const
        {
            pls.clear();
            typedef std::pair<int, boost::shared_ptr<Player> > pair_pl;

            BOOST_FOREACH(pair_pl p, players)
            {
                pls.push_back(p.second);
            }
        }

        void Club::getPlayers(std::set<boost::shared_ptr<Player> >& pls) const
        {
            pls.clear();
            typedef std::pair<int, boost::shared_ptr<Player> > pair_pl;

            BOOST_FOREACH(pair_pl p, players)
            {
                pls.insert(p.second);
            }
        }

        void Club::getPlayerIDs(std::vector<int>& ids) const
        {
            ids.clear();
            typedef std::pair<int, boost::shared_ptr<Player> > pair_pl;

            BOOST_FOREACH(pair_pl p, players)
            {
                ids.push_back(p.first);
            }
        }

        void Club::getPlayerIDs(std::set<int>& ids) const
        {
            ids.clear();
            typedef std::pair<int, boost::shared_ptr<Player> > pair_pl;

            BOOST_FOREACH(pair_pl p, players)
            {
                ids.insert(p.first);
            }
        }

        const boost::shared_ptr<Lineup>& Club::getLineup() const
        {
            return lineup;
        }

        const boost::shared_ptr<Formation>& Club::getFormation() const
        {
            return mFormation;
        }

        void Club::setFormation(const boost::shared_ptr<Formation>& f)
        {
            mFormation = f;
        }

        void Club::setLineup(const boost::shared_ptr<Lineup>& l)
        {
            lineup = l;
        }

        const std::string& Club::getPlayerPosition(int i) const
        {
            return lineup->getPlayerPosition(i);
        }
    }
}
