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

#include "MatchData.h"

namespace freekick
{
    namespace soccer
    {
        MatchData::MatchData (boost::shared_ptr<Club> cl1, 
                              boost::shared_ptr<Club> cl2,
                              boost::shared_ptr<Stadium> s)
            : homeclub(cl1),
              awayclub(cl2),
              stadium(s),
              ball(new Ball(0.4f)),     // don't forget the ball
              m_initial_data(NULL)
        {
            LIBXML_TEST_VERSION;
            // TODO: create m_initial_data XML doc
            std::cerr << "created MatchData from classes\n";
        }

        MatchData::MatchData (const std::string& xmldata)
        {
            LIBXML_TEST_VERSION;
            std::cerr << "creating MatchData from string\n";
            xmlDocPtr doc = xmlReadMemory(xmldata.c_str(), xmldata.length(), "", "UTF-8", 0);
            if(doc == NULL)
            {
                std::stringstream ss;
                ss << __func__ << ": could not parse XML string";
                std::cerr << ss.str() << std::endl;
                throw addutil::Exception(ss.str());
            }
            create_from_doc(doc);
        }

        MatchData::MatchData (const char* filename)
            : m_initial_data(NULL)
        {
            LIBXML_TEST_VERSION;
            std::cerr << "creating MatchData from file\n";
            xmlDocPtr doc = xmlReadFile(filename, NULL, 0);
            if(doc == NULL)
            {
                std::stringstream ss;
                ss << __func__ << ": could not parse XML file";
                std::cerr << ss.str() << std::endl;
                throw addutil::Exception(ss.str());
            }
            create_from_doc(doc);
        }

        MatchData::~MatchData()
        {
            if(m_initial_data)
                xmlFreeDoc(m_initial_data);
            xmlCleanupParser();
        }

        void MatchData::create_from_doc (xmlDocPtr doc)
        {
            xmlNode *root_element = xmlDocGetRootElement(doc);
            if(!addutil::xml::node_has_name(root_element, "Match"))
            {
                xmlFreeDoc(doc);
                xmlCleanupParser();
                std::stringstream ss;
                ss << __func__ << ": expected: Match; found: " << root_element->name;
                throw addutil::Exception(ss.str());
            }

            try
            {
                std::cerr << __func__ << ": parsing match\n";
                parse_match(root_element->children);
            } 
            catch(addutil::Exception& e)
            {
                xmlFreeDoc(doc);
                xmlCleanupParser();
                throw e;
            }

            std::cerr << __func__ << ": copying\n";
            m_initial_data = xmlCopyDoc(doc, 1);

            // success
            xmlFreeDoc(doc);
        }

        void MatchData::parse_match(xmlNode* root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            for (node = root; node; node = node->next)
            {
                if(node_is_node(node, "Clubs"))
                {
                    std::cerr << __func__ << ": parsing clubs\n";
                    parse_clubs(node->children);
                }
                else if (node_is_node(node, "HomeClubPlayers"))
                {
                    std::cerr << __func__ << ": parsing home club players\n";
                    parse_players(node->children, true);
                }
                else if (node_is_node(node, "AwayClubPlayers"))
                {
                    std::cerr << __func__ << ": parsing away club players\n";
                    parse_players(node->children, false);
                }
                else if (node_is_node(node, "OtherKits"))
                {
                    std::cerr << __func__ << ": parsing other kits\n";
                    parse_other_kits(node->children);
                }
                else if(node_is_node(node, "Tournament"))
                {
                    std::cerr << __func__ << ": parsing tournament\n";
                    // TODO
                }
                else if(node_is_node(node, "stadium"))
                {
                    std::cerr << __func__ << ": parsing stadium\n";
                    // TODO
                }
                else if(node_is_node(node, "pitch"))
                {
                    std::cerr << __func__ << ": parsing pitch\n";
                    float w, l;
                    xmlNodePtr chnode = NULL;
                    for(chnode = node->children; chnode; chnode = chnode->next)
                    {
                        if(node_is_node(chnode, "area"))
                        {
                            get_attribute(chnode, "width", w);
                            get_attribute(chnode, "length", l);
                        }
                    }
                    stadium.reset(new Stadium(w, l));
                    // TODO: rest
                }
                else if(node_is_node(node, "start"))
                {
                    // TODO
                }
                else if(node_is_node(node, "weather"))
                {
                    // TODO
                }
                else if(node_is_node(node, "ball"))
                {
                    // TODO
                    ball.reset(new Ball(0.4f));
                }
                else if(node_is_node(node, "formation"))
                {
                    std::cerr << __func__ << ": parsing formation\n";
                    parse_formation(node);
                }
                else if(node_is_node(node, "lineup"))
                {
                    std::cerr << __func__ << ": parsing lineup\n";
                    parse_lineup(node);
                }
                else if(!node_has_name(node, "text"))
                {
                    std::cerr << __func__ << ": unknown node met: " << node->name << std::endl;
                }
            }
        }

        void MatchData::parse_clubs(xmlNode* root)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            for (node = root->next; node; node = node->next)
            {
                if(node_is_node(node, "club"))
                {
                    std::string clubname, ishome;
                    try
                    {
                        get_attribute(node, "name", clubname);
                        get_attribute(node, "home", ishome);
                    }
                    catch(addutil::Exception& e)
                    {
                        std::cerr << __func__ << ": " << e.what() << std::endl;
                        throw e;
                    }

                    if(ishome != "0")
                    {
                        homeclub.reset(new Club(clubname));
                        std::cout << "Home club name: " << homeclub->getName() << std::endl;
                        homeclub->from_xml(node->children);
                    }
                    else
                    {
                        awayclub.reset(new Club(clubname));
                        std::cout << "Away club name: " << awayclub->getName() << std::endl;
                        awayclub->from_xml(node->children);
                    }
                }                
            }
        }

        void MatchData::parse_players(xmlNode* root, bool home)
        {
            using namespace addutil::xml;
            xmlNode *node = NULL;
            for (node = root->next; node; node = node->next)
            {
                if(node_is_node(node, "player"))
                {
                    boost::shared_ptr<Player> pl;
                    int plid;
                    std::string plname;
                    get_attribute(node, "id", plid);
                    xmlNodePtr chnode = NULL;
                    for(chnode = node->children; chnode; chnode = chnode->next)
                    {
                        if(node_is_node(chnode, "personal"))
                        {
                            get_attribute(chnode, "name", plname);
                            pl.reset(new Player(plname, 0, plid, Goalkeeper));
                            // TODO: appearance, nationality
                        }
                    }
                    if(pl.get() == 0)
                        throw addutil::Exception("Error while parsing player: player not created");
                    for(chnode = node->children; chnode; chnode = chnode->next)
                    {
                        if(node_is_node(chnode, "personality"))
                        {
                            get_attribute(chnode, "active", pl->playerpersonality.active);
                            get_attribute(chnode, "risktaking", pl->playerpersonality.risktaking);
                            get_attribute(chnode, "offensive", pl->playerpersonality.offensive);
                            get_attribute(chnode, "aggressive", pl->playerpersonality.aggressive);
                            get_attribute(chnode, "consistent", pl->playerpersonality.consistent);
                            get_attribute(chnode, "creative", pl->playerpersonality.creative);
                            get_attribute(chnode, "experienced", pl->playerpersonality.experienced);
                        }
                        else if(node_is_node(chnode, "skills"))
                        {
                            get_attribute(chnode, "stamina", pl->playerskills.stamina);
                            get_attribute(chnode, "dexterity", pl->playerskills.dexterity);
                            get_attribute(chnode, "speed", pl->playerskills.speed);
                            get_attribute(chnode, "tackling", pl->playerskills.tackling);
                            get_attribute(chnode, "passing", pl->playerskills.passing);
                            get_attribute(chnode, "shooting", pl->playerskills.shooting);
                            get_attribute(chnode, "control", pl->playerskills.control);
                            get_attribute(chnode, "accuracy", pl->playerskills.accuracy);
                            get_attribute(chnode, "goalkeeping", pl->playerskills.goalkeeping);
                            get_attribute(chnode, "heading", pl->playerskills.heading);
                        }
                        else if(node_is_node(chnode, "position"))
                        {
                            pl->position = pp_from_xml(chnode);
                        }
                        else if(node_is_node(chnode, "status"))
                        {
                            // TODO
                        }
                    }
                    if(home)
                    {
                        homeclub->addPlayer(pl, NotPlaying);
                    }
                    else
                    {
                        awayclub->addPlayer(pl, NotPlaying);
                    }
                }
            }
        }

        void MatchData::parse_other_kits(xmlNode* root)
        {
            // TODO
        }

        void MatchData::parse_formation(xmlNode* root)
        {
            boost::shared_ptr<Formation> f(new Formation(root));
            std::string owner;
            addutil::xml::get_attribute(root, "owner", owner);
            if(owner == homeclub->getName())
            {
                homeclub->setFormation(f);
            }
            else
            {
                awayclub->setFormation(f);
            }
        }

        void MatchData::parse_lineup(xmlNode* root)
        {
            boost::shared_ptr<Lineup> l(new Lineup(root));
            std::string owner;
            addutil::xml::get_attribute(root, "owner", owner);
            if(owner == homeclub->getName())
            {
                homeclub->setLineup(l);
            }
            else
            {
                awayclub->setLineup(l);
            }
        }

        boost::shared_ptr<Club> MatchData::getClub(BallOwner b) const
        {
            if(b == Home)
                return homeclub;
            return awayclub;
        }

        boost::shared_ptr<Club> MatchData::getHomeClub() const
        {
            return homeclub;
        }

        boost::shared_ptr<Club> MatchData::getAwayClub() const
        {
            return awayclub;
        }

        void MatchData::getHomeClubName(std::string& s) const
        {
            s = homeclub->getName();
        }

        void MatchData::getAwayClubName(std::string& s) const
        {
            s = awayclub->getName();
        }

        template <typename ContT>
        void MatchData::getHomePlayerIDs(ContT& ids) const
        {
            homeclub->getPlayerIDs(ids);
        }

        template <typename ContT>
        void MatchData::getAwayPlayerIDs(ContT& ids) const
        {
            awayclub->getPlayerIDs(ids);
        }

        boost::shared_ptr<Ball> MatchData::getBall() const
        {
            return ball;
        }

        void MatchData::setStadium(boost::shared_ptr<Stadium>& stad)
        {
            stadium = stad;
        }

        boost::shared_ptr<Stadium> MatchData::getStadium() const
        {
            return stadium;
        }

        xmlDocPtr MatchData::getInitialDataXML() const
        {
            return m_initial_data;
        }
    }
}
